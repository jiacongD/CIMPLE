#' surv_est
#'
#' @param long_data
#' @param method
#' @param surv_data
#' @param LM_fixedEffect_withTime_variables
#' @param LM_randomEffect_variables
#' @param SM_variables
#' @param SM_base_variables
#' @param imp_time_factor
#'
#' @return
#' @export
#'
#' @examples
surv_est <- function(long_data,
                     method,
                     surv_data,
                     LM_fixedEffect_withTime_variables = NULL,
                     LM_randomEffect_variables = NULL,
                     SM_variables = NULL,
                     SM_base_variables = NULL,
                     imp_time_factor = NULL) {
  if (method == "cox") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_withTime_variables` must be provided." = !is.null(LM_fixedEffect_withTime_variables))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_variables` must be provided." = !is.null(SM_variables))
    stopifnot("`SM_base_variables` must be provided." = !is.null(SM_base_variables))


    long_data2 <- long_data %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(time0 = ifelse(is.na(dplyr::lag(time)), 0, dplyr::lag(time))) %>%
      dplyr::filter(time != 0) %>%
      dplyr::filter(time0 < time)
    long_data2$d0 <- surv_data$d[match(long_data2$id, surv_data$id)]

    long_data2 <- na.omit(long_data2) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(d = ifelse(time < max(time, na.rm = TRUE), 0, d0))
    model_formula <- as.formula(paste("Surv(time0, time, d) ~ ", paste(SM_variables, collapse = "+")))
    model <- survival::coxph(model_formula, data = long_data2)
    alpha.hat <- summary(model)$coef[, 1]

    return(alpha.hat)
  } else if (method == "JMLD") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_withTime_variables` must be provided." = !is.null(LM_fixedEffect_withTime_variables))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_base_variables` must be provided." = !is.null(SM_base_variables))

    # remove patients with no Y measurement
    zeroRecords_id <- long_data[is.na(long_data$Y), "id"]
    long_data <- long_data[!long_data$id %in% zeroRecords_id, ]
    surv_data <- surv_data[!surv_data$id %in% zeroRecords_id, ]

    # longitudinal submodel, try nlminb optimizer first, if there is an error, then use optim optimizer
    lmeFit_fixedformula <- as.formula(paste("Y ~ ", paste(LM_fixedEffect_withTime_variables, collapse = "+")))
    lmeFit_randomformula <- as.formula(paste("~", paste(LM_randomEffect_variables, collapse = "+"), "|id"))
    control_optim <- nlme::lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt = "optim")
    control_nlminb <- nlme::lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt = "nlminb")
    lmeFit <- tryCatch(
      {
        lmeFit <- nlme::lme(lmeFit_fixedformula, random = lmeFit_randomformula, data = long_data, control = control_optim)
      },
      error = function(e) {
        print(paste0("Error with optim: ", e))
        lmeFit <- nlme::lme(lmeFit_fixedformula, random = lmeFit_randomformula, data = long_data, control = control_nlminb)
      }
    )
    # print(lmeFit)

    # survival submodel
    coxFit_formula <- as.formula(paste("Surv(D, d) ~ ", paste(SM_base_variables, collapse = "+")))
    coxFit <- survival::coxph(coxFit_formula, data = surv_data, x = TRUE)
    # summary(coxFit)

    # jointFit = JMbayes2::jm(coxFit,lmeFit,time_var="time", n_chains=1L,n_iter=11000L,n_burnin=1000L)
    jointFit <- JMbayes2::jm(coxFit, lmeFit, time_var = "time", n_chains = 1L)

    # print(summary(jointFit))

    surv_proc <- unlist(coef(jointFit)) # change the name of the output to alpha
    long_proc <- unlist(fixef(jointFit))

    results <- list(
      "long_proc" = long_proc,
      "surv_proc" = surv_proc
    )

    return(results)
  } else if (method == "VA_JMLD") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_withTime_variables` must be provided." = !is.null(LM_fixedEffect_withTime_variables))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_base_variables` must be provided." = !is.null(SM_base_variables))

    # remove patients with no Y measurement
    zeroRecords_id <- long_data[is.na(long_data$Y), "id"]
    long_data <- long_data[!long_data$id %in% zeroRecords_id, ]
    surv_data <- surv_data[!surv_data$id %in% zeroRecords_id, ]

    # add Ni(t) as a predictor
    long_data$Ni <- NA
    for (i in 1:nrow(long_data)) {
      id <- long_data$id[i]
      time <- long_data$time[i]
      long_data$Ni[i] <- sum(!is.na(long_data$Y[long_data$id == id & long_data$time <= time]))
    }

    # longitudinal submodel, try nlminb optimizer first, if there is an error, then use optim optimizer
    lmeFit_fixedformula <- as.formula(paste("Y ~ ", paste(LM_fixedEffect_withTime_variables, collapse = "+"), "+Ni"))
    lmeFit_randomformula <- as.formula(paste("~", paste(LM_randomEffect_variables, collapse = "+"), "|id"))
    control_optim <- nlme::lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt = "optim")
    control_nlminb <- nlme::lmeControl(msMaxIter = 1000, msMaxEval = 1000, opt = "nlminb")
    lmeFit <- tryCatch(
      {
        lmeFit <- nlme::lme(lmeFit_fixedformula, random = lmeFit_randomformula, data = long_data, control = control_optim)
      },
      error = function(e) {
        print(paste0("Error with optim: ", e))
        lmeFit <- nlme::lme(lmeFit_fixedformula, random = lmeFit_randomformula, data = long_data, control = control_nlminb)
      }
    )
    print(lmeFit)

    # survival submodel
    coxFit_formula <- as.formula(paste("Surv(D, d) ~ ", paste(SM_base_variables, collapse = "+")))
    coxFit <- survival::coxph(coxFit_formula, data = surv_data, x = TRUE)
    summary(coxFit)

    # jointFit = JMbayes2::jm(coxFit,lmeFit,time_var="time", n_chains=1L,n_iter=11000L,n_burnin=1000L)
    jointFit <- JMbayes2::jm(coxFit, lmeFit, time_var = "time", n_chains = 1L)

    print(summary(jointFit))

    surv_proc <- unlist(coef(jointFit))
    long_proc <- unlist(nlme::fixef(jointFit))

    results <- list(
      "long_proc" = long_proc,
      "surv_proc" = surv_proc
    )

    return(results)
  } else if (method == "Imputation_Cox") {
    # Check Inputs
    stopifnot("`LM_fixedEffect_withTime_variables` must be provided." = !is.null(LM_fixedEffect_withTime_variables))
    stopifnot("`LM_randomEffect_variables` must be provided." = !is.null(LM_randomEffect_variables))
    stopifnot("`SM_base_variables` must be provided." = !is.null(SM_base_variables))

    data <- long_data
    if (is.null(imp_time_factor)) {
      imp_time_factor <- 1
      data3 <- data
    } else {
      # If the imp_time_factor is specified and is not 1
      # imp_time_factor = 0.5
      # group the data based on the time factor
      data <- data %>%
        dplyr::mutate(time_new = ceiling(time / imp_time_factor)) %>%
        dplyr::group_by(id, time_new) %>%
        dplyr::mutate(Y_mean = mean(Y, na.rm = TRUE)) %>%
        dplyr::select(id, Y_mean, time_new, dplyr::setdiff(names(data), c("id", "Y", "time"))) %>%
        dplyr::ungroup()
      colnames(data)[2:3] <- c("Y", "time")

      # create a new dataset with the same columns in data1. For a given id, the time is from 1 to max_time. If there is no value of Y at a given time, then Y is NA.
      max_time <- max(data$time, na.rm = TRUE)
      id_all <- rep(unique(data$id), each = max_time)
      time_all <- rep(as.numeric(1:max_time), length(unique(data$id)))
      data_base <- data %>%
        dplyr::select(-Y, -time) %>%
        unique()
      data2 <- data_base %>%
        dplyr::slice(rep(1:n(), each = max_time)) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time = rep(1:max_time))
      # delete time after the disease time
      data2$D <- surv_data$D[match(data2$id, surv_data$id)]
      data2 <- data2[data2$time <= ceiling(data2$D / imp_time_factor), dplyr::setdiff(names(data2), c("Y"))]

      # join the two datasets
      data3 <- dplyr::left_join(data2, data, by = dplyr::setdiff(names(data), c("Y")))
      data3$time <- data3$time * imp_time_factor # convert time back to the original scale
    }

    print(paste0("imp factor: ", imp_time_factor))

    df_full <- data3
    dim(df_full)

    #### Start imputation ####
    # empty imputation
    imp0 <- mice::mice(as.matrix(df_full), maxit = 0)
    predM <- imp0$predictorMatrix
    impM <- imp0$method

    # specify predictor matrix and method
    predM1 <- predM
    predM1["Y", "id"] <- -2
    predM1["Y", LM_fixedEffect_withTime_variables] <- 1 # fixed x effects imputation
    impM1 <- impM
    impM1["Y"] <- "2l.lmer"

    # multilevel imputation
    imp1 <- mice::mice(as.matrix(df_full),
                       m = 5,
                       predictorMatrix = predM1, method = impM1, maxit = 5
    )

    # fit the cox-ph model
    coxph_imp <- function(data_imp) {
      # cox-ph model
      long_data_imp2 <- data_imp %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time0 = ifelse(is.na(dplyr::lag(time)), 0, dplyr::lag(time)))
      long_data_imp2$d0 <- surv_data$d[match(long_data_imp2$id, surv_data$id)]
      long_data_imp2 <- na.omit(long_data_imp2) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(d = ifelse(time < max(time, na.rm = TRUE), 0, d0)) %>%
        dplyr::filter(time0 < time)
      model_formula <- as.formula(paste("Surv(time0, time, d) ~ ", paste(SM_variables, collapse = "+")))
      model <- survival::coxph(model_formula, data = long_data_imp2)
      (alpha.hat <- summary(model)$coef[, 1])
    }
    fit <- lapply(1:5, function(i) coxph_imp(mice::complete(imp1, action = i)))
    alpha.hat <- sapply(seq_along(fit[[1]]), function(i) mean(sapply(fit, `[`, i)))
    names(alpha.hat) <- names(fit[[1]])

    return(alpha.hat)
  } else if (method == "VAImputation_Cox") {
    # Check Inputs
    stopifnot("`SM_variables` must be provided." = !is.null(SM_variables))

    data <- long_data

    if (is.null(imp_time_factor)) {
      imp_time_factor <- 1
      data3 <- data
    } else {
      # If the imp_time_factor is specified and is not 1
      # group the data based on the time factor
      data <- data %>%
        dplyr::mutate(time_new = ceiling(time / imp_time_factor)) %>%
        dplyr::group_by(id, time_new) %>%
        dplyr::mutate(Y_mean = mean(Y, na.rm = TRUE)) %>%
        # mutate(Y_mean = ifelse(is.numeric(Y_mean),Y_mean,NA)) %>%
        dplyr::select(id, Y_mean, time_new, dplyr::setdiff(names(data), c("id", "Y", "time"))) %>%
        dplyr::ungroup()
      colnames(data)[2:3] <- c("Y", "time")

      # create a new dataset with the same columns in data1. For a given id, the time is from 1 to max_time. If there is no value of Y at a given time, then Y is NA.
      max_time <- max(data$time, na.rm = TRUE)
      id_all <- rep(unique(data$id), each = max_time)
      time_all <- rep(as.numeric(1:max_time), length(unique(data$id)))
      data_base <- data %>%
        dplyr::select(-Y, -time) %>%
        unique()
      data2 <- data_base %>%
        dplyr::slice(rep(1:n(), each = max_time)) %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time = rep(1:max_time))
      # delete time after the disease time
      data2$D <- surv_data$D[match(data2$id, surv_data$id)]
      data2 <- data2[data2$time <= ceiling(data2$D / imp_time_factor), dplyr::setdiff(names(data2), c("Y"))]

      # join the two datasets
      data3 <- dplyr::left_join(data2, data, by = dplyr::setdiff(names(data), c("Y")))
      data3$time <- data3$time * imp_time_factor # convert time back to the original scale
    }

    # insert the predictor Ni(t)
    df_full <- data3
    df_full$Ni <- NA
    for (i in 1:nrow(df_full)) {
      id <- df_full$id[i]
      time <- df_full$time[i]
      df_full$Ni[i] <- sum(!is.na(df_full$Y[df_full$id == id & df_full$time <= time]))
    }
    df_full <- df_full %>%
      as.data.frame() %>%
      dplyr::select(all_of(c(colnames(data), "Ni")))
    head(df_full)

    #### Start imputation ####
    # empty imputation
    imp0 <- mice::mice(as.matrix(df_full), maxit = 0)
    predM <- imp0$predictorMatrix
    impM <- imp0$method

    # specify predictor matrix and method
    predM1 <- predM
    predM1["Y", "id"] <- -2
    predM1["Y", c(LM_fixedEffect_withTime_variables, "Ni")] <- 1 # fixed x effects imputation
    impM1 <- impM
    impM1["Y"] <- "2l.lmer"

    # multilevel imputation
    imp1 <- mice::mice(as.matrix(df_full),
                       m = 5,
                       predictorMatrix = predM1, method = impM1, maxit = 5
    )

    # # cox-ph model
    # long_data_imp = mice::complete(imp1)
    # long_data_imp2 = long_data_imp %>%
    #   dplyr::group_by(id) %>%
    #   dplyr::mutate(time0 = ifelse(is.na(dplyr::lag(time)),0,dplyr::lag(time) ))
    # long_data_imp2$d0 = surv_data$d[match(long_data_imp2$id,surv_data$id)]
    # long_data_imp2 = na.omit(long_data_imp2) %>%
    #             dplyr::group_by(id) %>%
    #             dplyr::mutate(d=ifelse(time<max(time,na.rm = TRUE),0,d0))

    # model_formula = as.formula(paste("Surv(time0, time, d) ~ ",paste(SM_variables,collapse="+")))
    # model = survival::coxph(model_formula, data = long_data_imp2)
    # (alpha.hat = summary(model)$coef[,1])

    # fit the cox-ph model
    coxph_imp <- function(data_imp) {
      # cox-ph model
      long_data_imp2 <- data_imp %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(time0 = ifelse(is.na(dplyr::lag(time)), 0, dplyr::lag(time)))
      long_data_imp2$d0 <- surv_data$d[match(long_data_imp2$id, surv_data$id)]
      long_data_imp2 <- na.omit(long_data_imp2) %>%
        dplyr::group_by(id) %>%
        dplyr::filter(time0 < time) %>%
        dplyr::mutate(d = ifelse(time < max(time, na.rm = TRUE), 0, d0))
      model_formula <- as.formula(paste("Surv(time0, time, d) ~ ", paste(SM_variables, collapse = "+")))
      model <- survival::coxph(model_formula, data = long_data_imp2)
      (alpha.hat <- summary(model)$coef[, 1])
    }
    fit <- lapply(1:5, function(i) coxph_imp(mice::complete(imp1, action = i)))
    alpha.hat <- sapply(seq_along(fit[[1]]), function(i) mean(sapply(fit, `[`, i)))
    names(alpha.hat) <- names(fit[[1]])

    return(alpha.hat)
  }
}
