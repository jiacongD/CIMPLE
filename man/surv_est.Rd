% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surv_est.R
\name{surv_est}
\alias{surv_est}
\title{Coefficient estimation in the survival model with longitudinal measurements.}
\usage{
surv_est(
  long_data,
  surv_data,
  method,
  id_var,
  time = NULL,
  survTime = NULL,
  survEvent = NULL,
  LM_fixedEffect_variables = NULL,
  LM_randomEffect_variables = NULL,
  SM_timeVarying_variables = NULL,
  SM_timeInvariant_variables = NULL,
  imp_time_factor = NULL
)
}
\arguments{
\item{long_data}{Longitudinal dataset.}

\item{surv_data}{Survival dataset.}

\item{method}{The following methods are available:
\itemize{
\item \code{cox}: Cox proportional hazard model with time-varying covariates.
\item \code{JMLD}: Joint modeling the longitudinal and disease diagnosis processes.
\item \code{VA_JMLD}: Joint modeling the longitudinal and disease diagnosis processes with an adjustment for the historical number of visits in the longitudinal model.
\item \code{Imputation_Cox}: Cox proportional hazard model with time-varying covariates after imputation.
\item \code{VAImputation_Cox}: Cox proportional hazard model with time-varying covariates after imputation with an adjustment for the historical number of visits in the longitudinal model.
}}

\item{id_var}{Variable for the subject ID to indicate the grouping
structure.}

\item{time}{Variable for the observational time.}

\item{survTime}{Variable for the survival time.}

\item{survEvent}{Variable for the survival event.}

\item{LM_fixedEffect_variables}{Vector input of variable names with fixed
effects in the longitudinal model. Variables should not contain time.}

\item{LM_randomEffect_variables}{Vector input of variable names with random
effects in the longitudinal model.}

\item{SM_timeVarying_variables}{Vector input of variable names for
time-varying variables in the survival model.}

\item{SM_timeInvariant_variables}{Vector input of variable names for
time-invariant variables in the survival model.}

\item{imp_time_factor}{Scale factor for the time variable. This argument is
only needed in the imputation-based methods, e.g., \code{Imputation_Cox} and
\code{VAImputation_Cox}. The default is \code{NULL} (no scale).}
}
\value{
\code{alpha_hat}: Estimated coefficients for the survival model.

Other output in each method:
\itemize{
\item \code{JMLD}:
\itemize{
\item \code{beta_hat}: Estimated coefficients for the longitudinal model.
}
\item \code{VA_JMLD}:
\itemize{
\item \code{beta_hat}: Estimated coefficients for the longitudinal model.
}
}
}
\description{
This function offers a collection of methods of coefficient estimation in a
survival model with a longitudinally measured predictor. These methods
include Cox proportional hazard model with time-varying covariates (\code{cox}),
Joint modeling the longitudinal and disease diagnosis processes (\code{JMLD}), Joint
modeling the longitudinal and disease diagnosis processes with an adjustment
for the historical number of visits in the longitudinal model (\code{VA_JMLD}), Cox
proportional hazard model with time-varying covariates after imputation
(\code{Imputation_Cox}), Cox proportional hazard model with time-varying covariates
after imputation with an adjustment for the historical number of visits in
the longitudinal model (\code{VAImputation_Cox}).
}
\examples{
# Setup arguments

id_var = "id"
time = "time"
survTime = "D"
survEvent = "d"
LM_fixedEffect_variables = c("Age","Sex","SNP")
LM_randomEffect_variables = c("SNP")
SM_timeVarying_variables = c("Y")
SM_timeInvariant_variables = c("Age","Sex","SNP")
imp_time_factor = 1

# Run the cox model
fit_cox = surv_est(surv_data = surv_data,
                   long_data = long_data,
                   method = "cox",
                   id_var = id_var,
                   time = time,
                   survTime = survTime,
                   survEvent = survEvent,
                   LM_fixedEffect_variables = LM_fixedEffect_variables,
                   LM_randomEffect_variables = LM_randomEffect_variables,
                   SM_timeVarying_variables = SM_timeVarying_variables,
                   SM_timeInvariant_variables = SM_timeInvariant_variables,
                   imp_time_factor = imp_time_factor)
# Return the coefficient estimates
fit_cox$alpha_hat
}
\references{
Rizopoulos, D. (2010). Jm: An r package for the joint modelling of longitudinal and time-to-event data. Journal of
statistical software, 35:1–33.

Rizopoulos, D. (2012b). Joint models for longitudinal and time-to-event data: With applications in R. CRC press.
}
