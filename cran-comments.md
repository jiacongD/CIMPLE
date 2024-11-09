## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Current Resubmission Notes

The package has been updated to address the following issues identified by Benjamin Altmann:

"You missed one instance of cat() in R/long_est.R (line 967) which cannot
be suppressed. Please change that to message() or wrap it in if(verbose)
as well."

- `cat()` in R/long_est.R (line 967) has been changed to `message()`.


## Prior Resubmission Notes

The package has been updated to address the following issues identified by Konstanze Lauseker:

"Please always explain all acronyms in the description text. -> EHR"

- Spelled out acronym: Electronic Health Record (EHR).

"Please omit the redundant "A collection of methods involved with" from the description."

- Omitted.

"You write information messages to the console that cannot be easily suppressed."

- Replaced all instances of print() with message().

