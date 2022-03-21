#' Example Schedule
#'
#' An edited example of a ten week schedule for thirty-five doctors imported
#' from Medinet using the `load_tidy_schedule()` function. The names have been
#' replaced by randomly generated names using the
#' [swedishname](https://github.com/JonasEngstrom/swedishname) package. The
#' schedule is meant to be used in examples, vignettes, and unit tests in the
#' package, as well as for experimentation in creating useful visualizations and
#' reports using the package, in case Medinet data is not available.
#'
#' @format A tibble with 2,166 rows and 3 variables: \describe{
#'   \item{date}{shift date, in lubridate format} \item{doctor_name}{name of
#'   doctor scheduled to work the shift, as a factor} \item{shift_type}{the type
#'   of shift scheduled, as a factor} }
#' @md
"example_schedule"
