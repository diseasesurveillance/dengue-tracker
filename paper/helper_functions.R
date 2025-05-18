# Helper functions (for reference file processing)
is_ew_format <- function(x) {
  grepl("^\\d{6,7}$", as.character(x))
}

is_date_format <- function(x) {
  d <- try(as.Date(x), silent = TRUE)
  !inherits(d, "try-error") && !is.na(d)
}

date_to_ew <- function(date_obj) {
  aw_obj <- date2week(date_obj, week_start = "Sunday")
  year <- substr(aw_obj, 1, 4)
  weeknum <- substr(aw_obj, 7, 8)
  paste0(year, weeknum)
}

normalize_ew <- function(x) {
  if (is_ew_format(x)) {
    return(as.character(x))
  } else if (is_date_format(x)) {
    return(date_to_ew(as.Date(x)))
  } else {
    stop("Invalid `start` or `end` format. Use YYYYWW or YYYY-MM-DD.")
  }
}