#' Measure statistic function
#' This function calculates a measure statistic with average medicare payments
#' requires ipps_data to be loaded
#' @param x a measure statistic such as mean, median, sd etc.
#'
#' @return a measure statistic
#' @export
#' @import dplyr
#'
#' @examples
#' stat_avg_pay(mean)
stat_avg_pay <- function(x) {
  fun <- deparse(substitute(x))
  output <- ipps_data %>%
    select(`DRG Definition`, `Average Medicare Payments`) %>%
    group_by(`DRG Definition`) %>%
    summarize(N = n(), !!paste0(fun) := x(`Average Medicare Payments`, na.rm = TRUE)) %>%
    arrange(`DRG Definition`)
  output
}
