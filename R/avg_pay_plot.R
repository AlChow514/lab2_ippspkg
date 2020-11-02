#' Box plot of Average Medicare Payment vs DRUG code
#'
#' This function produces a box plot of average medicare payment with a 3 digit drug code
#' requires ipps data to be loaded
#'
#' @param xcode input a 3 digit drug code
#'
#' @return a box plot of Average Medicare Payment vs DRUG code
#' @export
#' @import dplyr
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @examples
#' avg_pay_plot(039)
avg_pay_plt <- function(xcode) {
  xcodefun <- sprintf('%03d', xcode)
  ipps_data %>%
    mutate(drg_code = substr(`DRG Definition`, start = 1, stop = 3)) %>%
    filter(drg_code == xcodefun) %>%
    select(`DRG Definition`, `Average Medicare Payments`, drg_code) %>%
    ggplot(mapping = aes(x = xcodefun, y = `Average Medicare Payments`)) +
    geom_boxplot() +
    ggtitle("Average Medicare Payment vs DRUG code") +
    theme(axis.title.x = element_blank())
}
