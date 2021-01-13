#' @title Correlation Function
#' @description  Compute correlation between two variables
#'
#' @param data dataframe
#'
#' @return
#' @export
#'
#' @examples
#' ddply(data_logchangeNb_eggsEmergence_rate, .(Generation, Fruit_s), computecorrelation)

computecorrelation <- function(data)
{

  return(data.frame(correl = cor.test(data$Nb_eggslogchange, data$Emergence_ratelogchange)$estimate, pval = cor.test(data$Nb_eggslogchange, data$Emergence_ratelogchange)$p.value))
}

