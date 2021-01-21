#' @title Fill out dataset with correlation estimates
#'
#' @description  Fill out dataset with estimates from correlation test and lmodel2 output
#'
#' @param emptydata Empty dataset
#' @param gen Phenotyping step of interest
#' @param pair vector with first fruit of the pairwise and second fruit of the pairwise

#' @return Filled dataset
#' @export
#'
#' @examples
#'Estimates_pairwise <- fillout_estimate_correlation (Estimates_pairwise, 7, "Cherry_Cranberry")



fillout_estimate_correlation <- function(emptydata = Estimates_pairwise, gen = 7, pair = "Cherry_Cranberry") {
  emptydata$Estimates[emptydata$Generation == gen &
                        emptydata$Pairwise == pair &
                        emptydata$Variables=="correlation"] <- weightedcor$estimate
  emptydata$Estimates[emptydata$Generation == gen &
                        emptydata$Pairwise == pair &
                        emptydata$Variables=="cor_CI_inf"] <- weightedcor$ci[1]
  emptydata$Estimates[emptydata$Generation == gen &
                        emptydata$Pairwise == pair &
                        emptydata$Variables=="cor_CI_sup"] <- weightedcor$ci[2]
  emptydata$Estimates[emptydata$Generation == gen &
                        emptydata$Pairwise == pair &
                        emptydata$Variables=="intercept"] <- model$regression.results[2, 2]
  emptydata$Estimates[emptydata$Generation == gen &
                        emptydata$Pairwise == pair &
                        emptydata$Variables=="slope"] <- model$regression.results[2, 3]

  return (emptydata)
}
