#' @title Fill out empty dataset with prediction from MA-regression
#'
#' @description  Fill out empty dataset with prediction from Ma-regression
#'
#'
#'
#' @param model output model using lmodel2() ex: mod0_G7_CheCran
#' @param empty_dataset  empty dataframe ex. Summary_estimates_MAregression
#' @param generation generation in the model: 7 or 29
#' @param pairwise pairwise of fruit: Cherry_Cranberry, Cranberry_Strawberry, Strawberry_Cherry
#'
#' @return
#' @export
#'
#' @examples
#'Summary_estimates_MAregression <- Summary_estimates_MAregression(model = mod0_G7_CheCran, empty_dataset = Summary_estimates_MAregression, generation = 7, pairwise = "Cherry_Cranberry")
#'
filldata_maregression <- data.frame(Variables = rep(rep(c("slope", "slope_CI_inf", "slope_CI_sup",
                                                              "intercept", "intercept_CI_inf", "intercept_CI_sup"),2),3),
                                             Generation = rep(c(rep(7, 6), rep(29, 6)),3),
                                             Pairwise = rep(c("Cherry_Cranberry","Cranberry_Strawberry", "Strawberry_Cherry"), each=12),
                                             Estimates = NA)

filldata_maregression <- function(model = mod0_G7_CheCran, empty_dataset = Summary_estimates_MAregression,
                                          generation = 7, pairwise = "Cherry_Cranberry") {

  empty_dataset$Estimates[empty_dataset$Variables == "slope" &
                           empty_dataset$Generation == generation &
                           empty_dataset$Pairwise == pairwise] <- model$regression.results[2, 3]
  empty_dataset$Estimates[empty_dataset$Variables == "slope_CI_inf"&
                           empty_dataset$Generation == generation &
                           empty_dataset$Pairwise == pairwise]  <- model$confidence.intervals[2, 4]
  empty_dataset$Estimates[empty_dataset$Variables == "slope_CI_sup"&
                           empty_dataset$Generation == generation &
                           empty_dataset$Pairwise == pairwise]  <- model$confidence.intervals[2, 5]
  empty_dataset$Estimates[empty_dataset$Variables == "intercept"&
                           empty_dataset$Generation == generation &
                           empty_dataset$Pairwise == pairwise] <- model$regression.results[2, 2]
  empty_dataset$Estimates[empty_dataset$Variables == "intercept_CI_inf"&
                           empty_dataset$Generation == generation &
                           empty_dataset$Pairwise == pairwise]  <- model$confidence.intervals[2, 2]
  empty_dataset$Estimates[empty_dataset$Variables == "intercept_CI_sup"&
                           empty_dataset$Generation == generation &
                           empty_dataset$Pairwise == pairwise]  <- model$confidence.intervals[2, 3]

  return(empty_dataset)

}
