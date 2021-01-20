#' @title CI difference of correlations
#'
#' @description  Compute 95% confidence interval of the difference between two correlation coefficient based on Zou 2007 (Zou, G. Y. (2007). Toward using confidence intervals to compare correlations. Psychological methods, 12(4), 399.)
#'
#'
#'
#' @param pair pair of environments
#'
#' @return
#' @export
#'
#' @examples
#'computeCIcordifG29G7(pair="Cherry_Cranberry")

computeCIcordifG7G29 <- function(pair="Cherry_Cranberry"){
## theta1-theta2 in Eq. 11 and 12 of Zhou 2007
dif <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==7& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"]

## theta1-l1 in Eq. 11 of Zhou 2007
difLG7 <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==7& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==7& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="cor_CI_inf"]

## u2-theta2 in Eq. 11 of Zhou 2007
difHG29 <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="cor_CI_sup"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"]

## u1-theta1 in Eq. 12 of Zhou 2007
difHG7 <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==7& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="cor_CI_sup"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==7& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"]

## theta2-l2 in Eq. 12 of Zhou 2007
difLG29 <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="cor_CI_inf"]

return(c(correlationdifference=dif, lowerCI = dif - sqrt(difLG7^2+difHG29^2), higherCI = dif + sqrt(difHG7^2+difLG29^2)))
}
