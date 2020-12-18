#' @title CI difference of correlations
#'
#' @description  Compute 95% confidence interval of the difference between two correlations
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

computeCIcordifG29G7 <- function(pair="Cherry_Cranberry"){

dif <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==7& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"]

difLG29 <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="cor_CI_inf"]

difHG7 <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="cor_CI_sup"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"]

difHG29 <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="cor_CI_sup"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"]

difLG7 <- Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="correlation"] - Estimates_pairwise$Estimates[Estimates_pairwise$Generation==29& Estimates_pairwise$Pairwise==pair&Estimates_pairwise$Variables=="cor_CI_inf"]

return(c(dif=dif, lower = dif - sqrt(difLG29^2+difHG7^2), higher = dif + sqrt(difHG29^2+difLG7^2)))
}
