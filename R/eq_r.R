#' @title Create formatted equation for ggplot
#'
#' @description  Create formatted equation for ggplot
#'
#' @param gen Phenotyping step of interest
#' @param pair vector with first fruit of the pairwise and second fruit of the pairwise

#' @return Formatted equation
#' @export
#'
#' @examples
#'eq_r(gen = 7, pair = "Cherry_Cranberry")



## Function equation R
eq_r <- function(gen = 7, pair = "Cherry_Cranberry") {
  if(gen == 7 &pair == "Cherry_Cranberry"){
    as.character(
      as.expression(
        substitute(~~italic(rho)~"="~r2~"["~inf~","~sup~"]",
                   list(r2 = format(Estimates_pairwise$Estimates[Estimates_pairwise$Generation == gen &
                                                                   Estimates_pairwise$Pairwise == pair &
                                                                   Estimates_pairwise$Variables == "correlation"], digits = 2),
                        inf = format(Estimates_pairwise$Estimates[Estimates_pairwise$Generation == gen &
                                                                    Estimates_pairwise$Pairwise == pair &
                                                                    Estimates_pairwise$Variables == "cor_CI_inf"], digits = 1, nsmall=2),
                        sup = format(Estimates_pairwise$Estimates[Estimates_pairwise$Generation == gen &
                                                                    Estimates_pairwise$Pairwise == pair &
                                                                    Estimates_pairwise$Variables == "cor_CI_sup"], digits = 2)))
      )
    )


  }else{
    as.character(
      as.expression(
        substitute(~~italic(rho)~"="~r2~"["~inf~","~sup~"]",
                   list(r2 = format(Estimates_pairwise$Estimates[Estimates_pairwise$Generation == gen &
                                                                   Estimates_pairwise$Pairwise == pair &
                                                                   Estimates_pairwise$Variables == "correlation"], digits = 2),
                        inf = format(Estimates_pairwise$Estimates[Estimates_pairwise$Generation == gen &
                                                                    Estimates_pairwise$Pairwise == pair &
                                                                    Estimates_pairwise$Variables == "cor_CI_inf"], digits = 2, nsmall=2),
                        sup = format(Estimates_pairwise$Estimates[Estimates_pairwise$Generation == gen &
                                                                    Estimates_pairwise$Pairwise == pair &
                                                                    Estimates_pairwise$Variables == "cor_CI_sup"], digits = 2)))
      )
    )
  }

}
