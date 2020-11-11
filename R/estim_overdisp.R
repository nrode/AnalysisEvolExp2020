#' Analyze A Dataset Using A Negative Binomial Distribution
#'
#' @description Analyze a dataset using a negative binomial distribution and returns the fitted mean, variance and overdispersion
#' @param fruit Type of fruit considered
#' @param data Dataset considered
#' @param generation Generation considered
#'
#' @return
#' @export
#'
#' @examples
#' estim_overdisp(data=data_G0, fruit="Cherry", generation="G0")
#'
estim_overdisp <- function(fruit, data, generation){
  ## Fit model
  m <- MASS::glm.nb(Nb_adults~1, data=data[data$Treatment==fruit,])
  ## GEt parameters
  as.vector(c(fruit, generation, round(c(as.numeric(exp(coef(m))), m$theta, as.numeric(exp(coef(m))+(exp(coef(m))^2)/m$theta), var(data$Nb_adults[data$Treatment==fruit])/mean(data$Nb_adults[data$Treatment==fruit]), 1+as.numeric(exp(coef(m)))/m$theta), 2)))
}
