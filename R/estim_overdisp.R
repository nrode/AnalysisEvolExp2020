#' Analyze A Dataset Using A Negative Binomial Distribution
#'
#' @description Analyze a dataset using a negative binomial distribution and returns the fitted mean, variance and overdispersion
#' @param colfactor Factor considered (e.g. fruit)
#' @param colfactorlevel Level of factor considered (e.g. cherry)
#' @param data Dataset considered
#' @param generation Generation considered
#'
#' @return "number_tubes_counted",
#' "fitted_mean_nb_adults",
#' "fitted_theta",
#' "observed_var_nb_adults",
#' "fitted_var_nb_adults",
#' "obsoverdisp=observed_var_nb_adults/observed_mean_adults",
#' "fittedoverdisp==fitted_var_nb_adults/fitted_mean_nb_adults",
#' "standardized_mean",
#' "standardized_mean_small_sample"
#' @export
#'
#' @examples
#' estim_overdisp(data=data_G0, colfactor="Treatment", colfactorlevel="Cherry", generation="G0")
#'
estim_overdisp <- function(colfactorlevel, colfactor, data, generation){
  ## Subset data with the right factor level
  data <- data[data[, colfactor]==colfactorlevel,]
  ## Fit model
  m <- MASS::glm.nb(Nb_adults~1, data=data)
  ## Get parameters
  param <- c(length(data$Nb_adults),
             as.numeric(exp(coef(m))),
             m$theta,
             var(data$Nb_adults),
             as.numeric(exp(coef(m))+(exp(coef(m))^2)/m$theta),
             var(data$Nb_adults)/mean(data$Nb_adults),
             1+as.numeric(exp(coef(m)))/m$theta,
             sqrt(length(data$Nb_adults))*(mean(data$Nb_adults)/sd(data$Nb_adults)),
             ((4*length(data$Nb_adults)^(3/2))/(1+4*length(data$Nb_adults)))* (mean(data$Nb_adults)/sd(data$Nb_adults)))
  as.vector(c(colfactorlevel, generation, round(param, 2)))
}
