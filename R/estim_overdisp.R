#' Analyze A Dataset Using A Negative Binomial Distribution
#'
#' @description Analyze a dataset using a negative binomial distribution and returns the fitted mean, variance and overdispersion
#' @param colfactor Factor considered (e.g. fruit)
#' @param colfactorlevel Level of factor considered (e.g. cherry)
#' @param data Dataset considered
#' @param generation Generation considered
#' @param trait Trait considered (default Nb_adults)
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
estim_overdisp <- function(colfactorlevel, colfactor, data, generation, trait = "Nb_adults"){
  ## Subset data with the right factor level
  data <- data[data[, colfactor]==colfactorlevel,]
  ## Fit model
  if(trait=="Nb_adults"){
    m <- MASS::glm.nb(Nb_adults~1, data=data)
  }else{
    if(trait=="Nb_eggs"){
      m <- MASS::glm.nb(Nb_eggs~1, data=data)
    }else{
      m <- glm(cbind(Nb_adults, Nb_eggs)~1 , data=data, family="binomial")
    }
  }

  ## Get parameters
    param <- c(nrow(data),
               as.numeric(exp(coef(m))),
               m$theta,
               var(data[, trait]),
               as.numeric(exp(coef(m))+(exp(coef(m))^2)/m$theta),
               var(data[, trait])/mean(data[, trait]),
               1+as.numeric(exp(coef(m)))/m$theta,
               sqrt(nrow(data))*(mean(data[, trait])/sd(data[, trait])),
               ((4*nrow(data)^(3/2))/(1+4*nrow(data)))* (mean(data[, trait])/sd(data[, trait])))

    return(as.vector(c(colfactorlevel, generation, round(param, 2))))

  c("factor", "generation", "number_tubes_counted", "fitted_mean_Emergence_rate", "observed_var_Emergence_rate", "fitted_var_Emergence_rate", "obsoverdisp", "fittedoverdisp", "standardized_mean", "standardized_mean_small_sample")
}
