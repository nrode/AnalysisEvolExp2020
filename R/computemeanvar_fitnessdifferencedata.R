#' Fitness Difference Mean And Variance
#'
#' @description Compute the mean and variance of the fitness difference between evolved and ancestral populations using either the delta method or bootstrapping
#' @param seed Seed used for simulation
#' @param ntubes Number of tubes whose individuals have been counted
#' @param N Mean of the negative binomial distribution (i.e. average number of individuals counted per tube)
#' @param theta Overdispersion parameter of the negative binomial distribution (i.e. of the count of individuals per tube)
#'
#' @return
#' @export
#'
#' @examples
#' computemeanvar_fitnessdifferencedata(seed=1, ntubes_ancestor=30, N_ancestor=25, theta_ancestor=3, ntubes_derived=30, N_derived=30, theta_derived=3)
#'
computemeanvar_fitnessdifferencedata <- function(seed=1, ntubes_ancestor=30, N_ancestor=25, theta_ancestor=3, ntubes_derived=30, N_derived=30, theta_derived=3){
  set.seed(seed)
  ## Simulate count data
  fitness_ancestor <- MASS::rnegbin(n = ntubes_ancestor, mu = N_ancestor, theta =  theta_ancestor)
  fitness_derived <- MASS::rnegbin(n = ntubes_derived, mu = N_derived, theta =  theta_derived)
  ## Compute 100 bootstrapped values
  boot_meanfitness_ancestor <- sapply(1:100, boot_fitnessdata, vecindpertube=fitness_ancestor)
  boot_meanfitness_derived <- sapply(1:100, boot_fitnessdata, vecindpertube=fitness_derived)
  diff_meanfitness <- boot_meanfitness_derived-boot_meanfitness_ancestor
  ## Compute quantiles
  quant <- quantile(diff_meanfitness, c(0.025, 0.975))
  quantnormaprox <- 1.96 * sqrt(var(fitness_derived) / (ntubes_derived*mean(fitness_derived)^2) + var(fitness_ancestor)/(ntubes_ancestor*mean(fitness_ancestor)^2))
  quantnormaprox <- c(`2.5%` = log(mean(fitness_derived) / mean(fitness_ancestor)) - quantnormaprox, `97.5%` = log(mean(fitness_derived) / mean(fitness_ancestor)) + quantnormaprox)
  indboot <- as.numeric(ifelse(quant[1]<log(N_derived/N_ancestor) & quant[2]>log(N_derived/N_ancestor), 1, 0))
  indnormaprox <- as.numeric(ifelse(quantnormaprox[1]<log(N_derived/N_ancestor) & quantnormaprox[2]>log(N_derived/N_ancestor), 1, 0))
return(c(expected_meanfitnessdiff=log(N_derived/N_ancestor), observed_meanfitnessdiff = log(mean(fitness_derived)/mean(fitness_ancestor)), bootstrapped_meanfitnessdiff=mean(diff_meanfitness), deltamethod_varfitnessdiff=1/(ntubes_ancestor*N_ancestor) + 1/(ntubes_ancestor*theta_ancestor) + 1/(ntubes_derived*ntubes_derived) + 1/(ntubes_derived*theta_derived), deltamethod_varestimfitnessdiff=var(fitness_ancestor)/(ntubes_ancestor*mean(fitness_ancestor)^2)+var(fitness_derived)/(ntubes_derived*mean(fitness_derived)^2), boot_varestimfitnessdiff=var(diff_meanfitness), CIboot=quant, indicboot=indboot, CInormapprox=quantnormaprox, indicnormapprox=indnormaprox))

}
