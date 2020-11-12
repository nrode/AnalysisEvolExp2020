#' Fitness Mean And Variance
#'
#' @description Compute the mean and the variance in fitness of a population using either the delta method or bootstrapping
#' @param seed Seed used for simulation
#' @param ntubes Number of tubes whose individuals have been counted
#' @param N Mean of the negative binomial distribution (i.e. average number of individuals counted per tube)
#' @param theta Overdispersion parameter of the negative binomial distribution (i.e. of the count of individuals per tube)
#'
#' @return
#' @export
#'
#' @examples
#'computemeanvar_fitnessdata(seed=1, ntubes=30, N=25, theta=3)

computemeanvar_fitnessdata <- function(seed=1, ntubes=30, N=25, theta=3){
  set.seed(seed)
  ## Simulate count data
  fitnessdata <- MASS::rnegbin(n = ntubes, mu = N, theta =  theta)
  ## Compute 100 bootstrapped values
  boot_meanfitness <- sapply(1:100, boot_fitnessdata, vecindpertube=fitnessdata)
  ## Compute quantiles
  quant <- quantile(boot_meanfitness, c(0.025, 0.975))
  ind <- as.numeric(ifelse(quant[1]<log(N/20)&quant[2]>log(N/20), 1, 0))
  quantnormaprox <- 1.96 * sqrt(var(fitnessdata) / (ntubes*mean(fitnessdata)^2))
  quantnormaprox <- c(`2.5%` = log(mean(fitnessdata) / 20) - quantnormaprox, `97.5%` = log(mean(fitnessdata) / 20) + quantnormaprox)
  indnormaprox <- as.numeric(ifelse(quantnormaprox[1] < log(N/20) & quantnormaprox[2] > log(N/20), 1, 0))
return(c(expected_meancount = N, observed_meancount = mean(fitnessdata), expected_variancecount = N + (N^2)/theta, observed_variancecount = var(fitnessdata), expected_meanfitness = log(N/20), observed_meanfitness = log(mean(fitnessdata)/20), bootstrapped_meanfitness = mean(boot_meanfitness), deltamethod_varfitness = 1/(ntubes * N) + 1 / (ntubes * theta), deltamethod_varestimfitness = var(fitnessdata) / (ntubes * mean(fitnessdata)^2), boot_varestimfitness = var(boot_meanfitness), CIboot = quant, indicboot = ind, CInormaprox = quantnormaprox, indicnormaprox=indnormaprox))

}
