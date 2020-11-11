#' Compute the mean and variance in fitness using bootstrapping
#'
#' @param seed
#' @param ntubes
#' @param N
#' @param theta
#'
#' @return
#' @export
#'
#' @examples
computemeanvar_fitnessdata <- function(seed=1, ntubes=30, N=25, theta=3){
  set.seed(seed)
  ## Simulate count data
  fitnessdata <- MASS::rnegbin(n = ntubes, mu = N, theta =  theta)
  ## Compute 100 bootstrapped values
  boot_meanfitness <- sapply(1:100, boot_fitnessdata, vecindpertube=fitnessdata)
  ## Compute quantiles
  delta <- 1/16
  quant <- quantile(boot_meanfitness, c(0.025*delta, 0.975+0.025*(1-delta)))
  ind <- as.numeric(ifelse(quant[1]<log(N/20)&quant[2]>log(N/20), 1, 0))
return(c(expected_meancount=N, observed_meancount=mean(fitnessdata), expected_variancecount=N+(N^2)/theta, observed_variancecount=var(fitnessdata), expected_meanfitness=log(N/20), bootstrapped_meanfitness=mean(boot_meanfitness), deltamethod_varfitness=1/(ntubes*N)+1/(ntubes*theta), deltamethod_varestimfitness=var(fitnessdata)/(ntubes*mean(fitnessdata)^2), boot_varestimfitness=var(boot_meanfitness), CI=quant, indic=ind))

}
