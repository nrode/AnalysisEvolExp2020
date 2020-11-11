##
#' Bootstrap Function That Resample Fitness Date
#'
#' @param seed Seed used of reproductibility of results
#' @param vecindpertube vector with fitness data
#'
#' @return
#' @export
#'
#' @examples
#' bootfunc(seed=1, vecindpertube=sim_derived)

boot_fitnessdata <- function(seed, vecindpertube){
  set.seed(seed)
  log(mean(sample(vecindpertube, size=length(vecindpertube), replace = TRUE))/20)
}
