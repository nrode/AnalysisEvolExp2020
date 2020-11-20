#' @title Lmodel2 with bootstrap
#'
#' @description  Lmodel2 with bootstrap
#'
#'
#'
#' @param data_pairwise dataset from pairwise of fruits,
#' produces using formattinglogchange()

#' @return
#' @export
#'
#' @examples
#'sim <- bootstrap_lmodel2(seed=1, data_pairwise = TEMP_dataG7_CheCran)

bootstrap_lmodel2 <- function(seed = 1, data_pairwise = TEMP_dataG7_CheCran) {

  set.seed(seed)

  # Compute vector of probability weights
  data_pairwise$Inv_sum<-1/((data_pairwise$sd_symp^2)+(data_pairwise$sd_allop^2))
  data_pairwise$Vector_sample <- data_pairwise$Inv_sum/sum(data_pairwise$Inv_sum,na.rm = TRUE)

  # Problem with NA (if one sd missing): replace by 0 (max value)
  data_pairwise$Vector_sample[is.na(data_pairwise$Vector_sample)] <- 0


  # Weighted sample
  sample_data_pairwise <- data_pairwise[sample(nrow(data_pairwise),
                                               length(data_pairwise$Line),
                                               replace = TRUE,
                                               prob = data_pairwise$Vector_sample), ]

  if ( length(unique(sample_data_pairwise$Line)) > 2 ) {
  # Model with sample dataset
  model <- lmodel2::lmodel2(logchange_allop ~ logchange_symp,
                            range.y = "interval",range.x = "interval",
                            data = sample_data_pairwise, nperm=0)

  # Extract slope
  estimate_inercept <- model$regression.results[2,2]
  estimate_slope <- model$regression.results[2,3]
  } else {
    estimate_inercept <- NA
    estimate_slope <- NA
  }

  return(c(intercept = estimate_inercept, slope = estimate_slope))
}
