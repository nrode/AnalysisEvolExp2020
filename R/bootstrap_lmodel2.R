#' @title Lmodel2 with bootstrap
#'
#' @description  Lmodel2 with bootstrap
#'
#'
#'
#' @param data_pairwise dataset from pairwise of fruits, produces using formattinglogchange()
#' @param sd_NA average of the sd in all environments, to replace NA value
#'
#' @return
#' @export
#'
#' @examples
#'sim <- bootstrap_lmodel2(seed=1, data_pairwise = TEMP_dataG7_CheCran, sd_NA = mean_sd_G7)

bootstrap_lmodel2 <- function(seed = 1, data_pairwise = TEMP_dataG7_CheCran, sd_NA = mean_sd_G7) {

  set.seed(seed)


  # Problem with NA (if one sd missing): replace by 0 (max value)
  data_pairwise$sd_allop[is.na(data_pairwise$sd_allop)] <- sd_NA
  data_pairwise$sd_symp[is.na(data_pairwise$sd_symp)] <- sd_NA


  # Compute vector of probability weights
  data_pairwise$Inv_sum <- 1 / ((data_pairwise$sd_symp^2)+(data_pairwise$sd_allop^2))
  data_pairwise$Vector_sample <- data_pairwise$Inv_sum / sum(data_pairwise$Inv_sum,na.rm = TRUE)

  # Weighted sample
  sample_data_pairwise <- data_pairwise[sample(x = nrow(data_pairwise),
                                               size = length(data_pairwise$Line),
                                               replace = TRUE,
                                               prob = data_pairwise$Vector_sample), ]

  # Compute correlation only if at least than three data points
  if ( length(unique(sample_data_pairwise$Line)) > 2 ) {
    # 1- Correlation
    correlation_test<- cor.test(sample_data_pairwise$logchange_allop,sample_data_pairwise$logchange_symp)

    # Extract correlation
    estimate_correlation <- correlation_test$estimate

    # 2- Model with sample dataset
     model <- lmodel2::lmodel2(logchange_allop ~ logchange_symp,
                            range.y = "interval",range.x = "interval",
                            data = sample_data_pairwise, nperm=0)

    # Extract slope
     estimate_intercept <- model$regression.results[2, 2]
     estimate_slope <- model$regression.results[2, 3]

  } else {
    estimate_intercept <- NA
    estimate_slope <- NA
    estimate_correlation <- NA
  }

  return(c(correlation = estimate_correlation,
           intercept = estimate_intercept,
           slope = estimate_slope))
}
