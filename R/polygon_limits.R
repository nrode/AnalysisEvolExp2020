#' @title Calculate polygon limits from lmodel2
#'
#' @description  Calculate polygon limits for Ggplot2 (grey area)
#'
#'
#'
#' @param model output model using lmodel2()

#' @return
#' @export
#'
#' @examples
#'polygondata <- filldata_predict_maregression(model = mod0_G7_CheCran)



filldata_predict_maregression <- function(model = mod0_G7_CheCran) {

    # Limits for plot (arbitrary)
  ymin=-5
  ymax=5

  # Create empty dataframe with slopes and confidence interval for all pairwise
  empty_dataset <- data.frame(Variables = c("slope", "slope_CI_inf", "slope_CI_sup",
                                            "intercept", "intercept_CI_inf", "intercept_CI_sup"),
                              Estimate = NA)

  # Fill dataset
  empty_dataset$Estimate[empty_dataset$Variables=="slope"] <- model$regression.results[2, 3]
  empty_dataset$Estimate[empty_dataset$Variables=="slope_CI_inf"] <- model$confidence.intervals[2, 4]
  empty_dataset$Estimate[empty_dataset$Variables=="slope_CI_sup"] <- model$confidence.intervals[2, 5]
  empty_dataset$Estimate[empty_dataset$Variables=="intercept"] <- model$regression.results[2, 2]
  empty_dataset$Estimate[empty_dataset$Variables=="intercept_CI_inf"] <- model$confidence.intervals[2, 2]
  empty_dataset$Estimate[empty_dataset$Variables=="intercept_CI_sup"] <- model$confidence.intervals[2, 3]


  if (empty_dataset$Estimate[empty_dataset$Variables=="slope"] < 1 & empty_dataset$Estimate[empty_dataset$Variables=="slope"] > - 1 ) {
    # Polygon limits
    intercept <- empty_dataset$Estimate[empty_dataset$Variables == "intercept"]
    inf_min<-(empty_dataset$Estimate[empty_dataset$Variables=="slope_CI_inf"]*ymin)+intercept
    inf_max<-(empty_dataset$Estimate[empty_dataset$Variables=="slope_CI_inf"]*ymax)+intercept
    sup_min <- (ymin-intercept)/empty_dataset$Estimate[empty_dataset$Variables == "slope_CI_sup"]
    sup_max <- (ymax-intercept)/empty_dataset$Estimate[empty_dataset$Variables == "slope_CI_sup"]



    # Dataframe with coordiantes
    polygon_data <- data.frame (xvalue = c(ymin, sup_min,
                                           sup_max,ymax),
                                yvalue = c(inf_min,ymin,
                                           ymax, inf_max))

  } else {
    # Polygon limits
    intercept <- empty_dataset$Estimate[empty_dataset$Variables == "intercept"]
    inf_min <- (ymin-intercept)/empty_dataset$Estimate[empty_dataset$Variables == "slope_CI_inf"]
    inf_max <- (ymax-intercept)/empty_dataset$Estimate[empty_dataset$Variables == "slope_CI_inf"]
    sup_min <- (ymin-intercept)/empty_dataset$Estimate[empty_dataset$Variables == "slope_CI_sup"]
    sup_max <- (ymax-intercept)/empty_dataset$Estimate[empty_dataset$Variables == "slope_CI_sup"]

    # Dataframe with coordiantes
    polygon_data <- data.frame (xvalue = c(inf_min,sup_min,
                                           sup_max,inf_max),
                                yvalue = c(ymin, ymin,
                                           ymax, ymax))
  }


  return(polygon_data)


}

