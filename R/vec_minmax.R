#' @title Create formatted vector with limits for ggplot
#'
#' @description  Create formatted vector with minimum and maximum value for axis ggplot
#'
#' @param data_set Dataset used for the plot
#' @param gen Phenotyping step of interest
#' @param fruit selective fruit

#' @return Vector with minimum and maximum valuefor axis ggplot
#' @export
#'
#' @examples
#'vec_minmax(data_logchange,"7","Cherry")

vec_minmax <- function (data_set = data_logchange, gen = "7", fruit ="Cherry") {
  ymin_PAIR_Che_G7=min(min(data_logchange[data_logchange$Generation==gen&
                                            data_logchange$Fruit_s==fruit,]$lowCIlogfecundchange, na.rm= TRUE),
                       min(data_logchange[data_logchange$Generation==gen&
                                            data_logchange$Fruit_s==fruit,]$lowCIlogeggtoadchange, na.rm= TRUE))
  ymax_PAIR_Che_G7=max(max(data_logchange[data_logchange$Generation==gen&
                                            data_logchange$Fruit_s==fruit,]$upCIlogfecundchange, na.rm= TRUE),
                       max(data_logchange[data_logchange$Generation==gen&
                                            data_logchange$Fruit_s==fruit,]$upCIlogeggtoadchange, na.rm= TRUE))
  vec_min_max<-c(ymin_PAIR_Che_G7,ymax_PAIR_Che_G7)
  return(vec_min_max)
}

