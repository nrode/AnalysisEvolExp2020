#' @title Formatting a logchange dataset to pairwise dataset for plot
#'
#' @description  Formatting a logchange dataset
#' From a logchange dataset created using computelogchange() to pairwise dataset for supplementary plot
#'
#' @param logchange_data Intermediate phenotyping dataset created using loadfitnessdata()
#' @param generation Phenotyping step of interest
#' @param fruit1 First fruit of the pairwise
#' @param fruit2 Second fruit of the pairwise

#' @return Dataset with logchange for two fruits during a specific phenotyping step
#' @export
#'
#' @examples
#'data_sum <- formattingplotpair(logchange_dataset = data_logchange, generation = "7", fruit1 = "Cherry",fruit2 = "Cranberry")

formattingplotpair <- function(logchange_dataset = data_logchange,
                                generation = "7", fruit1 = "Cherry",fruit2 = "Cranberry"){


  TEMP_data_generation<-logchange_dataset[logchange_dataset$Generation==generation,]
  TEMP_data_generation<-TEMP_data_generation[TEMP_data_generation$Treatment==fruit1|TEMP_data_generation$Treatment==fruit2,]

  data_generation_fruit1_fruit2<-data.table::dcast(data.table::setDT(TEMP_data_generation), Line + Fruit_s  ~ Treatment,
                         value.var  = c("logchange", "lowCIlogfitnesschange", "upCIlogfitnesschange"))

  return(data_generation_fruit1_fruit2)

}
