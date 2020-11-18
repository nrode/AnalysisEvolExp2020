' @title Formatting a logchange dataset to test correlation
#'
#' @description  Formatting a logchange dataset to test correlation
#' From a logchange dataset created using computelogchange() to dataset for testing MA-regression
#'
#' @param logchange_data Intermediate phenotyping dataset created using loadfitnessdata()

#' @return Dataset
#' @export
#'
#' @examples
#'data_sum <- formattinglogchange(logchange_dataset = data_logchange)

formattinglogchange <- function(logchange_dataset = data_logchange){

  #Temporary dataset
  TEMP_data_sum_G7G29_logchange<-logchange_dataset

  #Select allopatric environments
  TEMP_data_sum_G7G29_logchange$logchange_allop<-ifelse(TEMP_data_sum_G7G29_logchange$SA==1,NA,TEMP_data_sum_G7G29_logchange$logchange)
  TEMP_data_sum_G7G29_logchange$sd_allop<-ifelse(TEMP_data_sum_G7G29_logchange$SA==1,NA,TEMP_data_sum_G7G29_logchange$sd_logchange)

  #Select sympatric environments
  #Create temporary data with logchange only in sympatric environments
  temp_symp<-TEMP_data_sum_G7G29_logchange[TEMP_data_sum_G7G29_logchange$SA=="1",c("Line","logchange","sd_logchange")]
  colnames(temp_symp)<-c("Line", "logchange_symp","sd_symp")
  data_sum_G7G29_logchange_temp<-merge(TEMP_data_sum_G7G29_logchange,temp_symp,by="Line")
  data_sum_G7G29_logchange<-data_sum_G7G29_logchange_temp[data_sum_G7G29_logchange_temp$SA=="0",]

  return(data_sum_G7G29_logchange)

}
