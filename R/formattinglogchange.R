#' @title Formatting a logchange dataset to pairwise dataset for testing MA-regression
#'
#' @description  Formatting a logchange dataset
#' From a logchange dataset created using computelogchange() to pairwise dataset for testing MA-regression
#'
#' @param logchange_data Intermediate phenotyping dataset created using loadfitnessdata()
#' @param generation Phenotyping step of interest
#' @param fruit1 First fruit of the pairwise
#' @param fruit2 Second fruit of the pairwise

#' @return Dataset with logchange for two fruits during a specific phenotyping step
#' @export
#'
#' @examples
#'data_sum <- formattinglogchange(logchange_dataset = data_logchange, generation = "7", fruit1 = "Cherry",fruit2 = "Cranberry")

formattinglogchange <- function(logchange_dataset = data_logchange,
                                generation = "7", fruit1 = "Cherry", fruit2 = "Cranberry"){

  #Temporary dataset
  TEMP_data_sum_G7G29_logchange<-logchange_dataset

  #Select allopatric environments
  TEMP_data_sum_G7G29_logchange$logchange_allop<-ifelse(TEMP_data_sum_G7G29_logchange$SA==1,NA,TEMP_data_sum_G7G29_logchange$logchange)
  TEMP_data_sum_G7G29_logchange$sd_allop<-ifelse(TEMP_data_sum_G7G29_logchange$SA==1,NA,TEMP_data_sum_G7G29_logchange$sd_logchange)

  #Select sympatric environments
  #Create temporary data with logchange only in sympatric environments
  temp_symp <- TEMP_data_sum_G7G29_logchange[TEMP_data_sum_G7G29_logchange$SA=="1", c("Line","logchange","sd_logchange", "N")]
  colnames(temp_symp)<-c("Line", "logchange_symp", "sd_symp", "N_symp")
  data_sum_G7G29_logchange_temp <- merge(TEMP_data_sum_G7G29_logchange, temp_symp, by="Line")
  data_sum_G7G29_logchange <- data_sum_G7G29_logchange_temp[data_sum_G7G29_logchange_temp$SA=="0",]

  names(data_sum_G7G29_logchange)[names(data_sum_G7G29_logchange)=="N"] <- "N_allop"
  # Create pairwise dataset
  data_pairwise_generation<-data_sum_G7G29_logchange[data_sum_G7G29_logchange$Generation==generation&
                                                       data_sum_G7G29_logchange$Symp==fruit1|
                                                       data_sum_G7G29_logchange$Generation==generation&
                                                       data_sum_G7G29_logchange$Symp==fruit2,]
  data_pairwise_generation<-data_pairwise_generation[data_pairwise_generation$Allop==fruit1|
                                                       data_pairwise_generation$Allop==fruit2,]


  return(data_pairwise_generation)


}
