#' @title HYPOTHESIS 1
#'
#' @description
#'
#' @param fitness_dataset_intermediate Intermediate phenotyping dataset created using loadfitnessdata()t

#' @return
#' @export
#'
#' @examples
#'data_sum <- formatting_POOL_hypo1(logchange_dataset = data_logchange)

formatting_POOL_hypo1 <- function(fitness_dataset_intermediate = data_G7){


  # Mean per fruit
  data_adults_sum_hypo1<-Rmisc::summarySE(fitness_dataset_intermediate,
                                          measurevar="Nb_adults",
                                          groupvars=c("Fruit_s","Treatment"))

  # Calculate initial mean and sd
  TEMP_mean_G1 <- data.frame(Mean_initial = tapply(data_G0$Nb_adults, data_G0$Treatment, mean),
                             Sd_initial = tapply(data_G0$Nb_adults, data_G0$Treatment, sd),
                             N_initial = tapply(data_G0$Nb_adults, data_G0$Treatment, length),
                             Treatment = rownames(tapply(data_G0$Nb_adults, data_G0$Treatment, mean)))

  # Merge three phenotyping steps
  data_hypo1 <- merge(data_adults_sum_hypo1, TEMP_mean_G1, by="Treatment")

  # Calculate logchange
  data_hypo1$logchange <- log(data_hypo1$Nb_adults / data_hypo1$Mean_initial)

  # Calculate sd logchange
  data_hypo1$sd_logchange <- sqrt(((data_hypo1$sd^2) / (data_hypo1$N*(data_hypo1$Nb_adults^2))) +
                                    ((data_hypo1$Sd_initial^2) / (data_hypo1$N_initial*(data_hypo1$Mean_initial^2))))

  # Add sympatry indic
  data_hypo1$SA<-as.factor(ifelse(as.character(data_hypo1$Treatment)==as.character(data_hypo1$Fruit_s),1,0))

  # Subset
  data_hypo1_temp<-subset(data_hypo1, select = -c(N,Nb_adults,sd,se,Mean_initial,Sd_initial,N_initial,ci))

  # Add symp and allop
  data_hypo1_temp$Symp <- data_hypo1_temp$Fruit_s
  data_hypo1_temp$Allop <- data_hypo1_temp$Treatment

  # Add symp and allop
  dataset_hypo1<-data.table::dcast(data.table::setDT(data_hypo1_temp), Fruit_s  ~ Treatment,
                                                   value.var  = c("logchange", "sd_logchange"))

  return(dataset_hypo1)

}

