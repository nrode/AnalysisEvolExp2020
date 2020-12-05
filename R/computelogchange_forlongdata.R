#' @title Calculation difference in fitness: from longitudinal data to logchange
#'
#' @description  Log change in fitness in the three phase for the population i in selective fruit j
#'
#' = log(mean(nb_adults_ij_allgenerationphase1/mean(nb_adlts_j_phase1))
#' = log(mean(nb_adults_ij_allgenerationphase3/mean(nb_adlts_j_phase1))

#' Example for CE2 during phase1
#' log(mean (CE2 in G2 G3 G4 G5) / mean (all lines Cherry in phase1))
#'
#' Mean and sd are weighted by the number of tubes counted
#'
#'
#'
#'
#' @param longitudinal_dataset Longitudinal dataset created using loadlongitudinaldata()

#' @return dataset of fitness difference between fitness during step and phase 1 mean per fruit
#' @export
#'
#' @examples
#'data_sum <- computelogchange_forlongdata(longitudinal_dataset = data_sum)

computelogchange_forlongdata <- function(longitudinal_dataset = data_sum){

  # Calculate weighted mean during each step for each line
  TEMP_data_step <- Rmisc::summarySE(longitudinal_dataset,
                                      measurevar = "Nb_adults",
                                      groupvars = c("Line", "Fruit_s", "Phase"))

  TEMP_data_step <- data.frame(Line=tapply(longitudinal_dataset$Line,longitudinal_dataset$Line,unique),
                               Fruit_s=tapply(longitudinal_dataset$Fruit_s,longitudinal_dataset$Line,unique),
                               Phase=tapply(longitudinal_dataset$Phase,longitudinal_dataset$Line,unique),
                               N=tapply(longitudinal_dataset$N,longitudinal_dataset$Line,sum),
                               Nb_adults=sapply(split(longitudinal_dataset,longitudinal_dataset$Line),
                                           function(x) weighted.mean(x$Nb_adults,x$N)),
                               sd=sapply(split(longitudinal_dataset,longitudinal_dataset$Line),
                                         function(x) radiant.data::weighted.sd(x$Nb_adults,x$N)))

  # Calculate G2 mean and sd
  TEMP_phase1 <- longitudinal_dataset[longitudinal_dataset$Generation=="2"|
                                    longitudinal_dataset$Generation=="3"|
                                    longitudinal_dataset$Generation=="4"|
                                    longitudinal_dataset$Generation=="5",]

  TEMP_mean_phase1<- data.frame(Mean_Ph1 = sapply(split(TEMP_phase1,TEMP_phase1$Fruit_s),
                                              function(x) weighted.mean(x$Nb_adults,x$N)),
                             Sd_Ph1 = sapply(split(TEMP_phase1,TEMP_phase1$Fruit_s),
                                            function(x) radiant.data::weighted.sd(x$Nb_adults,x$N)),
                             N_Ph1 = tapply(TEMP_phase1$N, TEMP_phase1$Fruit_s, sum),
                             Fruit_s = rownames(tapply(TEMP_phase1$Nb_adults, TEMP_phase1$Fruit_s, mean)))

  # Merge three phenotyping steps
  data_logchange_long <- merge(TEMP_data_step, TEMP_mean_phase1, by="Fruit_s")

  # Calculate logchange
  data_logchange_long$logchange_long <- log(data_logchange_long$Nb_adults / data_logchange_long$Mean_Ph1)

  # Calculate sd logchange
  data_logchange_long$sd_logchange_long <- sqrt(((data_logchange_long$sd^2) / (data_logchange_long$N*(data_logchange_long$Nb_adults^2))) +
                                        ((data_logchange_long$Sd_Ph1^2) / (data_logchange_long$N_Ph1*(data_logchange_long$Mean_Ph1^2))))

  # Subset
  data_logchange_long<-subset(data_logchange_long, select = -c(N,Nb_adults,sd,Mean_Ph1,Sd_Ph1,N_Ph1))

  return(data_logchange_long)

}

