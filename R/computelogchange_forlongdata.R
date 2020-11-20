#' @title Calculation difference in fitness: from longitudinal data to logchange
#'
#' @description  Log change in fitness in the three phase for the population i in selective fruit j
#'
#' = log(mean(nb_adults_ij_allgenerationphase1/mean(nb_adlts_j_G2))
#' = log(mean(nb_adults_ij_allgenerationphase3/mean(nb_adlts_j_G2))

#' Example for CE2 during phase1
#' log(mean (CE2 in G2 G3 G4 G5) / mean (all lines Cherry in G2))
#'
#' Mean and sd are weighted by the number of tubes counted
#'
#'
#'
#'
#' @param longitudinal_dataset Longitudinal dataset created using loadlongitudinaldata()

#' @return dataset of fitness difference between fitness during step and G2 mean per fruit
#' @export
#'
#' @examples
#'data_sum <- computelogchange_forlongdata(longitudinal_dataset = data_sum)

computelogchange_forlongdata <- function(longitudinal_dataset = data_sum){

  # Calculate weighted mean during each step for each line
  TEMP_data_step <- Rmisc::summarySE(longitudinal_dataset,
                                      measurevar = "Nb_adults",
                                      groupvars = c("Line", "Fruit_s", "Step"))

  TEMP_data_step <- data.frame(Line=tapply(longitudinal_dataset$Line,longitudinal_dataset$Line,unique),
                               Fruit_s=tapply(longitudinal_dataset$Fruit_s,longitudinal_dataset$Line,unique),
                               Step=tapply(longitudinal_dataset$Step,longitudinal_dataset$Line,unique),
                               N=tapply(longitudinal_dataset$N,longitudinal_dataset$Line,sum),
                               Nb_adults=sapply(split(longitudinal_dataset,longitudinal_dataset$Line),
                                           function(x) weighted.mean(x$Nb_adults,x$N)),
                               sd=sapply(split(longitudinal_dataset,longitudinal_dataset$Line),
                                         function(x) radiant.data::weighted.sd(x$Nb_adults,x$N)))

  # Calculate G2 mean and sd
  TEMP_G2 <- longitudinal_dataset[longitudinal_dataset$Generation=="2",]
  TEMP_mean_G2 <- data.frame(Mean_G2 = sapply(split(TEMP_G2,TEMP_G2$Fruit_s),
                                              function(x) weighted.mean(x$Nb_adults,x$N)),
                             Sd_G2 = sapply(split(TEMP_G2,TEMP_G2$Fruit_s),
                                            function(x) radiant.data::weighted.sd(x$Nb_adults,x$N)),
                             N_G2 = tapply(TEMP_G2$N, TEMP_G2$Fruit_s, sum),
                             Fruit_s = rownames(tapply(TEMP_G2$Nb_adults, TEMP_G2$Fruit_s, mean)))

  # Merge three phenotyping steps
  data_logchange_long <- merge(TEMP_data_step, TEMP_mean_G2, by="Fruit_s")

  # Calculate logchange
  data_logchange_long$logchange_long <- log(data_logchange_long$Nb_adults / data_logchange_long$Mean_G2)

  # Calculate sd logchange
  data_logchange_long$sd_logchange_long <- sqrt(((data_logchange_long$sd^2) / (data_logchange_long$N*(data_logchange_long$Nb_adults^2))) +
                                        ((data_logchange_long$Sd_G2^2) / (data_logchange_long$N_G2*(data_logchange_long$Mean_G2^2))))

  # Subset
  data_logchange_long<-subset(data_logchange_long, select = -c(N,Nb_adults,sd,Mean_G2,Sd_G2,N_G2))

  return(data_logchange_long)

}
