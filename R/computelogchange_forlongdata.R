#' @title Calculation difference in fitness: from longitudinal data to logchange
#'
#' @description  Log change in fitness in generation x for the population i in the test fruit j = mean(W)Gxij / mean(W)G1j
#'
#'
#'
#' @param longitudinal_dataset Longitudinal dataset created using loadlongitudinaldata()

#' @return dataset of fitness difference between XXXXXXXXXXXXXXXXX and XXXXXXXXXXXX
#' @export
#'
#' @examples
#'data_sum <- computelogchange_forlongdata(longitudinal_dataset = data_sum)

computelogchange_forlongdata <- function(longitudinal_dataset = data_sum){

  # Remove pool step
  longitudinal_dataset<-longitudinal_dataset[longitudinal_dataset$Step!="pool",]

  # Calculate mean during each step for each line
  TEMP_data_step <- Rmisc::summarySE(longitudinal_dataset,
                                      measurevar = "Nb_adults",
                                      groupvars = c("Line", "Fruit_s", "Step"))

  # Calculate G2 mean and sd
  TEMP_G2 <- longitudinal_dataset[longitudinal_dataset$Generation=="2",]
  TEMP_mean_G2 <- data.frame(Mean_G2 = tapply(TEMP_G2$Nb_adults, TEMP_G2$Fruit_s, mean),
                             Sd_G2 = tapply(TEMP_G2$Nb_adults, TEMP_G2$Fruit_s, sd),
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
  data_logchange_long<-subset(data_logchange_long, select = -c(N,Nb_adults,sd,se,Mean_G2,Sd_G2,N_G2,ci))

  return(data_logchange_long)

}
