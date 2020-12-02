#' @title Longitudinal fitness data Import Function for ALL FRUITS
#'
#' @description Reads a fitness data file in table format and subset it to return the dataframe with all the generations
#' @param dataset Fitness dataset: csv file
#' @param rm_generation_max Last generation of interest
#'
#' @return
#' @export
#'
#' @examples
#'data_sum <- loadlongitudinaldata_allfruits(dataset = "DATA_Adults_G1G29.csv", rm_generation_max==5)

loadlongitudinaldata_allfruits <- function(dataset = "DATA_Adults_G1G29.csv", rm_generation_max = 5){

  ######  0- Load data
  # Path using the here function so that the command line is reproducible across platforms
  data_fitness_all <- read.table(file=here::here("data", dataset), sep=";", header=TRUE)


  # Keep only
  TEMP_data_fitness_all<-data_fitness_all[data_fitness_all$Generation <= rm_generation_max,]


  ######  1- Extract mean for each lines and each generations + se + N (number of tubes)
  data<-Rmisc::summarySE(TEMP_data_fitness_all,
                         measurevar="Nb_adults",
                         groupvars=c("Line", "Fruit_s", "Generation", "Phase", "T_counted"), na.rm=TRUE)

  # Replace the N (=Number of tubes used to estimate the mean) for pool Phase (replace NA by T_counted)
  data[data$Phase=="pool",]$N <- data[data$Phase=="pool",]$T_counted

  # Remove useless columns
  data <- subset(data, select = -c(T_counted,ci))

  ###### 2- Calculate fitness (log(mean/20))
  data$fitness <- log(data$Nb_adults/20)

  ###### 3- Calculate se for fitness (sqrt(sd/N*mean))
  #With Hedges et al. 1999
  #logchange=Ln(x1/x2) with x1 and the x2 the mean of Nt+1 and Nt respectively
  #logchange=ln((meanNt+1)/20)
  #se(logchange)=sqrt((s1²/n1x1²)+(s2²/n2x2²)) with s1 and s2 the standard deviation of Nt+1 and Nt respesctively,
  #and n1 and n2 the sample size of  Nt+1 and Nt respesctively
  #In our case: x2=20 and s2=0
  #So: se(logchange)=sqrt((s1²/n1x1²))=sqrt((sdNt+1²)/(NNt+1*meanNt+1²))

  data$se_fitness <- sqrt((data$sd^2) / (data$N * (data$Nb_adults^2)))

  #Remove useless columns
  data <- subset(data, select = -c(se))


  return(data)


}
