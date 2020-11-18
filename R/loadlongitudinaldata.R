#' @title Longitudinal fitness data Import Function
#'
#' @description Reads a fitness data file in table format and subset it to return the dataframe with all the generations
#' @param dataset Fitness dataset: csv file
#' @param rm_generation1 First generation removed due to GF development or parental GF development
#' @param rm_generation2 Second generation removed due to GF development or parental GF development
#' @param rm_generation3 Third generation removed due to GF development or parental GF development
#'
#' @return
#' @export
#'
#' @examples
#'data_sum <- loadlongitudinaldata(dataset = "DATA_Adults_G1G29.csv", rm_generation1 = 1,rm_generation2 = 7,rm_generation3 = 29)

loadlongitudinaldata <- function(dataset = "DATA_Adults_G1G29.csv", rm_generation1 = 1, rm_generation2 = 7, rm_generation3 = 29){

  ######  0- Load data
  # Path using the here function so that the command line is reproducible across platforms
  data_complet <- read.table(file=here::here("data", dataset), sep=";", header=TRUE)

  # Subset dataset
  data_fitness_all <- data_complet[data_complet$Treatment == "Cherry"|
                                   data_complet$Treatment == "Cranberry"|
                                   data_complet$Treatment == "Strawberry",]

  # Remove generation with parental development on GF : G1 G7 and G29
  TEMP_data_fitness_all<-data_fitness_all[data_fitness_all$Generation != rm_generation1&
                                            data_fitness_all$Generation != rm_generation2&
                                            data_fitness_all$Generation != rm_generation3,]


  ######  1- Extract mean for each lines and each generations + se + N (number of tubes)
  data<-Rmisc::summarySE(TEMP_data_fitness_all,
                                 measurevar="Nb_adults",
                                 groupvars=c("Lines", "Fruit_s", "Generation", "Step", "T_counted"), na.rm=TRUE)

  # Replace the N (=Number of tubes used to estimate the mean) for pool step (replace NA by T_counted)
  data[data$Step=="pool",]$N <- data[data$Step=="pool",]$T_counted

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

  #Check with se:
  data$se_fitness_verif <- sqrt((data$se^2) / (data$Nb_adults^2))

  #Remove useless columns
  data <- subset(data, select = -c(se, se_fitness_verif))



  return(data)


}
