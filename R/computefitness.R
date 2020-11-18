#' @title Calculation fitness during phenotyping steps
#'
#' @description  Fitness during phenotyping step: w = log(mean/20)
#' @param fitness_dataset_intermediate Intermediate phenotyping dataset created using loadfitnessdata()
#' @param fitness_dataset_final Final phenotyping dataset created using loadfitnessdata()

#' @return dataset of fitness difference between the phenotyping step and the initial phenotyping step
#' @export
#'
#' @examples
#'data_sum <- computefitness(fitness_dataset_intermediate = data_G7, fitness_dataset_final = data_G29)

computefitness <- function(fitness_dataset_intermediate = data_G7, fitness_dataset_final = data_G29){

  # Calculate mean during intermediate step
  TEMP_data_intermediate <- Rmisc::summarySE(fitness_dataset_intermediate,
                                             measurevar = "Nb_adults",
                                             groupvars = c("Line", "Fruit_s", "Treatment"))

  # Calculate mean during final step
  TEMP_data_final <- Rmisc::summarySE(fitness_dataset_final,
                                      measurevar = "Nb_adults",
                                      groupvars = c("Line", "Fruit_s", "Treatment"))


  # Add generation
  TEMP_data_intermediate$Generation <- "7"
  TEMP_data_final$Generation <- "29"

  # Merge intermediate and final
  data<-rbind(TEMP_data_intermediate,TEMP_data_final)

  # Calculate fitness (log(mean/20))
  data$fitness_pheno <- log(data$Nb_adults/20)

  # Calculate sd fitness
  data$se_fitness_pheno <- sqrt((data$sd^2) / (data$N * (data$Nb_adults^2)))

  return(data)

}
