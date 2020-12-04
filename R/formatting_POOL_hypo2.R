#' @title HYPOTHESIS 1
#'
#' @description
#'
#' @param fitness_dataset_intermediate Intermediate phenotyping dataset created using loadfitnessdata()

#' @return
#' @export
#'
#' @examples
#'data_sum <- formatting_POOL_hypo1(logchange_dataset = data_logchange, generation = "7")

formatting_POOL_hypo2 <- function(fitness_dataset_intermediate = data_G7){

  # Mean per fruit
  data_adults_sum_hypo2<-Rmisc::summarySE(fitness_dataset_intermediate,
                                          measurevar="Nb_adults",
                                          groupvars=c("Line","Fruit_s","Treatment"))

  # Extract line with the highest mean
  temp_fruit1<-data_adults_sum_hypo2[data_adults_sum_hypo2$Fruit_s == "Cherry"&
                                       data_adults_sum_hypo2$Treatment == "Cherry",]
  temp_fruit2<-data_adults_sum_hypo2[data_adults_sum_hypo2$Fruit_s == "Strawberry"&
                                       data_adults_sum_hypo2$Treatment == "Strawberry",]
  temp_fruit3<-data_adults_sum_hypo2[data_adults_sum_hypo2$Fruit_s == "Cranberry"&
                                       data_adults_sum_hypo2$Treatment == "Cranberry",]
  fittest_line_fruit1<-temp_fruit1$Line[temp_fruit1$Nb_adults == max(temp_fruit1$Nb_adults)]
  fittest_line_fruit2<-temp_fruit2$Line[temp_fruit2$Nb_adults == max(temp_fruit2$Nb_adults)]
  fittest_line_fruit3<-temp_fruit3$Line[temp_fruit3$Nb_adults == max(temp_fruit3$Nb_adults)]


  vec_fittest_pop<-c(as.character(fittest_line_fruit1),as.character(fittest_line_fruit2),as.character(fittest_line_fruit3) )

  return(vec_fittest_pop)

}
