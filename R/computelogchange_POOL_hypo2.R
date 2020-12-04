#' @title Calculation difference in fitness: from fitness data to logchange for hypo 1
#'
#' @description  Log change in fitness in generation x for the fruit sel i in the test fruit j = mean(W)Gxij / mean(W)G1j
#'
#'
#'   Log change:
#'
#'   logchange=Ln(x1/x2) with x1 and the x2 the mean of Nt+1 and Nt respectively
#'
#'   logchange=ln((meanNt+1)/mean(Nt))
#'
#'
#'
#'   sd log change with Hedges et al. 1999
#'
#'   se(logchange)=sqrt((s1²/n1x1²)+(s2²/n2x2²)) with s1 and s2 the standard deviation of Nt+1 and Nt respesctively,
#'
#'   and n1 and n2 the sample size of  Nt+1 and Nt respesctively
#'
#'   In our case: x2=N0 and s2=sd N0
#'
#'   se(logchange)=sqrt((s1²/n1x1²))=sqrt((sdNt+1²/NNt+1 x meanNt+1²)+(sdN0²/N0 x meanN0²))
#'
#' @param fitness_dataset_intermediate Intermediate phenotyping dataset created using loadfitnessdata()
#' @param fruit1 First fruit of the pairwise
#' @param fruit2 Second fruit of the pairwise

#' @return Dataset with logchange for two fruits during a specific phenotyping step
#' @export
#'
#' @examples
#'data_sum <- computelogchange_POOL_hypo1(fitness_dataset_intermediate = data_G7, fruit1 = "Cherry",fruit2 = "Cranberry")

computelogchange_POOL_hypo2 <- function(fitness_dataset_intermediate = data_G7, fruit1 = "Cherry",fruit2 = "Cranberry"){

  # Mean per fruit
  data_adults_sum_hypo2<-Rmisc::summarySE(fitness_dataset_intermediate,
                                          measurevar="Nb_adults",
                                          groupvars=c("Line","Fruit_s","Treatment"))

  # Extract line with the highest mean
  temp_fruit1<-data_adults_sum_hypo2[data_adults_sum_hypo2$Fruit_s == fruit1&
                                data_adults_sum_hypo2$Treatment == fruit1,]
  temp_fruit2<-data_adults_sum_hypo2[data_adults_sum_hypo2$Fruit_s == fruit2&
                                      data_adults_sum_hypo2$Treatment == fruit2,]
  fittest_line_fruit1<-temp_fruit1$Line[temp_fruit1$Nb_adults == max(temp_fruit1$Nb_adults)]
  fittest_line_fruit2<-temp_fruit2$Line[temp_fruit2$Nb_adults == max(temp_fruit2$Nb_adults)]

  vec_fittest_pop<-c(as.character(fittest_line_fruit1),as.character(fittest_line_fruit2))

  return(vec_fittest_pop)

}
