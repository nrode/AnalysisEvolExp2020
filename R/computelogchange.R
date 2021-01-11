#' @title Calculation difference in fitness: from fitness data to logchange
#'
#' @description  Log change in fitness in generation x for the population i in the test fruit j = mean(W)Gxij / mean(W)G1j
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
#'   se(logchange)=sqrt((s1²/n1x1²)+(s2²/n2x2²)) with s1 and s2 the standard deviation of Nt+1 and Nt respectively,
#'
#'   and n1 and n2 the sample size of  Nt+1 and Nt respesctively
#'
#'   In our case: x2=N0 and s2=sd N0
#'
#'   se(logchange)=sqrt(s1²/n1x1²)=sqrt((sdNt+1²/(NNt+1 x meanNt+1²))+(sdN0²/(N0 x meanN0²)))
#'
#' @param fitness_dataset_ancestral phenotyping dataset of the ancestral population created using loadfitnessdata()
#' @param fitness_dataset_evolved phenotyping dataset of the ancestral population created using loadfitnessdata()
#' @param trait Trait for which logchange should be computed (e.g. Nb_eggs, Nb_adults, Emergence_rate)
#' @param gen Generation
#' @param byRack Compute fitness difference by rack
#' @return dataset of fitness difference between the phenotyping step and the initial phenotyping step
#' @export
#'
#' @examples
#'data_sum <- computelogchange(fitness_dataset_intermediate = data_G7, fitness_dataset_final = data_G29, trait="Nb_adults")

computelogchange <- function(fitness_dataset_ancestral = data_G0, fitness_dataset_evolved = data_G60, trait="Nb_adults", gen=7, byRack=FALSE){

  # Add generation
  fitness_dataset_evolved$Generation <- as.character(gen)
  if(byRack){
    # Calculate mean during phenotyping steps
    TEMP_data_Gevolved <- Rmisc::summarySE(fitness_dataset_evolved,
                                           measurevar = trait,
                                           groupvars = c("Line", "Fruit_s", "Generation", "Rack", "Treatment"))
  }else{
    # Calculate mean during phenotyping steps
    TEMP_data_Gevolved <- Rmisc::summarySE(fitness_dataset_evolved,
                                           measurevar = trait,
                                           groupvars = c("Line", "Fruit_s", "Generation", "Treatment"))
  }


  # Calculate initial mean and sd
  TEMP_mean_G0 <- data.frame(Mean_initial = tapply(fitness_dataset_ancestral[, trait], fitness_dataset_ancestral$Treatment, mean),
                             Sd_initial = tapply(fitness_dataset_ancestral[, trait], fitness_dataset_ancestral$Treatment, sd),
                             N_initial = tapply(fitness_dataset_ancestral[, trait], fitness_dataset_ancestral$Treatment, length),
                             Treatment = levels(fitness_dataset_ancestral$Treatment))

  # Merge two phenotyping steps
  data_logchange <- merge(TEMP_data_Gevolved, TEMP_mean_G0, by="Treatment")

  # Calculate logchange
  data_logchange$logchange <- log(data_logchange[, trait] / data_logchange$Mean_initial)

  # Calculate sd logchange
  data_logchange$sd_logchange <- sqrt(((data_logchange$sd^2) / (data_logchange$N*(data_logchange[, trait]^2))) +
                                      ((data_logchange$Sd_initial^2) / (data_logchange$N_initial*(data_logchange$Mean_initial^2))))

  # Add sympatry indic
  data_logchange$SA <- as.factor(ifelse(as.character(data_logchange$Treatment)==as.character(data_logchange$Fruit_s),1,0))

  # Subset
  data_logchange <- data_logchange[, !names(data_logchange)%in%c(trait, "sd", "se", "Mean_initial", "Sd_initial", "N_initial", "ci")]

  # Add symp and allop
  data_logchange$Symp <- data_logchange$Fruit_s
  data_logchange$Allop <- data_logchange$Treatment

  return(data_logchange)

}
