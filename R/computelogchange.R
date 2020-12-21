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
#'   se(logchange)=sqrt((s1²/n1x1²)+(s2²/n2x2²)) with s1 and s2 the standard deviation of Nt+1 and Nt respesctively,
#'
#'   and n1 and n2 the sample size of  Nt+1 and Nt respesctively
#'
#'   In our case: x2=N0 and s2=sd N0
#'
#'   se(logchange)=sqrt((s1²/n1x1²))=sqrt((sdNt+1²/NNt+1 x meanNt+1²)+(sdN0²/N0 x meanN0²))
#'
#' @param fitness_dataset_intermediate Intermediate phenotyping dataset created using loadfitnessdata()
#' @param fitness_dataset_final Final phenotyping dataset created using loadfitnessdata()
#' @param trait Trait for which logchange should be computed (e.g. Nb_eggs, Nb_adults, Emergence_rate)
#' @return dataset of fitness difference between the phenotyping step and the initial phenotyping step
#' @export
#'
#' @examples
#'data_sum <- computelogchange(fitness_dataset_intermediate = data_G7, fitness_dataset_final = data_G29, trait="Nb_adults")

computelogchange <- function(fitness_dataset_intermediate = data_G7, fitness_dataset_final = data_G29, trait="Nb_adults"){

  # Add generation
  fitness_dataset_intermediate$Generation <- "7"
  fitness_dataset_final$Generation <- "29"

  # Merge intermediate and final
  fitnessdataset <- rbind(fitness_dataset_intermediate,fitness_dataset_final)

  # Calculate mean during phenotyping steps
  TEMP_data_G7G29 <- Rmisc::summarySE(fitnessdataset,
                       measurevar = trait,
                       groupvars = c("Line", "Fruit_s", "Generation", "Treatment"))

  # Calculate initial mean and sd
  TEMP_mean_G1 <- data.frame(Mean_initial = tapply(data_G0[, trait], data_G0$Treatment, mean),
                         Sd_initial = tapply(data_G0[, trait], data_G0$Treatment, sd),
                         N_initial = tapply(data_G0[, trait], data_G0$Treatment, length),
                         Treatment = levels(data_G0$Treatment))

  # Merge three phenotyping steps
  data_logchange <- merge(TEMP_data_G7G29, TEMP_mean_G1, by="Treatment")

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
