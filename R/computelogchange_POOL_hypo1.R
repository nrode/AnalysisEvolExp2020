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

computelogchange_POOL_hypo1 <- function(fitness_dataset_intermediate = data_G7, fruit1 = "Cherry",fruit2 = "Cranberry"){

  # Mean per fruit
  data_adults_sum_hypo1<-Rmisc::summarySE(fitness_dataset_intermediate,
                                          measurevar="Nb_adults",
                                          groupvars=c("Fruit_s","Treatment"))
  data_adults_sum_hypo1$Generation <- "7"

  # Calculate initial mean and sd
  TEMP_mean_G1 <- data.frame(Mean_initial = tapply(data_G0$Nb_adults, data_G0$Treatment, mean),
                             Sd_initial = tapply(data_G0$Nb_adults, data_G0$Treatment, sd),
                             N_initial = tapply(data_G0$Nb_adults, data_G0$Treatment, length),
                             Treatment = rownames(tapply(data_G0$Nb_adults, data_G0$Treatment, mean)))

  # Merge three phenotyping steps
  data_hypo1 <- merge(data_adults_sum_hypo1, TEMP_mean_G1, by="Treatment")

  # Calculate logchange
  data_hypo1$logchange <- log(data_hypo1$Nb_adults / data_hypo1$Mean_initial)

  # Calculate sd logchange
  data_hypo1$sd_logchange <- sqrt(((data_hypo1$sd^2) / (data_hypo1$N*(data_hypo1$Nb_adults^2))) +
                                    ((data_hypo1$Sd_initial^2) / (data_hypo1$N_initial*(data_hypo1$Mean_initial^2))))

  # Add sympatry indic
  data_hypo1$SA<-as.factor(ifelse(as.character(data_hypo1$Treatment)==as.character(data_hypo1$Fruit_s),1,0))

  # Subset
  data_hypo1<-subset(data_hypo1, select = -c(N,Nb_adults,sd,se,Mean_initial,Sd_initial,N_initial,ci))

  # Add symp and allop
  data_hypo1$Symp <- data_hypo1$Fruit_s
  data_hypo1$Allop <- data_hypo1$Treatment


  #Select allopatric environments
  data_hypo1$logchange_allop<-ifelse(data_hypo1$SA==1,NA,data_hypo1$logchange)
  data_hypo1$sd_allop<-ifelse(data_hypo1$SA==1,NA,data_hypo1$sd_logchange)

  #Select sympatric environments
  #Create temporary data with logchange only in sympatric environments
  temp_symp<-data_hypo1[data_hypo1$SA=="1",c("Fruit_s","logchange","sd_logchange")]
  colnames(temp_symp)<-c("Fruit_s", "logchange_symp","sd_symp")
  data_hypo1_logchange<-merge(data_hypo1,temp_symp,by="Fruit_s")
  data_hypo1_logchange<-data_hypo1_logchange[data_hypo1_logchange$SA=="0",]


  #Create pairwise dataset
  data_hypo1_logchange<-data_hypo1_logchange[data_hypo1_logchange$Symp==fruit1|
                                                  data_hypo1_logchange$Symp==fruit2,]
  data_hypo1_logchange<-data_hypo1_logchange[data_hypo1_logchange$Allop==fruit1|
                                                       data_hypo1_logchange$Allop==fruit2,]




  return(data_hypo1_logchange)

}
