#' @title Formatting a logchange dataset to pairwise dataset for testing MA-regression
#'
#' @description  Formatting a logchange dataset
#' From a logchange dataset to pairwise dataset for testing MA-regression
#'
#' @param logchange_dataset Intermediate phenotyping dataset created using loadfitnessdata()
#' @param generation Phenotyping step of interest
#' @param fruitcomb vector with first fruit of the pairwise and second fruit of the pairwise
#' @param trait trait of interest: can be "fitness, "fecundity" or  "eggtoad"

#' @return Dataset with logchange for two fruits during a specific phenotyping step
#' @export
#'
#' @examples
#'data_sum <- formattinglogchange(logchange_dataset = data_logchange, generation = "7", fruitcomb=c("Cherry", "Cranberry"), trait="fecundity")


formattinglogchange <- function(logchange_dataset = data_logchange, generation="7",
                                fruitcomb=c("Cherry", "Cranberry"), trait="fecundity"){
  ## Extract sympatric combinations
  TEMP_symp <- logchange_dataset[logchange_dataset$Generation==generation&
                                   logchange_dataset$Treatment%in%fruitcomb&
                                   logchange_dataset$Fruit_s%in%fruitcomb&logchange_dataset$SA==1,]
  names(TEMP_symp)[6:ncol(TEMP_symp)] <- paste0(names(TEMP_symp)[6:ncol(TEMP_symp)], "_symp")
  ## Sort dataset
  TEMP_symp <- TEMP_symp[order(TEMP_symp$Line),]

  ## Extract allopatric combinations
  TEMP_allop <- logchange_dataset[logchange_dataset$Generation==generation&
                                    logchange_dataset$Treatment%in%fruitcomb&
                                    logchange_dataset$Fruit_s%in%fruitcomb&logchange_dataset$SA==0,]
  names(TEMP_allop)[6:ncol(TEMP_allop)] <- paste0(names(TEMP_allop)[6:ncol(TEMP_allop)], "_allop")
  ## Sort dataset
  TEMP_allop <- TEMP_allop[order(TEMP_allop$Line),]

  ## Combine datasets
  if(identical(TEMP_symp$Line, TEMP_allop$Line)){
    if(trait == "fitness"){
      TEMP <- data.frame(TEMP_symp[, c(1:4, 6:9)], TEMP_allop[, 6:9])
      TEMP$N_sumsympallop <- TEMP$N_symp + TEMP$N_allop
    }else{
      if (trait == "fecundity") {
        TEMP <- data.frame(TEMP_symp[, c(1:4, 6, 10:12)], TEMP_allop[, c(6, 10:12)])
        TEMP$N_sumsympallop <- TEMP$N_symp + TEMP$N_allop
      }else{
        if (trait == "eggtoad") {
          TEMP <- data.frame(TEMP_symp[, c(1:4, 6, 13:15)], TEMP_allop[, c(6, 13:15)])
          TEMP$N_sumsympallop <- TEMP$N_symp + TEMP$N_allop
        }else{
          print("Error: trait unknown")
        }
      }
    }

  }else{
    print("Error: sympatric and allopatric datasets are not in the same order")
  }

  return(TEMP)
}
