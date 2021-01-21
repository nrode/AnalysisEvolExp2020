#' @title Formatting a logchange dataset to pairwise dataset for testing MA-regression
#'
#' @description  Formatting a logchange dataset
#' From a logchange dataset created using computelogchange() to pairwise dataset for testing MA-regression
#'
#' @param logchange_dataset dataset with log fitness change between two phenotyping steps
#' @param generation Phenotyping step of interest
#' @param fruitcomb Pair of fruit media considered

#' @return Dataset with logchange for two fruits during a specific phenotyping step
#' @export
#'
#' @examples
#'TEMP_dataG7_CheCran <- formattinglogchange(logchange_dataset = data_logchange, generation = "7", fruitcomb=c("Cherry", "Cranberry"))

formattinglogchange <- function(logchange_dataset = data_logchange, generation="7", fruitcomb=c("Cherry", "Cranberry")){
  ## Extract sympatric combinations
  TEMP_symp <- data_logchange[data_logchange$Generation==generation&data_logchange$Treatment%in%fruitcomb&data_logchange$Fruit_s%in%fruitcomb&data_logchange$SA==1,]
  names(TEMP_symp)[6:ncol(TEMP_symp)] <- paste0(names(TEMP_symp)[6:ncol(TEMP_symp)], "_symp")
  ## Sort dataset
  TEMP_symp <- TEMP_symp[order(TEMP_symp$Line),]

  ## Extract allopatric combinations
  TEMP_allop <- data_logchange[data_logchange$Generation==generation&data_logchange$Treatment%in%fruitcomb&data_logchange$Fruit_s%in%fruitcomb&data_logchange$SA==0,]
  names(TEMP_allop)[6:ncol(TEMP_allop)] <- paste0(names(TEMP_allop)[6:ncol(TEMP_allop)], "_allop")
  ## Sort dataset
  TEMP_allop <- TEMP_allop[order(TEMP_allop$Line),]

  ## Combine datasets
  if(identical(TEMP_symp$Line, TEMP_allop$Line)){
    TEMP <- data.frame(TEMP_symp[, c(1:4, 6:ncol(TEMP_symp))], TEMP_allop[, 6:ncol(TEMP_allop)])
    TEMP$N_sumsympallop <- TEMP$N_symp + TEMP$N_allop
    return(TEMP)
  }else{
    print("Error: sympatric and allopatric datasets are not in the same order")
  }

}

