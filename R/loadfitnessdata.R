#' @title Fitness Data Import Function
#'
#' @description Reads a fitness data file in table format and subset it to return the dataframe with the right generation
#' @param dataset Fitness dataset
#' @param generation Generation considered for the subset
#'
#' @return
#' @export
#'
#' @examples
#'data_G29 <- loadfitnessdata(dataset = "PERFORMANCE_Comptage_adultes_G13G14G15G16G17G18G19G20G21G22G23G24G25G26G27G28G29.csv", generation = "29")

loadfitnessdata <- function(dataset = "Selection_Phenotypage_G0_G7_G8.csv", generation = "G1"){

  ## Path using the here function so that the command line is reproducible across platforms
  data_complet <- read.table(file=here::here("data", dataset), sep=";", header=TRUE)

  ## Take the right generation (G1, G7 or )
  data <- subset(data_complet, Generation == generation)

  if(generation!=29){

    ## Update variables
    data$Nb_adults <- as.numeric(as.character(data$Nb_adults))
    data$Nb_eggs <- as.numeric(as.character(data$Nb_eggs))

  }



  ## Subset dataset
  data <- data[data$Treatment=="Strawberry"|
                 data$Treatment=="Cherry"|
                 data$Treatment=="Cranberry",]

  ## Keep only some columns
  data <- data[, c("Treatment", "Line", "Fruit_s", "Nb_eggs", "Nb_adults")]

  ## Create the SA variable
  data$SA <- as.factor(ifelse(as.character(data$Treatment)==as.character(data$Fruit_s),1,0))

  ## Compute emergence rate
  data$Emergence_rate <- data$Nb_adults/data$Nb_eggs
  data$Treatment <- factor(data$Treatment)
  data$Line <- factor(data$Line)
  data <- droplevels(data)


  return(data)
}
