#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
loadfitnessfitnessdata <- function(){

  ## Path using the here function so that the command line is reproducible across platforms
  data_complet <- read.table(file=here::here("data","Selection_Phenotypage_G0_G7_G8.csv"), sep=";", header=T)

  data_complet <- data_complet[data_complet$Traitement=="Strawberry"|
                                 data_complet$Traitement=="Cherry"|
                                 data_complet$Traitement=="Cranberry",]
  data_G0 <- subset(data_complet, Generation == "G1")

  data_G0$Nb_adultes<-as.numeric(as.character(data_G0$Nb_adultes))

  names(data_G0)[names(data_G0) == "Nb_adultes"] <- "Nb_adults"
  names(data_G0)[names(data_G0) == "Nb_oeufs"] <- "Nb_eggs"
  names(data_G0)[names(data_G0) == "Fruit_S"] <- "Fruit_s"
  names(data_G0)[names(data_G0) == "Traitement"] <- "Treatment"
  data_G0$Treatment <- factor(data_G0$Treatment)
  return(data_G0)
}
