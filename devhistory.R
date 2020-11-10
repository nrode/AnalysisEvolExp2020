
## Create compendium
rrtools::use_compendium("/Users/rodenico/Documents/Pro/Articles/2020_EvolExp/AnalysisEvolExp2020", open = FALSE)

## Create a R directory and a file for functions
usethis::use_r("loadfitnessdata")
usethis::use_r("simfitnessdiff")

dir.create("data")

## Update DESCRIPTION file
usethis::use_package("here")
usethis::use_package("readr")
