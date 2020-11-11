# Create project on Github

## Create compendium
rrtools::use_compendium("/Users/rodenico/Documents/Pro/Articles/2020_EvolExp/AnalysisEvolExp2020", open = FALSE)

## Modify DESCRIPTION file

## Create a R directory and a file for functions
usethis::use_r("loadfitnessdata")
usethis::use_r("simfitnessdiff")
usethis::use_r("estim_overdisp")



dir.create("data")
dir.create("reports")

## Update DESCRIPTION file
usethis::use_package("here")
usethis::use_package("readr")
usethis::use_package("MASS")
usethis::use_package("fitdistrplus")

## Update NAMESPACE file
devtools::document()

## Load all required packages
devtools::load_all()



