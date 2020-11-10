# Create project on Github

## Create compendium
rrtools::use_compendium("/Users/rodenico/Documents/Pro/Articles/2020_EvolExp/AnalysisEvolExp2020", open = FALSE)

## Modify DESCRIPTION file

## Create a R directory and a file for functions
usethis::use_r("loadfitnessdata")
usethis::use_r("simfitnessdiff")

dir.create("data")

## Update DESCRIPTION file
usethis::use_package("here")
usethis::use_package("readr")

## Update NAMESPACE file
devtools::document()

## Load all required packages
devtools::load_all()


data_G0 <- loadfitnessdata(dataset = "Selection_Phenotypage_G0_G7_G8.csv", generation = "G1")
data_G7 <- loadfitnessdata(dataset = "Selection_Phenotypage_G0_G7_G8.csv", generation = "G7")
data_G29 <- loadfitnessdata(dataset = "PERFORMANCE_Comptage_adultes_G13G14G15G16G17G18G19G20G21G22G23G24G25G26G27G28G29.csv", generation = "29")

head(data_G0)
head(data_G7)
head(data_G29)
