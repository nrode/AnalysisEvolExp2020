# Create project on Github

## Create compendium
rrtools::use_compendium("/Users/rodenico/Documents/Pro/Articles/2020_EvolExp/AnalysisEvolExp2020", open = FALSE)

## Add to .gitignore
usethis::use_git_ignore(".DS_Store")
usethis::use_build_ignore(".DS_Store")
usethis::use_git(message = ":see_no_evil: Ban .DS_Store files")

## Modify DESCRIPTION file
usethis::edit_file("DESCRIPTION")
usethis::use_git(message = ":bulb: Update documentation")

## Create a R directory and a file for functions
usethis::use_r("loadfitnessdata")
usethis::use_r("loadfitnessdata_allfruits")
usethis::use_r("loadlongitudinaldata")
usethis::use_r("computelogchange")
usethis::use_r("formattinglogchange")
usethis::use_r("theme_LO_sober")
usethis::use_r("formattingplotpair")
usethis::use_r("computefitness")
usethis::use_r("computelogchange_forlongdata")
usethis::use_r("filldata_maregression") #to potentially delete
usethis::use_r("polygon_limits") #to potentially delete
usethis::use_r("bootstrap_lmodel2")
usethis::use_r("computelogchange_POOL_hypo1") #to potentially delete
usethis::use_r("computelogchange_POOL_hypo2") #to potentially delete
usethis::use_r("formatting_POOL_hypo1")
usethis::use_r("formatting_POOL_hypo2")
usethis::use_r("computecorrelation")
usethis::use_r("computeCIcordifG7G29.R")
usethis::use_r("fillout_estimate_correlation")
usethis::use_r("eq_r")
usethis::use_r("vec_minmax")


usethis::use_r("simfitnessdiff")
usethis::use_r("estim_overdisp")
usethis::use_r("sim_fitnessdata")
usethis::use_r("boot_fitnessdata")
usethis::use_r("computemeanvar_fitnessdata")
usethis::use_r("computemeanvar_fitnessdifferencedata")
usethis::use_r("bCI_tab")


usethis::use_r("computeCIcordifG29G7")

dir.create("data")
dir.create("reports")

## Update DESCRIPTION file
usethis::use_package("here")
usethis::use_package("readr")
usethis::use_package("MASS")
usethis::use_package("fitdistrplus")
usethis::use_package("ggplot2")
usethis::use_package("lme4")
usethis::use_package("Rmisc")
usethis::use_package("cowplot")
usethis::use_package("MuMIn")
usethis::use_package("lmodel2")
usethis::use_package("lemon")
usethis::use_package("data.table")
usethis::use_package("radiant.data")
usethis::use_package("sjstats")
usethis::use_package("sjPlot")
usethis::use_package("plyr")


## Update NAMESPACE file
devtools::document()

## Load all required packages
devtools::load_all()



