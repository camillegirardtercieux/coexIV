library(usethis)

#Create Description file
usethis::use_description()

#Generate the documentation
devtools::document()

#Create NAMESPACE file
usethis::use_namespace()

#Then modify the description file

#to use packages
usethis::use_package('dplyr')
usethis::use_package('tidyr')
usethis::use_package('readr')
usethis::use_package('knitr')
usethis::use_package('stringr')
usethis::use_package('bazar')
usethis::use_package('lubridate')
usethis::use_package('ggplot2')
usethis::use_package('here')
usethis::use_package('glue')
usethis::use_package('sp')
usethis::use_package('gstat')
usethis::use_package('mgcv')
usethis::use_package('Rcpp')
usethis::use_package('RcppAlgos')
usethis::use_package('RcppArmadillo')
usethis::use_package('parallel')
usethis::use_package('brms')
usethis::use_package('bayesplot')
usethis::use_package('kableExtra')
usethis::use_package('mvtnorm')
usethis::use_package('coda')
usethis::use_package('ape')
usethis::use_package('ggnewscale')
usethis::use_package('viridis')
usethis::use_package('ggpubr')

#Create folders and script files
#The first use_r function creates the R folder automatically

file.create(here::here('make.R'))

dir.create("data")
dir.create(here::here("data", "EUCFLUX"))
dir.create(here::here("data", "Paracou"))
dir.create(here::here("data", "Uppangala"))
dir.create(here::here("data", "BCI"))

dir.create("outputs")
dir.create(here::here("outputs", "theoretical_model"))
dir.create(here::here("outputs", "clonal_analysis"))
dir.create(here::here("outputs", "tropical_analysis"))
dir.create(here::here("outputs", "theoretical_model", "figures"))
dir.create(here::here("outputs", "clonal_analysis", "figures"))
dir.create(here::here("outputs", "tropical_analysis", "figures"))
dir.create(here::here("outputs", "tropical_analysis", "Dists_Paracou"))
dir.create(here::here("outputs", "tropical_analysis", "Dists_Uppangala"))
dir.create(here::here("outputs", "tropical_analysis", "Dists_BCI"))

dir.create("C")
file.create(here::here("C", "Distance.cpp"))
file.create(here::here("C", "Distlist.cpp"))

dir.create("analyses")
dir.create(here::here("analyses", "theoretical_model"))
dir.create(here::here("analyses", "clonal_analysis"))
dir.create(here::here("analyses", "tropical_analysis"))
file.create(here::here("analyses", "theoretical_model", "theoretical_model.Rmd"))
file.create(here::here("analyses", "clonal_analysis", "clonal_analysis.Rmd"))
file.create(here::here("analyses", "tropical_analysis", "tropical_analysis.Rmd"))

usethis::use_r(name="functions_Moran.R")
usethis::use_r(name="functions_data_manip.R")
usethis::use_r(name="functions_variance_comparison.R")
usethis::use_r(name="functions_abundance_diagrams.R")
usethis::use_r(name="functions_stan.R")

#to get the pipe (writes a file in the R directory)
usethis::use_pipe()


#Update the documentation
devtools::document()

#Modify gitignore file
# Results
#usethis::use_git_ignore("*outputs")
usethis::use_git_ignore(".RData")
usethis::use_git_ignore(".txt")
# Data
usethis::use_git_ignore("*data/")
# Result files
#usethis::use_git_ignore(".html")
#usethis::use_git_ignore(".pdf")
#usethis::use_git_ignore(".docx")