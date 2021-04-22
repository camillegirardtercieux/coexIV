############################################################

### Clean workspace and install the research compendium ####

############################################################

# ----- clean workspace
rm(list = ls())
gc()

# ----- install/update packages
devtools::install_deps()

# ----- install compendium package
#devtools::install(build = FALSE)

devtools::load_all()

#----- load C++ functions (maybe possible to do it with load_all() ?)
Rcpp::sourceCpp(file="C/Distance.cpp")
Rcpp::sourceCpp(file="C/Distlist.cpp")

#################################

### Run the theoretical model ###

#################################
# ----- Knit Rmd
#All formats
rmarkdown::render(here::here("analyses", "theoretical_model","theoretical_model.Rmd"), output_format = "all")

#Only PDF
# If there are problems with the PDF, try to reinstall tinytex. If the error message concerns crops, install texlive-extra-utils
rmarkdown::render(here::here("analyses", "theoretical_model","theoretical_model.Rmd"), output_format = "pdf_document")

#Only html
rmarkdown::render(here::here("analyses", "theoretical_model","theoretical_model.Rmd"), output_format = "html_document")

#######################################

### Run the clonal dataset analysis ###

#######################################
# ----- Knit Rmd
#All formats
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "all")

#Only PDF
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "pdf_document")

#Only html
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "html_document")

##########################################

### Run the tropical datasets analyses ###

##########################################
# ----- Knit Rmd
#All formats
rmarkdown::render(here::here("analyses", "tropical_analysis","tropical_analysis.Rmd"), output_format = "all")

#Only PDF
rmarkdown::render(here::here("analyses", "tropical_analysis","tropical_analysis.Rmd"), output_format = "pdf_document")

#Only html
rmarkdown::render(here::here("analyses", "tropical_analysis","tropical_analysis.Rmd"), output_format = "html_document")
