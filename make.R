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

#----- load C++ functions
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
rmarkdown::render(here::here("analyses", "theoretical_model","theoretical_model.Rmd"), output_format = "bookdown::pdf_book")

#Only html
rmarkdown::render(here::here("analyses", "theoretical_model","theoretical_model.Rmd"), output_format = "bookdown::html_document2")

#######################################

### Run the clonal dataset analysis ###

#######################################
# ----- Knit Rmd
#All formats
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "all")

#Only PDF
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "bookdown::pdf_book")

#Only html
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "bookdown::html_document2")

##########################################

### Run the tropical datasets analyses ###

##########################################
# ----- Knit Rmd
#All formats
rmarkdown::render(here::here("analyses", "tropical_analysis","tropical_analysis.Rmd"), output_format = "all")

#Only PDF
rmarkdown::render(here::here("analyses", "tropical_analysis","tropical_analysis.Rmd"), output_format = "bookdown::pdf_book")

 #Only html
rmarkdown::render(here::here("analyses", "tropical_analysis","tropical_analysis.Rmd"), output_format = "bookdown::html_document2")

###########################
### Figures and scripts ###
###########################

rmarkdown::render(here::here("analyses","body_of_evidence.Rmd"), output_format = "bookdown::html_document2")

rmarkdown::render(here::here("analyses","Script_figure_1.Rmd"), output_format = "bookdown::html_document2")

##########################
### Full supplementary ###
##########################

pdf_format <- bookdown::pdf_book(toc = TRUE, toc_float = TRUE, number_sections = TRUE, includes=list(in_header=here::here("header.tex"), before_body=here::here("doc_prefix.tex")))

rmarkdown::render(here::here("analyses","full_supplementary.Rmd"), output_format = pdf_format)

rmarkdown::render(here::here("analyses","full_supplementary.Rmd"), output_format = "bookdown::html_document2")

##########################
### New version online ###
##########################

file.copy(here::here("analyses", "theoretical_model", "theoretical_model.html"), here::here("docs", "theoretical_model.html"), overwrite = TRUE)
file.copy(here::here("analyses", "clonal_analysis", "clonal_analysis.html"), here::here("docs", "clonal_analysis.html"), overwrite = TRUE)
file.copy(here::here("analyses", "tropical_analysis", "tropical_analysis.html"), here::here("docs", "tropical_analysis.html"), overwrite = TRUE)
file.copy(here::here("analyses", "body_of_evidence.html"), here::here("docs", "body_of_evidence.html"), overwrite = TRUE)
file.copy(here::here("analyses", "Script_figure_1.html"), here::here("docs", "Script_figure_1.html"), overwrite = TRUE)
file.copy(here::here("analyses", "full_supplementary.html"), here::here("docs", "full_supplementary.html"), overwrite = TRUE)
