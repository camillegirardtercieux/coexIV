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


########################
### Run all analyses ###
########################

#All formats
rmarkdown::render(here::here("analyses","All_analyses.Rmd"),  output_format = "all")

#Only PDF
rmarkdown::render(here::here("analyses","All_analyses.Rmd"),  output_format = "bookdown::pdf_book")

#Only html
rmarkdown::render(here::here("analyses","All_analyses.Rmd"), output_format = "bookdown::html_document2")


##############################
### Supplementary material ###
##############################

#  PDF
pdf_format <- bookdown::pdf_book(toc = TRUE, toc_float = TRUE, number_sections = TRUE, includes=list(in_header=here::here("tools_latex", "header.tex"), before_body=here::here("tools_latex","doc_prefix.tex")))
rmarkdown::render(here::here("supplementary_materials","Supplementary_materials.Rmd"), output_format = pdf_format)

# html
rmarkdown::render(here::here("supplementary_materials","Supplementary_materials.Rmd"), output_format = "bookdown::html_document2")
# Upload new version on the GitHub site:
file.copy(here::here("supplementary_materials", "Supplementary_materials.html"), here::here("docs", "Supplementary_materials.html"), overwrite = TRUE)


#################################
### Run the theoretical model ###
#################################

#All formats
rmarkdown::render(here::here("analyses", "theoretical_model","theoretical_model.Rmd"), output_format = "all")

#Only PDF

rmarkdown::render(here::here("analyses", "theoretical_model","theoretical_model.Rmd"), output_format = "bookdown::pdf_book")

#Only html
rmarkdown::render(here::here("analyses", "theoretical_model","theoretical_model.Rmd"), output_format = "bookdown::html_document2")

#######################################
### Run the clonal dataset analysis ###
#######################################

#All formats
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "all")

#Only PDF
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "bookdown::pdf_book")

#Only html
rmarkdown::render(here::here("analyses", "clonal_analysis","clonal_analysis.Rmd"), output_format = "bookdown::html_document2")

##########################################
### Run the tropical datasets analyses ###
##########################################

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