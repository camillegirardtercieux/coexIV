############################################################
### Clean workspace and install the research compendium ####
############################################################

# ----- clean workspace
rm(list = ls())
gc()

# ----- restart R session
.rs.restartR()

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
### Supporting information ###
##############################

#  PDF
pdf_format <- bookdown::pdf_book(toc = TRUE, toc_float = TRUE, number_sections = TRUE, includes=list(in_header=here::here("tools_latex", "header.tex"), before_body=here::here("tools_latex","doc_prefix.tex")))
rmarkdown::render(here::here("supporting_information","Supporting_information.Rmd"), output_format = pdf_format)

# html
rmarkdown::render(here::here("supporting_information","Supporting_information.Rmd"), output_format = "bookdown::html_document2")
# Upload new version on the GitHub site:
file.copy(here::here("supporting_information", "Supporting_information.html"), here::here("docs", "Supporting_information.html"), overwrite = TRUE)


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
