# coexIV

Reproductible analyses supporting the idea that a large part of intraspecific variability is due to environmental heterogeneity and does not blur species niche differences nor slow down competitive exclusion.

These analyses are associated with an article (*Rethinking the nature of intraspecific variability and its consequences on species coexistence*, Girard-Tercieux *et al.* 2021).
The Supplementary Materials are available here:
[Supplementary_materials.html](https://camillegirardtercieux.github.io/coexIV/Supplementary_materials.html). It can also be downloaded as a PDF from the GitHub repository.

**Execute and access to the code**:
In order to execute the analyses, use the make.R file. The analyses are organised in a research compendium and therefore some functions and a package name need to be loaded.
To look directly at the code, open the .Rmd files in the folder "analyses". They contain all the code for the analyses of Appendix 1 and 2, and call functions that are in the "R" and "C" folders for the analyses of Appendix 3. Each analysis can be executed separately in its own folder, or they can be executed all together with the "All_analyses.Rmd" file. Some computations are time-consuming. Therefore, in the .Rmd files, the option "eval=FALSE" enables to ignore the R chunk that launches the computation, and the results are directly loaded as .RData files.
The figures are available in png format in the folder "outputs", "analysis_x", "figures".

**Debugging**:
If there are problems to produce the PDF, try to reinstall tinytex. If the error message concerns crops, try installing texlive-extra-utils.