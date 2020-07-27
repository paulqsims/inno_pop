################################################################################
# Purpose: Markdown and HTML generation for Rmd files
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
################################################################################

# Data readme 
rmarkdown::render("data/data_readme.Rmd", output_dir = "data/",
                  output_format = c("html_document", "github_document"))

# Analyses and figures
rmarkdown::render("R/analysis_pop-comp.Rmd", output_dir = "reports/",
                  output_format = c("html_document", "github_document"))
rmarkdown::render("R/analysis_inno-predict.Rmd", output_dir = "reports/",
                  output_format = c("html_document", "github_document"))
rmarkdown::render("R/analysis-summary.Rmd", output_dir = "reports/",
                  output_format = c("html_document", "github_document"))
rmarkdown::render("R/figures.Rmd", output_dir = "reports/",
                  output_format = c("html_document", "github_document"))