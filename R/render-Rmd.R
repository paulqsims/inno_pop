################################################################################
# Purpose: Markdown and HTML generation for Rmd files
# Author: Paul Q. Sims
# Contact: paul.q.sims@gmail.com
# Date: 2020
################################################################################

# Readme - shows that the repository has been updated
rmarkdown::render("readme.Rmd", 
                  output_format = c("html_document", "github_document"))

# Data readme 
rmarkdown::render("data/data_readme.Rmd", output_dir = "data/",
                  output_format = c("html_document", "github_document"))

# Analyses and figures
docs_to_render <- 
  c("R/analysis_pop-comp.Rmd", "R/analysis_inno-predict.Rmd",
                    "R/analysis-summary.Rmd", "R/figures.Rmd") 

for (i in seq_along(docs_to_render)) {
  rmarkdown::render(docs_to_render[i], output_dir = "reports/",
                    output_format = c("html_document",
                                      "github_document"))
}
