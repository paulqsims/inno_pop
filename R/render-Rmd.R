# Markdown and HTML generation 
rmarkdown::render("R/analysis_pop-comp.Rmd", output_dir = "reports/",
                  output_format = c("html_document", "github_document"))
rmarkdown::render("R/analysis_inno-predict.Rmd", output_dir = "reports/",
                  output_format = c("html_document", "github_document"))
rmarkdown::render("R/analysis-summary.Rmd", output_dir = "reports/",
                  output_format = c("html_document", "github_document"))
rmarkdown::render("R/figures.Rmd", output_dir = "reports/",
                  output_format = c("html_document", "github_document"))