# Markdown and HTML generation 
rmarkdown::render("R/analysis_pop-comp.Rmd", 
                  output_dir = "reports/")
rmarkdown::render("R/analysis_inno-predict.Rmd", 
                  output_dir = "reports/")
rmarkdown::render("R/analysis-summary.Rmd", 
                  output_dir = "reports/")