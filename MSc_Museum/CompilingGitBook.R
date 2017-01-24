# WORKS IN TERMINAL, NOT RSTUDIO OR RGUI

r
setwd("~/Projects/TeachingMaterials/MSc_Museum/MacroModule")
bookdown::render_book("index.Rmd")
bookdown::render_book("index.Rmd", "bookdown::gitbook") # HTML version
bookdown::render_book("index.Rmd", "bookdown::pdf_book") # PDF version