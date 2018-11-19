# Makefile for Data Cleaning

## raw : Download raw data
raw :
	curl -o raw-data/2018-New-Coders-Survey.csv \
		https://files.gitter.im/FreeCodeCamp/DataScience/gR37/2018-New-Coders-Survey.csv

## eda : Run exploratory data analysis on raw data
eda :
	mkdir -p docs
	Rscript -e 'rmarkdown::render(here::here("reports", "explore-data.Rmd"), output_file = here::here("docs", "explore-data.html"))'

## clean : Clean and format raw data
clean :
	Rscript src/clean-data-2018.R

## readme : Compile RMarkdown README
readme :
	Rscript -e 'rmarkdown::render(here::here("README.Rmd"), output_file = here::here("README.md"))'
	rm README.html

## tree : Create repository structure for README
tree :
	tree >> README.Rmd

## help : Help page for Makefile
help :
	@echo ""
	@echo "Usage:"
	@echo -e "\tmake <target>\n"
	@echo -e "Target\t\tDescription"
	@echo -e "------\t\t-----------"
	@grep '## [A-Za-z]* : [A-Za-z]*' $(MAKEFILE_LIST) | sed 's/## //' | sed 's/: /\t\t/'

.DEFAULT_GOAL := help
