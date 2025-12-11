#!/usr/bin/env bash

Rscript -e "rmarkdown::render('exploratory_data_analysis.Rmd')"
Rscript -e "rmarkdown::render('data_analysis.Rmd')"