#!/bin/bash

cd datrader
Rscript -e 'library(rmarkdown); rmarkdown::render("README.Rmd")'
Rscript -e 'library(pkgdown); pkgdown::build_site(pkg=".")'
mv README-*.png ./docs
cd ../
rm -rf docs
mv datrader/docs .

