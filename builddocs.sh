#!/bin/bash

Rscript -e 'library(pkgdown); pkgdown::build_site(pkg="datrader")'
rm -rf docs
mv datrader/docs .

