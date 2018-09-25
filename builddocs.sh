#!/bin/bash

Rscript -e 'library(pkgdown); pkgdown::build_site(pkg="datrader")'
mv datrader/docs ../

