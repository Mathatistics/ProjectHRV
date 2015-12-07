## Clear Everything
rm(list = ls())

## Reading Packages
req.pkgs <- c('readxl', 'pls', 'signal', 'gridExtra', 'grid', 'MASS', 'dplyr')
invisible(ldply(req.pkgs, require, character.only = T))