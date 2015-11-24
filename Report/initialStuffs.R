## Clear Everything
rm(list = ls())

## Setting up working directory
wd <- "/Users/rajurim/Dropbox (NMBU)/ProjectHRV/Report"
if (getwd() != wd)
  setwd(wd)

## Reading Packages
req.pkgs <- c('readxl', 'pls', 'signal', 'gridExtra', 'grid', 'MASS')
invisible(ldply(req.pkgs, require, character.only = T))