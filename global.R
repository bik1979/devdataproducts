library(shiny)
suppressPackageStartupMessages(library(googleVis))
#source("plots.R")
print(getwd())
numbers.year <- read.table("numbers_year.txt", header = T)
stars.year <- read.table("stars_year.txt", header = T)
draws.year <- list("2004" = 47, "2005" = 52, "2006" = 52, "2007" = 52,
				   "2008" = 52, "2009" = 52, "2010" = 53, "2011" = 86,
				   "2012" = 104, "2013" = 105, "2014" = 73)
draws.total <- 728