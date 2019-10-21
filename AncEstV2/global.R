# Hendricks Mixtures Research Group - UC Denver
# Created by Ian Arriaga MacKenzie
# For use with gnomAD ancestry analysis information

library(shinyWidgets)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(dplyr)

colvec = c(brewer.pal(n = 8, name = 'Set2')[2], brewer.pal(n = 8, name = 'Set2')[3], brewer.pal(n = 8, name = 'Set2')[1], brewer.pal(n = 8, name = 'Set2')[5], brewer.pal(n = 8, name = 'Set2')[6])

# load genome/exome test data
# genome_tests_afr = read.csv('randsnpdat.txt', sep='')

# load data from laptop for testing purposes, comment out on published app
randsnpdat = read.csv('~/GitHub/mixturesresearch/AncEstV2/randsnpdat.txt', sep='')