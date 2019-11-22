# Hendricks Mixtures Research Group - UC Denver
# ver 2
# Created by Ian Arriaga MacKenzie
# For use with gnomAD ancestry analysis information

library(shinyWidgets)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(dplyr)
library(waiter)

colvec = c(brewer.pal(n = 8, name = 'Set2')[3],
           brewer.pal(n = 8, name = 'Set2')[1],
           brewer.pal(n = 8, name = 'Set2')[2],
           brewer.pal(n = 8, name = 'Set2')[6],
           brewer.pal(n = 8, name = 'Set2')[5])

# load genome/exome test data
randsnpdat = read.csv('randsnpdat.txt', sep='')
bbdat = read.csv('bbdat.txt', sep='')

# load data from laptop for testing purposes, comment out on published app
# randsnpdat = read.csv('~/GitHub/mixturesresearch/AncEstV2/randsnpdat.txt', sep='')
# bbdat = read.csv('~/GitHub/mixturesresearch/AncEstV2/bbdat.txt', sep='')

