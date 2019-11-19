library(shinyWidgets)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(scales)
library(gridExtra)

# Hendricks Mixtures Research Group - UC Denver
# Created by Ian Arriaga MacKenzie
# For use with gnomAD ancestry analysis information

colvec = c(brewer.pal(n = 8, name = 'Set2')[2], brewer.pal(n = 8, name = 'Set2')[3], brewer.pal(n = 8, name = 'Set2')[1], brewer.pal(n = 8, name = 'Set2')[5], brewer.pal(n = 8, name = 'Set2')[6])

# load genome/exome test data
# genome_tests_afr = read.csv('genome_test_afr.txt', sep='')
# genome_tests_amr = read.csv('genome_test_amr.txt', sep='')
# genome_tests_oth = read.csv('genome_test_oth.txt', sep='')
# exome_tests_afr = read.csv('exome_test_afr.txt', sep='')
# exome_tests_amr = read.csv('exome_test_amr.txt', sep='')
# exome_tests_oth = read.csv('exome_test_oth.txt', sep='')

# load data from laptop for testing purposes, comment out on published app
genome_tests_afr <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/genome_test_afr.txt", sep="")
genome_tests_amr <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/genome_test_amr.txt", sep="")
genome_tests_oth <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/genome_test_oth.txt", sep="")
exome_tests_afr <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/exome_test_afr.txt", sep="")
exome_tests_amr <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/exome_test_amr.txt", sep="")
exome_tests_oth <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/exome_test_oth.txt", sep="")

