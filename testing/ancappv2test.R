## anc est shiny app v2
## ian arrmack

library(shinyWidgets)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(scales)
library(gridExtra)
library(dplyr)

colvec = c(brewer.pal(n = 8, name = 'Set2')[2], brewer.pal(n = 8, name = 'Set2')[3], brewer.pal(n = 8, name = 'Set2')[1], brewer.pal(n = 8, name = 'Set2')[5], brewer.pal(n = 8, name = 'Set2')[6])

genome_tests_afr <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/genome_test_afr.txt", sep="")
genome_tests_amr <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/genome_test_amr.txt", sep="")
genome_tests_oth <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/genome_test_oth.txt", sep="")
exome_tests_afr <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/exome_test_afr.txt", sep="")
exome_tests_amr <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/exome_test_amr.txt", sep="")
exome_tests_oth <- read.csv("~/GitHub/mixturesresearch/AncestryEstimation-App/exome_test_oth.txt", sep="")

genome_tests_afr$exomegenome = rep('genome', 10022); genome_tests_afr$gnomadancestry = rep('afr', 10022)
genome_tests_amr$exomegenome = rep('genome', 10022); genome_tests_amr$gnomadancestry = rep('amr', 10022)
genome_tests_oth$exomegenome = rep('genome', 10022); genome_tests_oth$gnomadancestry = rep('oth', 10022)

exome_tests_afr$exomegenome = rep('exome', 9022); exome_tests_afr$gnomadancestry = rep('afr', 9022)
exome_tests_amr$exomegenome = rep('exome', 9022); exome_tests_amr$gnomadancestry = rep('amr', 9022)
exome_tests_oth$exomegenome = rep('exome', 9022); exome_tests_oth$gnomadancestry = rep('oth', 9022)

randsnpdat = rbind(genome_tests_afr, genome_tests_amr, genome_tests_oth, exome_tests_afr, exome_tests_amr, exome_tests_oth)

tdat = randsnpdat %>%
  filter(exomegenome == 'genome') %>% 
  filter(gnomadancestry == 'afr') %>% 
  filter(Number_SNPs == 1000) %>% 
  select(EUR, AFR, SAS, EAS, NAM)

randmeltdat <- melt(tdat)
names(randmeltdat) = c('Ancestry', 'Proportion')

randplot = ggplot(randmeltdat, aes(x=Proportion, fill = Ancestry)) +
  geom_histogram(bins = 400)+
  facet_grid(Ancestry ~ .)+
  scale_fill_manual(values = colvec)+
  guides(fill = FALSE)+
  scale_x_continuous(breaks = c(0,.25,.50,.75,1),
                     limits = c(-0.01,1)) +
  theme(axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.y = element_text(size = 15),
        axis.ticks = element_blank())
plot(randplot)

testpl = ggplot(randmeltdat, aes(x = Proportion, fill = Ancestry)) +
  geom_density() +
  facet_grid(cols = vars(Ancestry))
plot(testpl)

plot_list = list()
anc_list = c('EUR', 'AFR', 'EAS', 'SAS', 'NAM')

ql1 = c(quantile(tdat[[anc_list[1]]], probs = c(0.025, .975)))
pl1 = ggplot(data = tdat, aes(x = AFR)) +
  geom_density(alpha = mean(tdat$AFR), fill = brewer.pal(n = 8, name = 'Set2')[3]) +
  geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
  geom_vline(aes(xintercept = ql1[1]), linetype ="longdash", size = .8) +
  geom_vline(aes(xintercept = ql1[2]), linetype ="longdash", size = .8) +
  theme_minimal() + 
  labs(title = 'African Proportion Estimate', x = NULL, y = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = .01), 
                     breaks = c(ql1[1], mean(tdat$AFR), ql1[2])) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_blank())


ql1 = c(quantile(tdat$AFR, probs = c(0.025, .975)))
pl1 = ggplot(data = tdat, aes(x = AFR)) +
  geom_density(alpha = mean(tdat$AFR), fill = brewer.pal(n = 8, name = 'Set2')[3]) +
  geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
  geom_vline(aes(xintercept = ql1[1]), linetype ="longdash", size = .8) +
  geom_vline(aes(xintercept = ql1[2]), linetype ="longdash", size = .8) +
  theme_minimal() + 
  labs(title = 'African Proportion Estimate', x = NULL, y = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = .01), 
                     breaks = c(ql1[1], mean(tdat$AFR), ql1[2])) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_blank())
plot(pl1)

ql2 = c(quantile(tdat$EUR, probs = c(0.025, .975)))
pl2 = ggplot(data = tdat, aes(x = EUR)) +
  geom_density(alpha = mean(tdat$EUR), fill = brewer.pal(n = 8, name = 'Set2')[2]) +
  geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
  geom_vline(aes(xintercept = ql2[1]), linetype ="longdash", size = .8) +
  geom_vline(aes(xintercept = ql2[2]), linetype ="longdash", size = .8) +
  theme_minimal() + 
  labs(title = 'European Proportion Estimate', x = NULL, y = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = .01), 
                     breaks = c(ql2[1], mean(tdat$EUR), ql2[2])) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_blank())

grid.arrange(plot_list[[1]], plot_list[[2]], ncol = 2)
