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

data1 = read.csv("C:/Users/iansa/OneDrive/Desktop/poster/blockboot_afr.csv")
data1$gnomadanc = rep('afr', 1000)
data2 = read.csv("C:/Users/iansa/OneDrive/Desktop/poster/blockboot_amr.csv"); data2 = data2[,2:9]
data2$gnomadanc = rep('amr', 1000)
data3 = read.csv("C:/Users/iansa/OneDrive/Desktop/poster/blockboot_oth.csv")
data3$gnomadanc = rep('oth', 1000)
bbdat = rbind(data1, data2, data3)

bb = bbdat %>% 
  filter(gnomadanc == 'amr') %>% 
  select(european, african, south_asian, east_asian, native_american)
bbsum = bb %>% 
  summarise_all(mean)
bbsum = melt(bbsum)

bbplot = ggplot(bbsum, aes(x = variable, y = value)) +
  geom_col(fill = colvec) +
  scale_y_continuous(breaks = c(0,.25,.50,.75,1),
                     limits = c(0,1))
  
plot(bbplot)


randsnpdat = rbind(genome_tests_afr, genome_tests_amr, genome_tests_oth, exome_tests_afr, exome_tests_amr, exome_tests_oth)

write.table(randsnpdat, file = 'randsnpdat.txt')

randsnpdat = read.csv("~/GitHub/mixturesresearch/testing/randsnpdat.txt", sep="")
colvec = c(brewer.pal(n = 8, name = 'Set2')[2],
           brewer.pal(n = 8, name = 'Set2')[3],
           brewer.pal(n = 8, name = 'Set2')[1],
           brewer.pal(n = 8, name = 'Set2')[5],
           brewer.pal(n = 8, name = 'Set2')[6])

tdat = randsnpdat %>%
  filter(exomegenome == 'genome') %>% 
  filter(gnomadancestry == 'amr') %>% 
  filter(Number_SNPs == 1000) %>% 
  select(EUR, AFR, SAS, EAS, NAM)

randmeltdat <- melt(tdat)
names(randmeltdat) = c('Ancestry', 'Proportion')

randplot = ggplot(randmeltdat, aes(x=Proportion, fill = Ancestry)) +
  geom_histogram(bins = 400)+
  geom_density(fill = NA)+
  facet_grid(Ancestry ~ .)+
  scale_fill_manual(values = colvec)+
  guides(fill = FALSE)+
  scale_x_continuous(breaks = c(0,.25,.50,.75,1),
                     limits = c(-0.01,1)) +
  theme(axis.text.x = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text.y = element_text(size = 15),
        axis.ticks = element_blank())


plot_list = list()
anc_list = c('EUR', 'AFR', 'SAS', 'EAS', 'NAM')


for (i in 1:5){
  pl = ggplot(data = tdat, aes_string(x = anc_list[[i]])) +
    geom_density(fill = NA) +
    geom_histogram(aes(y = ..density..), color = 'black', fill = colvec[i], alpha = (mean(tdat[[anc_list[i]]])), bins = 30) +
    geom_vline(data = tdat, xintercept = quantile(tdat[[anc_list[i]]], probs = 0.025), linetype ="longdash", size = .8) +
    geom_vline(data = tdat, xintercept = quantile(tdat[[anc_list[i]]], probs = 0.975), linetype ="longdash", size = .8) +
    theme_minimal() + 
    labs(title = paste(anc_list[i], 'prop est', sep = ' '), x = NULL, y = NULL) +
    scale_x_continuous(labels = percent_format(accuracy = .01), 
                       breaks = c(quantile(tdat[[anc_list[i]]], probs = 0.025), mean(tdat[[anc_list[i]]]), quantile(tdat[[anc_list[i]]], probs = 0.975))) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.text.x = element_text(face="bold", size=15),
          axis.text.y = element_blank())
  plot_list[[i]] = pl
}

plot_list[[6]] = randplot

grid.arrange(
  grobs = plot_list,
  widths = c(1,1,1,1,1),
  heights = c(2,1),
  layout_matrix = rbind(c(6,6,6,6,6),
                        c(1,2,3,4,5))
)

grid.arrange(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], plot_list[[5]], ncol = 5)


plot(plot_list[[5]])

ql1 = c(quantile(tdat[[anc_list[1]]], probs = c(0.025, .975)))
pl1 = ggplot(data = tdat, aes_string(x = anc_list[[1]])) +
  geom_density(alpha = mean(tdat[[anc_list[1]]]), fill = colvec[1]) +
  geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
  geom_vline(aes(xintercept = ql1[1]), linetype ="longdash", size = .8) +
  geom_vline(aes(xintercept = ql1[2]), linetype ="longdash", size = .8) +
  theme_minimal() + 
  labs(title = 'African Proportion Estimate', x = NULL, y = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = .01), 
                     breaks = c(ql1[1], mean(tdat[[anc_list[1]]]), ql1[2])) +
  theme(plot.title = element_text(hjust = 0.5, size = 15),
        axis.text.x = element_text(face="bold", size=15),
        axis.text.y = element_blank())
plot(pl1)


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
