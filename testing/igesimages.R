library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(gridExtra)

data = read.csv("C:/Users/iansa/OneDrive/Desktop/poster/blockboot_afr.csv")

data = read.csv("C:/Users/iansa/OneDrive/Desktop/poster/blockboot_amr.csv"); data = data[,2:9]

data = read.csv("C:/Users/iansa/OneDrive/Desktop/poster/blockboot_oth.csv")

display.brewer.pal(n = 8, name = 'Set2')
brewer.pal(n = 8, name = 'Set2')

c(min(data$african), mean(data$african), max(data$african))

{
  ql1 = c(quantile(data$african, probs = c(0.025, .975)))
  pl1 = ggplot(data = data, aes(x = african)) +
    geom_density(alpha = mean(data$african), fill = brewer.pal(n = 8, name = 'Set2')[3]) +
    geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
    geom_vline(aes(xintercept = ql1[1]), linetype ="longdash", size = .8) +
    geom_vline(aes(xintercept = ql1[2]), linetype ="longdash", size = .8) +
    theme_minimal() + 
    labs(title = 'African Proportion Estimate', x = NULL, y = NULL) +
    scale_x_continuous(labels = percent_format(accuracy = .01), 
                       breaks = c(ql1[1], mean(data$african), ql1[2])) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.text.x = element_text(face="bold", size=15),
          axis.text.y = element_blank())
  
  ql2 = c(quantile(data$european, probs = c(0.025, .975)))
  pl2 = ggplot(data = data, aes(x = european)) +
    geom_density(alpha = mean(data$european), fill = brewer.pal(n = 8, name = 'Set2')[2]) +
    geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
    geom_vline(aes(xintercept = ql2[1]), linetype ="longdash", size = .8) +
    geom_vline(aes(xintercept = ql2[2]), linetype ="longdash", size = .8) +
    theme_minimal() + 
    labs(title = 'European Proportion Estimate', x = NULL, y = NULL) +
    scale_x_continuous(labels = percent_format(accuracy = .01), 
                       breaks = c(ql2[1], mean(data$european), ql2[2])) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.text.x = element_text(face="bold", size=15),
          axis.text.y = element_blank())
  
  ql3 = c(quantile(data$south_asian, probs = c(0.025, .975)))
  pl3 = ggplot(data = data, aes(x = south_asian)) +
    geom_density(alpha = mean(data$south_asian), fill = brewer.pal(n = 8, name = 'Set2')[1]) +
    geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
    geom_vline(aes(xintercept = ql3[1]), linetype ="longdash", size = .8) +
    geom_vline(aes(xintercept = ql3[2]), linetype ="longdash", size = .8) +
    theme_minimal() + 
    labs(title = 'South Asian Proportion Estimate', x = NULL, y = NULL) +
    scale_x_continuous(labels = percent_format(accuracy = .01), 
                       breaks = c(ql3[1], mean(data$south_asian), ql3[2])) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.text.x = element_text(face="bold", size=15),
          axis.text.y = element_blank())
  
  ql4 = c(quantile(data$east_asian, probs = c(0.025, .975)))
  pl4 = ggplot(data = data, aes(x = east_asian)) +
    geom_density(alpha = mean(data$east_asian), fill = brewer.pal(n = 8, name = 'Set2')[5]) +
    geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
    geom_vline(aes(xintercept = ql4[1]), linetype ="longdash", size = .8) +
    geom_vline(aes(xintercept = ql4[2]), linetype ="longdash", size = .8) +
    theme_minimal() + 
    labs(title = 'East Asian Proportion Estimate', x = NULL, y = NULL) +
    scale_x_continuous(labels = percent_format(accuracy = .01), 
                       breaks = c(ql4[1], mean(data$east_asian), ql4[2])) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.text.x = element_text(face="bold", size=15),
          axis.text.y = element_blank())
  
  ql5 = c(quantile(data$native_american, probs = c(0.025, .975)))
  pl5 = ggplot(data = data, aes(x = native_american)) +
    geom_density(alpha = mean(data$native_american), fill = brewer.pal(n = 8, name = 'Set2')[6]) +
    geom_histogram(aes(y = ..density..), color = 'black', fill = NA) +
    geom_vline(aes(xintercept = ql5[1]), linetype ="longdash", size = .8) +
    geom_vline(aes(xintercept = ql5[2]), linetype ="longdash", size = .8) +
    theme_minimal() + 
    labs(title = 'Native American Proportion Estimate', x = NULL, y = NULL) +
    scale_x_continuous(labels = percent_format(accuracy = .01), 
                       breaks = c(ql5[1], mean(data$native_american), ql5[2])) +
    theme(plot.title = element_text(hjust = 0.5, size = 15),
          axis.text.x = element_text(face="bold", size=15),
          axis.text.y = element_blank())
  
  grid.arrange(pl1, pl2, pl3, pl4, pl5, ncol = 5)
}




AFREUR = read.csv("C:/Users/iansa/OneDrive/Desktop/poster/Merged_AFR_EAS.csv")



### Python Bias
HAplot = ggplot(AFREUR, aes(x=AFREUR$True.afr, y=AFREUR$accuracy.HA.eas)) +
  geom_point(aes(alpha=1/100),color= brewer.pal(n = 8, name = 'Set2')[3], size=1, stroke=0.0000001, show.legend = FALSE) +
  geom_smooth(method = "auto", se=TRUE, linetype="dashed",color="black", size=.5 ) +
  geom_hline(yintercept = 0, size=.2, color="black") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(face="bold", size=20),
        axis.text.y = element_text(face="bold", size=20),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=15))
print(HAplot +
        labs(title="African Proportion vs. Python African Bias",
             y= "Python African Estimate - True Proportion",
             x= "True African Mixing Proportion")) +
        scale_y_continuous(labels = percent_format(accuracy = .01), limits = c(-.001, .001))

###R Bias 
HAplot = ggplot(AFREUR, aes(x=AFREUR$True.afr, y=AFREUR$accuracy.R.afr)) +
  geom_point(aes(alpha=1/100),color= brewer.pal(n = 8, name = 'Set2')[3], size=1, stroke=0.0000001, show.legend = FALSE) +
  geom_smooth(method = "auto", se=TRUE, linetype="dashed",color="black", size=.5 ) +
  geom_hline(yintercept = 0, size=.2, color="black") +
  theme_minimal() +
  theme(plot.title = element_blank(),
        axis.text.x = element_text(face="bold", size=20),
        axis.text.y = element_text(face="bold", size=20),
        axis.title.x = element_text(face="bold", size=20),
        axis.title.y = element_text(face="bold", size=15))
print(HAplot +
        labs(title= NA,
             y= "African Estimate - True Proportion",
             x= "True African Mixture Proportion")) +
  scale_y_continuous(labels = percent_format(accuracy = .01), limits = c(-.001, .001))+
  scale_x_continuous(labels = percent_format(accuracy = 1))



