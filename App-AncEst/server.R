#server.R
library(shiny)

server = function(input, output) {
  
  refdat = genome_tests_afr
  randchrdat = reactiveValues(chr_dat = NULL, rand_dat = NULL)
  
  output$authorid1 = renderText({
  'Hendricks Mixtures Research Group'
  })
  output$authorid2 = renderText({
  'University of Colorado Denver'
  })
  
  observeEvent({
    input$ancdat
    input$exge}, {
      temprefdat = get(paste(input$exge,'_tests_',tolower(input$ancdat), sep = ''))
      randchrdat$chr_dat = temprefdat[1:22,]
      randchrdat$rand_dat = temprefdat[23:10022,]
    }
  )
  
  output$mainPlot = renderPlot({
    
    if (input$chrrand == 'randsnp'){
      
      randdat = randchrdat$rand_dat
      
      indices = which(randdat$Number_SNPs == input$randsnpnum)
      
      randcleandat = randdat[(indices[1]):tail(indices, n = 1),4:8]
      names(randcleandat) = c('European', 'African', 'South Asian', 'East Asian', 'Native American')
      
      randmeltdat <- melt(randcleandat)
      names(randmeltdat) = c('Ancestry', 'Proportion')
      
      randplot = ggplot(randmeltdat, aes(x=Proportion, fill = Ancestry)) +
        geom_histogram(bins = 400)+
        xlim(c(0,1))+
        facet_grid(Ancestry ~ .)+
        guides(fill = FALSE)
      
      return(randplot)
      
    }
    
    if (input$chrrand == 'chr'){
      
      chrdat = randchrdat$chr_dat
      
      chrdat1 = chrdat[,1]; chrdat2 = chrdat[,4:8]
      chrdat = cbind(chrdat1, chrdat2)
      names(chrdat) = c('Chromosome', 'EUR', 'AFR', 'SAS', 'EAS', 'NAM')
      
      chrminmax = input$chrval
      
      chrdat = chrdat[chrminmax[1]:chrminmax[2],]
      
      chrmelt<- melt(chrdat, id="Chromosome", 
                         measure=c('EUR', 'AFR', 'SAS', 'EAS', 'NAM'), 
                         variable.name="Anc", value.name="Proportions")
      
      chrplot = ggplot(chrmelt, aes(Chromosome, Proportions, fill=Proportions)) + 
        facet_wrap( ~ Anc ,nrow = 1) +
        geom_bar(stat="identity") +
        ylim(c(0,1)) +
        coord_flip() +
        scale_fill_distiller(palette = 'Spectral')+
        guides(fill = FALSE)
  
      
      return(chrplot)
    }
    
  })
  
}