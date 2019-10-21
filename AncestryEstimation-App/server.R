library(shiny)

server = function(input, output) {
  
  refdat = genome_tests_afr
  randchrdat = reactiveValues(chr_dat = NULL, rand_dat = NULL)
  
  output$authorid1 = renderText({
  'Mixtures Research Group'
  })
  output$authorid2 = renderText({
  'University of Colorado Denver'
  })
  
  observeEvent({
    input$ancdat
    input$exge}, {
      temprefdat = get(paste(input$exge,'_tests_',tolower(input$ancdat), sep = ''))
      randchrdat$chr_dat = temprefdat[1:22,]
      randchrdat$rand_dat = temprefdat[23:length(temprefdat$Type_of_test),]
    }
  )
  
  output$mainPlot = renderPlot({
    
    if (input$chrrand == 'randsnp'){
      
      randdat = randchrdat$rand_dat
      
      if (input$exge == 'genome'){
        snpnum = input$randsnpnumge
      }
      else if (input$exge == 'exome'){
        snpnum = input$randsnpnumex
      }
      
      indices = which(randdat$Number_SNPs == snpnum)
      
      randcleandat = randdat[(indices[1]):tail(indices, n = 1),4:8]
      names(randcleandat) = c('EUR', 'AFR', 'SAS', 'EAS', 'NAM')
      
      randmeltdat <- melt(randcleandat)
      names(randmeltdat) = c('Ancestry', 'Proportion')
      
      randplot = ggplot(randmeltdat, aes(x=Proportion, fill = Ancestry)) +
        geom_histogram(bins = 400)+
        facet_grid(Ancestry ~ .)+
        scale_fill_manual(values = colvec)+
        guides(fill = FALSE)+
        scale_x_continuous(breaks = c(0,.25,.50,.75,1),
                           limits = c(0,1)) +
        theme(axis.text.x = element_text(size=20),
              axis.title.x = element_text(size=20),
              axis.text.y = element_blank(),
              axis.title.y = element_blank(),
              strip.text.y = element_text(size = 15),
              axis.ticks = element_blank())
      
      return(randplot)
      
    }
    
    if (input$chrrand == 'chr'){
      
      chrdat = randchrdat$chr_dat
      chrdat$Type_of_test = c(1:22)
      
      chrdat1 = chrdat[,1]; chrdat2 = chrdat[,4:8]
      chrdat = cbind(chrdat1, chrdat2)
      names(chrdat) = c('Chromosome', 'European', 'African', 'South Asian', 'East Asian', 'Native American')
      
      chrminmax = input$chrval
      
      chrdat = chrdat[chrminmax[1]:chrminmax[2],]
      
      chrmelt<- melt(chrdat, id="Chromosome", 
                         measure=c('European', 'African', 'South Asian', 'East Asian', 'Native American'), 
                         variable.name="Anc", value.name="Proportions")
      
      chrplot = ggplot(chrmelt, aes(Chromosome, Proportions, fill=Proportions)) + 
        facet_wrap( ~ Anc ,nrow = 1) +
        geom_bar(stat="identity") +
        ylim(c(0,1)) +
        coord_flip() +
        scale_fill_distiller(palette = 'Spectral')+
        guides(fill = FALSE) +
        scale_x_reverse(breaks = c(1:22), expand = c(0,0)) +
        theme(
          panel.grid.minor.y = element_blank()
        )
  
      
      return(chrplot)
    }
    
  })
  
  output$randinfo1 = renderPrint({
    
    randdatinfo = randchrdat$rand_dat
    
    if (input$exge == 'genome'){
      snpnuminfo = input$randsnpnumge
    }
    else if (input$exge == 'exome'){
      snpnuminfo = input$randsnpnumex
    }
    
    indicesinfo = which(randdatinfo$Number_SNPs == snpnuminfo)
    
    randcleandatinfo = randdatinfo[(indicesinfo[1]):tail(indicesinfo, n = 1),4:8]
    names(randcleandatinfo) = c('European', 'African', 'South Asian', 'East Asian', 'Native American')
    
    return(summary(randcleandatinfo))
    
  })
  
  output$randinfo2 = renderPrint({
    
    randdatinfo1 = randchrdat$rand_dat
    
    if (input$exge == 'genome'){
      snpnuminfo1 = input$randsnpnumge
    }
    else if (input$exge == 'exome'){
      snpnuminfo1 = input$randsnpnumex
    }
    
    indicesinfo1 = which(randdatinfo1$Number_SNPs == snpnuminfo1)
    
    randcleandatinfo1 = randdatinfo1[(indicesinfo1[1]):tail(indicesinfo1, n = 1),9:11]
    randcleandatinfo1$Test_time = as.numeric(randcleandatinfo1$Test_time)
    names(randcleandatinfo1) = c('Test Time (Seconds)', 'Algorithm Iterations', 'Least Squares Error')
    
    return(summary(randcleandatinfo1))
    
  })
  
  output$chrinfo = renderTable(
    
    {chrdatinfo = randchrdat$chr_dat
    chrdatinfo$Test_Number = NULL
    chrminmaxinfo = input$chrval
    chrdatinfo = chrdatinfo[chrminmaxinfo[1]:chrminmaxinfo[2],]
    chrdatinfo$Number_SNPs = as.character(chrdatinfo$Number_SNPs)
    names(chrdatinfo) = c('Chromosome', 'Number of SNPs', 'European', 'African', 'South Asian', 'East Asian', 'Native American',
                          'Test Time (Seconds)', 'Algorithm Iterations', 'Least Squares Error')
    
    return(chrdatinfo)},
    
    digits = 4
    
  )
  
}