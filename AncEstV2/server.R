library(shiny)

server = function(input, output) {
  
  output$authorid1 = renderText({
    'Mixtures Research Group'
  })
  output$authorid2 = renderText({
    'University of Colorado Denver'
  })
  
  bbranchrdat = reactiveValues(bbdat = NULL, randat = NULL, chrdat = NULL)
  
  
  
  observeEvent({
    input$ancdat
    input$exge}, {
      
      bbranchrdat$bbdat = bbdat %>% 
        filter(Exge == paste(input$exge)) %>% 
        filter(Gnomadanc == paste(tolower(input$ancdat)))
      
      tempdat = randsnpdat %>% 
        filter(Exge == paste(input$exge)) %>% 
        filter(Gnomadanc == paste(tolower(input$ancdat)))
      
      bbranchrdat$randat = tempdat %>% 
        slice(23:dim(tempdat)[1])
      
      bbranchrdat$chrdat = tempdat %>% 
        slice(1:22)
      
    }
  )
  
  output$mainPlot = renderPlot({
    
    if (input$bbranchr == 'randsnp'){
      
      snpnum = 0
      if (input$exge == 'genome'){
        snpnum = as.numeric(input$randsnpnumge)
      }
      else if (input$exge == 'exome'){
        snpnum = as.numeric(input$randsnpnumex)
      }
      
      tdat = bbranchrdat$randat %>%
        filter(NumberSNPs == snpnum) %>%
        select(AFR, EAS, EUR, NAM, SAS)
      
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
              strip.text.y = element_text(size = 18),
              axis.ticks = element_blank())
      
      return(randplot)

    }
    
    if (input$bbranchr == 'bb'){

      tdat = bbranchrdat$bbdat %>%
        select(AFR, EAS, EUR, NAM, SAS)

      bbmeltdat <- melt(tdat)
      names(bbmeltdat) = c('Ancestry', 'Proportion')
      
      bbplot = ggplot(bbmeltdat, aes(x=Proportion, fill = Ancestry)) +
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
              strip.text.y = element_text(size = 18),
              axis.ticks = element_blank())
      
      return(bbplot)

    }
    
    if (input$bbranchr == 'chr'){
      
      tdat = bbranchrdat$chrdat %>%
        select(TestType, AFR, EAS, EUR, NAM, SAS)
      tdat$TestType = c(1:22)
      
      names(tdat) = c('Chromosome', 'AFR', 'EAS', 'EUR', 'NAM', 'SAS')
      
      chrminmax = input$chrval
      
      tdat = tdat[chrminmax[1]:chrminmax[2],]
      
      chrmelt<- melt(tdat, id="Chromosome", 
                     measure=c('AFR', 'EAS', 'EUR', 'NAM', 'SAS'), 
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
  
  
  output$secondaryPlot = renderPlot({
    
    snpnum = 0
    
    if (input$bbranchr == 'bb'){
      t2dat = bbranchrdat$bbdat %>% 
        select(AFR, EAS, EUR, NAM, SAS)
    }
    else if (input$bbranchr == 'randsnp'){
      if (input$exge == 'genome'){
        snpnum = as.numeric(input$randsnpnumge)
      }
      else if (input$exge == 'exome'){
        snpnum = as.numeric(input$randsnpnumex)
      }
      t2dat = bbranchrdat$randat %>% 
        filter(NumberSNPs == snpnum) %>% 
        select(AFR, EAS, EUR, NAM, SAS)
    }
    
    plot_list = list()
    
    anc_list = names(t2dat)
    
    for (i in 1:5){
      pl = ggplot(data = t2dat, aes_string(x = anc_list[[i]])) +
        geom_histogram(aes(y = ..density..), color = 'black', fill = colvec[i], bins = 30) +
        geom_density(fill = NA) +
        geom_vline(data = t2dat, xintercept = quantile(t2dat[[anc_list[i]]], probs = 0.025), linetype ="longdash", size = .8) +
        geom_vline(data = t2dat, xintercept = quantile(t2dat[[anc_list[i]]], probs = 0.975), linetype ="longdash", size = .8) +
        theme_minimal() + 
        labs(title = paste(anc_list[i]), x = NULL, y = NULL) +
        scale_x_continuous(labels = percent_format(accuracy = .01), 
                           breaks = c(quantile(t2dat[[anc_list[i]]], probs = 0.025), quantile(t2dat[[anc_list[i]]], probs = 0.975))) +
        theme(plot.title = element_text(hjust = 0.5, size = 15),
              axis.text.x = element_text(face="bold", size=15),
              axis.text.y = element_blank())
      plot_list[[i]] = pl
    }
    
    return(grid.arrange(plot_list[[1]],
                        plot_list[[2]],
                        plot_list[[3]],
                        plot_list[[4]],
                        plot_list[[5]], ncol = 5)
)
    
  })
  
  output$bbraninfo1 = renderPrint({
    
    if (input$bbranchr == 'randsnp'){
      
      raninfo = bbranchrdat$randat
      
      if (input$exge == 'genome'){
        snpnuminfo = input$randsnpnumge
      }
      else if (input$exge == 'exome'){
        snpnuminfo = input$randsnpnumex
      }
      
      rancinfo = raninfo %>% 
        filter(NumberSNPs %in% snpnuminfo) %>% 
        select(AFR, EAS, EUR, NAM, SAS)
      names(rancinfo) = c('African', 'East Asian', 'European', 'Native American', 'South Asian')
      
      return(summary(rancinfo))
      
    }
    
    if (input$bbranchr == 'bb'){
      
      bbinfo = bbranchrdat$bbdat
      
      bbcinfo = bbinfo %>% 
        select(AFR, EAS, EUR, NAM, SAS)
      names(bbcinfo) = c('African', 'East Asian', 'European', 'Native American', 'South Asian')
      
      return(summary(bbcinfo))
      
    }
    
  })
  
  output$bbraninfo2 = renderPrint({
    
    if (input$bbranchr == 'randsnp'){
      
      raninfo2 = bbranchrdat$randat
      
      if (input$exge == 'genome'){
        snpnuminfo = input$randsnpnumge
      }
      else if (input$exge == 'exome'){
        snpnuminfo = input$randsnpnumex
      }
      
      rancinfo2 = raninfo2 %>% 
        filter(NumberSNPs %in% snpnuminfo) %>% 
        select(Time, Iterations, MinValue)
      names(rancinfo2) = c('Time', 'Iterations', 'Minimaztion Value')
      
      return(summary(rancinfo2))
      
    }
    
    if (input$bbranchr == 'bb'){
      
      bbinfo2 = bbranchrdat$bbdat
      
      bbcinfo2 = bbinfo2 %>% 
        select(Time, Iterations, MinValue)
      names(bbcinfo2) = c('Time', 'Iterations', 'Minimaztion Value')
      
      return(summary(bbcinfo2))
      
    }
    
  })
  
  output$chrinfo = renderTable(
    
    {chrdatinfo = bbranchrdat$chrdat
    chrdatinfo$TestNum = NULL
    chrdatinfo$Exge = NULL
    chrdatinfo$Gnomadanc = NULL
    chrminmaxinfo = input$chrval
    chrdatinfo = chrdatinfo[chrminmaxinfo[1]:chrminmaxinfo[2],]
    chrdatinfo$NumberSNPs = as.character(chrdatinfo$NumberSNPs)
    names(chrdatinfo) = c('Chromosome', 'Number of SNPs', 'European', 'African', 'South Asian', 'East Asian', 'Native American',
                          'Test Time (Seconds)', 'Algorithm Iterations', 'Function Value')
    
    return(chrdatinfo)},
    
    digits = 4
    
  )
  
}