server = function(input, output, session) {
  
  bbranchrdat = reactiveValues(bbdat = NULL, randat = NULL, chrdat = NULL)
  
  observeEvent({
    input$ancdat
    input$exge}, {
      
      tempdat = dat %>% 
        filter(Exge %in% paste(input$exge)) %>% 
        filter(Gnomadanc %in% paste(tolower(input$ancdat)))
      
      bbranchrdat$bbdat = tempdat %>% 
        filter(TestType %in% 'blockbootstrap')
      
      bbranchrdat$randat = tempdat %>% 
        filter(TestType %in% 'Random_SNPs_Genomewide')
      
      bbranchrdat$chrdat = tempdat %>% 
        filter(TestType %in% levels(dat$TestType)[grep('CHR', levels(dat$TestType))])
      bbranchrdat$chrdat$TestType = c(1:22)
      
    })
  
  output$plotbb = renderPlot({
    
    bbranchrdat$bbdat %>% 
      proportionplot()
    
  })
  
  output$distbb = renderPlot({
    
    bbranchrdat$bbdat %>% 
      distplot()
    
  })
  
  output$infobb = renderTable({
    
    bbranchrdat$bbdat %>% 
      bbraninfo()
    
  }, digits = 5, align = 'c')
  
  output$plotran = renderPlot({
    snpnum = 0
    if (input$exge == 'genome'){
      snpnum = as.numeric(input$randsnpnumge)
    }
    else if (input$exge == 'exome'){
      snpnum = as.numeric(input$randsnpnumex)
    }
    
    bbranchrdat$randat %>%
      filter(NumberSNPs == snpnum) %>% 
      proportionplot()
    
  })
  
  output$distran = renderPlot({
    snpnum = 0
    if (input$exge == 'genome'){
      snpnum = as.numeric(input$randsnpnumge)
    }
    else if (input$exge == 'exome'){
      snpnum = as.numeric(input$randsnpnumex)
    }
    
    bbranchrdat$randat %>%
      filter(NumberSNPs == snpnum) %>% 
      distplot()
    
  })
  
  output$inforan = renderTable({
    snpnum = 0
    if (input$exge == 'genome'){
      snpnum = as.numeric(input$randsnpnumge)
    }
    else if (input$exge == 'exome'){
      snpnum = as.numeric(input$randsnpnumex)
    }
    
    bbranchrdat$randat %>%
      filter(NumberSNPs == snpnum) %>% 
      bbraninfo()
    
  }, digits = 5, align = 'c')
  
  output$plotchr = renderPlot({
    
    bbranchrdat$chrdat %>% 
      chrplot()
    
  })
  
  output$sumchr = renderTable({
    
    sumchrdat = bbranchrdat$chrdat %>% 
      select(TestType, AFR, EAS, EUR, NAM, SAS)
    names(sumchrdat) = c('Chromosome', 'AFR', 'EAS', 'EUR', 'IAM', "SAS")
    
    return(sumchrdat)
    
  }, digits = 5, spacing = 'xs', align = 'c')
  
}