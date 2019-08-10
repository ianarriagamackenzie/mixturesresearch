output$distPlot = renderPlot({
  
  if (input$anctype == 'afr'){
    if (input$randorchr == 'chr'){
      indchr = refdatafr[1:19,1:8]
      
      b = ggplot(indchr, aes(Type_of_test, AFR))+
        geom_col(Type_of_test, AFR)
      
      print(b)
    }
    
    if (input$randorchr == 'rand'){
      ind = which(refdatafr$Number_SNPs == input$ancrand)
      
      gg = refdatafr[(ind[1]):tail(ind, n = 1),4:8]
      
      gg2 <- melt(gg)
      
      a = ggplot(gg2, aes(x=value, fill=variable)) +
        geom_histogram(binwidth=.005)+
        facet_grid(variable~.)
      
      print(a)
    }
  }
  
  if (input$anctype == 'amr'){
    if (input$randorchr == 'chr'){
      indchr = refdatamr[input$ancchr,4:8]
      
      indchr1 = melt(indchr)
      
      b = ggplot(indchr1, aes(x = '', y = value, fill = variable)) +
        geom_bar(width = 1, stat = 'identity')+
        coord_polar('y', start = 0)
      
      print(b)
    }
    
    if (input$randorchr == 'rand'){
      ind = which(refdatamr$Number_SNPs == input$ancrand)
      
      gg = refdatamr[(ind[1]):tail(ind, n = 1),4:8]
      
      gg2 <- melt(gg)
      
      a = ggplot(gg2, aes(x=value, fill=variable)) +
        geom_histogram(binwidth=.005)+
        facet_grid(variable~.)
      
      print(a)
    }
  }
  
  if (input$anctype == 'oth'){
    if (input$randorchr == 'chr'){
      indchr = refdatoth[input$ancchr,4:8]
      
      indchr1 = melt(indchr)
      
      b = ggplot(indchr1, aes(x = '', y = value, fill = variable)) +
        geom_bar(width = 1, stat = 'identity')+
        coord_polar('y', start = 0)
      
      print(b)
    }
    
    if (input$randorchr == 'rand'){
      ind = which(refdatoth$Number_SNPs == input$ancrand)
      
      gg = refdatoth[(ind[1]):tail(ind, n = 1),4:8]
      
      gg2 <- melt(gg)
      
      a = ggplot(gg2, aes(x=value, fill=variable)) +
        geom_histogram(binwidth=.005)+
        facet_grid(variable~.)
      
      print(a)
    }
  }
  
})

output$infoPlot = renderPrint({
  
  if (input$anctype == 'afr'){
    if (input$randorchr == 'chr'){
      refdatafr[input$ancchr,4:8]
    }
    
    else if(input$randorchr == 'rand'){
      summary(refdatafr[which(refdatafr$Number_SNPs == input$ancrand)[1]:tail(which(refdatafr$Number_SNPs == input$ancrand), n = 1),4:11])
    }
  }
  
  else if (input$anctype == 'amr'){
    if (input$randorchr == 'chr'){
      refdatamr[input$ancchr,4:8]
    }
    
    else if(input$randorchr == 'rand'){
      summary(refdatamr[which(refdatamr$Number_SNPs == input$ancrand)[1]:tail(which(refdatamr$Number_SNPs == input$ancrand), n = 1),4:11])
    }
  }
  
  else if (input$anctype == 'oth'){
    if (input$randorchr == 'chr'){
      refdatoth[input$ancchr,4:8]
    }
    
    else if(input$randorchr == 'rand'){
      summary(refdatoth[which(refdatoth$Number_SNPs == input$ancrand)[1]:tail(which(refdatoth$Number_SNPs == input$ancrand), n = 1),4:11])
    }
  }
  
})