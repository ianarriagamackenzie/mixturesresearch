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
      
      tempdat = randsnpdat %>% 
        filter(Exge == paste(input$exge)) %>% 
        filter(Gnomadanc == paste(tolower(input$ancdat)))
      
      bbranchrdat$bbdat = bbdat %>% 
        filter(Exge == paste(input$exge)) %>% 
        filter(Gnomadanc == paste(tolower(input$ancdat)))
      
      bbranchrdat$randat = tempdat %>% 
        slice(23:dim(tempdat)[1])
      
      bbranchrdat$chrdat = tempdat %>% 
        slice(1:22)
      
    }
  )
  
}