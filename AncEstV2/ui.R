library(shiny)

ui = fluidPage(
  
  titlePanel('GnomAD Ancestry Estimation', windowTitle = 'Mixtures Ancestry Estimation'),
  
  textOutput('authorid1'),
  textOutput('authorid2'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      wellPanel(
        
        pickerInput(
          inputId = 'ancdat',
          label = NULL, 
          choices = c('AFR', 'AMR', 'OTH'),
          choicesOpt = list(
            subtext = c('African/African American', 'American/Latinx', 'Other')
          ),
          selected = 'AFR'
        ),
        
        radioGroupButtons(
          inputId = 'bbranchr',
          label = NULL,
          choiceNames = c('Block Bootstrap', 'Random Sample', 'Chromosome'),
          choiceValues = c('bb', 'randsnp', 'chr'),
          selected = 'randsnp',
          justified = TRUE,
          individual = TRUE
        ),
        
        radioGroupButtons(
          inputId = 'exge',
          label = NULL,
          choiceNames = c('Genome', 'Exome'),
          choiceValues = c('genome', 'exome'),
          selected = 'genome',
          justified = TRUE,
          individual = TRUE
        )
        
      ),
      
      wellPanel(
        
        
        
      )
      
    ),
    
    mainPanel(
      
      fluidRow(
        plotOutput('mainPlot')
      ),
      
      fluidRow(
        
      ),
      
      fluidRow(
        
      )
      
    )
    
  )
  
)