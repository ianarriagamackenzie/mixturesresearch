library(shiny)

ui = fluidPage(

  titlePanel('GnomAD Ancestry Estimation', windowTitle = 'Hendricks Mixtures Ancestry Estimation'),

  textOutput('authorid1'),
  textOutput('authorid2'),

  sidebarLayout(
    
    sidebarPanel(
      
      (wellPanel(
        
        helpText('test test test'),
        
        radioGroupButtons(
          inputId = 'exge',
          label = NULL,
          choiceNames = c('Exome', 'Genome'),
          choiceValues = c('exome', 'genome'),
          selected = 'genome',
          justified = TRUE,
          individual = TRUE
        ),
        
        pickerInput(
          inputId = 'ancdat',
          label = NULL, 
          choices = c('AFR', 'AMR', 'OTH'),
          choicesOpt = list(
            subtext = c('African', 'American/Latino', 'Other')
          ),
          options = list(
            style = "btn-primary")
        )
        
      )),
      
      (wellPanel(
        
        helpText('rand or chr'),
        
        radioGroupButtons(
          inputId = 'chrrand',
          label = NULL,
          choiceNames = c('Random Sample of SNPs', 'Chromosome'),
          choiceValues = c('randsnp', 'chr'),
          selected = 'randsnp',
          justified = FALSE,
          individual = TRUE
        ),
        
        conditionalPanel(
          condition = "input.chrrand == 'randsnp' ",
          sliderTextInput(
            inputId = 'randsnpnum',
            label = NULL,
            choices = c(10, 50, 100, 500, 1000, 2500, 5000, 10000, 50000, 100000),
            selected = '1000',
            grid = TRUE,
            hide_min_max = TRUE
          ),
          
          helpText('randsnp test 12')
        ),
        
        conditionalPanel(
          condition = "input.chrrand == 'chr' ",
          sliderTextInput(
            inputId = 'chrval',
            label = NULL,
            choices = c(1:22),
            selected = c(1, 22),
            grid = TRUE,
            hide_min_max = TRUE
          ),
          
          helpText('chr test')
        )
        
        
      ))
      
    ),
    
    mainPanel(
      
      plotOutput('mainPlot')
      
    )
    
  )
  
)