library(shiny)

ui = fluidPage(
  
  titlePanel('GnomAD Ancestry Estimation', windowTitle = 'Hendricks Research Group'),
  
  textOutput('authorid1'),
  textOutput('authorid2'),
  textOutput('authorid3'),
  
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
        
        splitLayout(
          
          radioGroupButtons(
            inputId = 'bbranchr',
            label = NULL,
            choiceNames = c('Block Bootstrap', 'Random Sample', 'Chromosome'),
            choiceValues = c('bb', 'randsnp', 'chr'),
            selected = 'bb',
            direction = 'vertical',
            justified = TRUE,
            checkIcon = list(
              yes = icon("ok", 
                         lib = "glyphicon"))
          ),
          
          radioGroupButtons(
            inputId = 'exge',
            label = NULL,
            choiceNames = c('Genome', 'Exome'),
            choiceValues = c('genome', 'exome'),
            selected = 'genome',
            direction = 'vertical',
            justified = TRUE,
            checkIcon = list(
              yes = icon("ok", 
                         lib = "glyphicon"))
          )
          
        )
        
      ),
      
      conditionalPanel(
        condition = "input.bbranchr == 'randsnp' ",
        
        wellPanel(
          conditionalPanel(
            condition = "input.exge == 'genome' ",
            sliderTextInput(
              inputId = 'randsnpnumge',
              label = NULL,
              choices = c(10, 50, 100, 500, 1000, 2500, 5000, 10000, 50000, 100000),
              selected = '1000',
              grid = TRUE,
              hide_min_max = TRUE
            )
          ),
          
          conditionalPanel(
            condition = "input.exge == 'exome' ",
            
            sliderTextInput(
              inputId = 'randsnpnumex',
              label = NULL,
              choices = c(10, 50, 100, 500, 1000, 2500, 5000, 7500, 9000),
              selected = '1000',
              grid = TRUE,
              hide_min_max = TRUE
            )
          )
        )
        
      ),
      
      conditionalPanel(
        condition = "input.bbranchr == 'chr' ",
        
        wellPanel(
          sliderTextInput(
            inputId = 'chrval',
            label = NULL,
            choices = c(1:22),
            selected = c(1, 22),
            grid = TRUE,
            hide_min_max = TRUE
          )
        )
        
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          'Plots',
          plotOutput('mainPlot', height = 400),
          
          conditionalPanel(
            condition = "input.bbranchr == 'bb' || input.bbranchr == 'randsnp' ",
            
            wellPanel(
              plotOutput('secondaryPlot', height = 200)
            )
          )
        ),
        
        tabPanel(
          'Numeric Summaries',
          
          conditionalPanel(
            condition = "input.bbranchr == 'bb' || input.bbranchr == 'randsnp' ",
            
            fluidRow(
              verbatimTextOutput('bbraninfo1')
            ),
            
            fluidRow(
              verbatimTextOutput('bbraninfo2')
            )
          ),
          
          conditionalPanel(
            condition = "input.bbranchr == 'chr' ",
            tableOutput('chrinfo')
          )
          
        )
        
      )
    
  )
  
)

)