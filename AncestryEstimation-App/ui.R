library(shiny)

ui = fluidPage(

  titlePanel('GnomAD Ancestry Estimation', windowTitle = 'Mixtures Ancestry Estimation'),

  textOutput('authorid1'),
  textOutput('authorid2'),

  sidebarLayout(
    
    sidebarPanel(
      
      (wellPanel(
        
        helpText('Genome and exome specific ancestry estimation.
                 Analysis of the (African), (American/Latino), and (Other) allele frequency data sets from gnomAD.'),
        
        radioGroupButtons(
          inputId = 'exge',
          label = NULL,
          choiceNames = c('Genome', 'Exome'),
          choiceValues = c('genome', 'exome'),
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
          selected = 'AFR',
          options = list(
            style = "btn-primary")
        )
        
      )),
      
      (wellPanel(
        
        helpText('Analysis on sampling of SNPs taken across all chromosomes, or each chromosome individually.'),
        
        radioGroupButtons(
          inputId = 'chrrand',
          label = NULL,
          choiceNames = c('Random Sample', 'By Chromosome'),
          choiceValues = c('randsnp', 'chr'),
          selected = 'randsnp',
          justified = TRUE,
          individual = TRUE
        ),
        
        conditionalPanel(
          condition = "input.chrrand == 'randsnp' ",
          
          helpText('Number of SNPs randomly sampled across all chromosomes. 
                   Resampled and tested 1000 times for distribution plots. 
                   Bin size = 0.25%'),
          
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
          
        ),
        
        conditionalPanel(
          condition = "input.chrrand == 'chr' ",
          
          helpText('Range of chromosomes, estimated across all SNPs present.'),
          
          sliderTextInput(
            inputId = 'chrval',
            label = NULL,
            choices = c(1:22),
            selected = c(1, 22),
            grid = TRUE,
            hide_min_max = TRUE
          )
          
        )
        
        
      ))
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          'Plots',
          plotOutput('mainPlot')
        ),
        
        tabPanel(
          'Numeric Summaries',
          
          conditionalPanel(
            condition = "input.chrrand == 'randsnp' ",
            
            fluidRow(
              verbatimTextOutput('randinfo1')
            ),
            
            fluidRow(
              verbatimTextOutput('randinfo2')
            )
          ),
          
          conditionalPanel(
            condition = "input.chrrand == 'chr' ",
            tableOutput('chrinfo')
          )
          
        )
        
      )
      
    )
    
  )
  
)