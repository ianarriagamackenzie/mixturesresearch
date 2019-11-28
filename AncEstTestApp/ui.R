ui = dashboardPage(
  
  skin = 'blue',
  
  dashboardHeader(
    
    title = 'Gnomad Ancestry Estimation',
    titleWidth = 300
    
    ),
  
  dashboardSidebar(
    width = 250,
    
    fluidRow(
      align = 'center',
      h4('Hendricks Research Group'),
      h4('University of Colorado Denver')
    ),
    
    radioGroupButtons(
      inputId = 'exge',
      label = NULL,
      choiceNames = c('Genome', 'Exome'),
      choiceValues = c('genome', 'exome'),
      selected = 'genome',
      individual = TRUE,
      width = '100%',
      justified = TRUE,
      status = 'primary',
      checkIcon = list(
        yes = icon("ok", 
                   lib = "glyphicon"))
    ),
    
    pickerInput(
      inputId = 'ancdat',
      label = 'Ancestry Group',
      choices = c('AFR', 'AMR', 'OTH'),
      choicesOpt = list(
        subtext = c('African/African American', 'American/Latinx', 'Other')
      ),
      selected = 'AFR',
      options = list(
        `live-search` = TRUE)
    ),
    
    sidebarMenu(
      id = 'menuselect',
      menuItem("Block Bootstrap", tabName = "bb", icon = icon("chart-area"), selected = TRUE),
      menuItem("Chromosome", tabName = "chr", icon = icon("chart-bar")),
      menuItem("Random SNP Sample", tabName = 'ran', icon = icon("chart-area")),
      menuItem("ReadMe", tabName = "readme", icon = icon("readme")),
      menuItem("Github", icon = icon("code"),
               href = "https://github.com/hendriau/Mixtures")
      
    )
    
  ),
  
  dashboardBody(
    
    tabItems(

      tabItem(tabName = "bb",
              
              fluidRow(
                
                column(
                  width = 3,
                  box(
                    title = "bb Info", width = NULL, status = "primary", height = 260,
                    "bb Info"
                  )
                ),
                
                column(
                  width = 9,
                  tabBox(
                    title = "Proportion Estimates for Block Bootstrapping",
                    width = NULL, height = 440, side = 'right', selected = 'Visual',
                    
                    tabPanel(
                      'Numeric',
                      withSpinner(tableOutput(
                        'infobb'
                      ))
                    ),
                    tabPanel(
                      'Visual',
                      withSpinner(plotOutput(
                        'plotbb',
                        height = 370
                      ))
                    )
                  )
                )
                
              ),
              
              fluidRow(
                
                column(width = 1),
                column(
                  width = 10,
                  box(
                    title = "Distribution Plots and 95% Confidence Intervals", width = NULL,
                    status = "primary", height = 240,
                    withSpinner(plotOutput(
                      'distbb',
                      height = 170
                    ))
                  )
                )
                
              )
      ),
      
      tabItem(tabName = "ran",
              
              fluidRow(
                
                column(
                  width = 3,
                  box(
                    title = "ran Info", width = NULL, status = "primary", height = 260,
                    "ran Info"
                  ),
                  
                  box(
                    title = 'N Random SNPs', width = NULL,
                    status = "primary", height = 140,
                    
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
                
                column(
                  width = 9,
                  
                  tabBox(
                    title = "Proportion Estimates for Random SNP Sample",
                    width = NULL, height = 440, side = 'right', selected = 'Visual',
                    
                    tabPanel(
                      'Numeric',
                      withSpinner(tableOutput(
                        'inforan'
                      ))
                    ),
                    tabPanel(
                      'Visual',
                      withSpinner(plotOutput(
                        'plotran',
                        height = 370
                      ))
                    )
                  )
                )
                
              ),
              
              fluidRow(
                
                column(width = 1),
                column(
                  width = 10,
                  box(
                    title = "Distribution Plots and 95% Confidence Intervals", width = NULL,
                    status = "primary", height = 240,
                    withSpinner(plotOutput(
                      'distran',
                      height = 170
                    ))
                  )
                )
                
              )
      ),
      
      tabItem(tabName = "chr",
              
              fluidRow(
                
                column(
                  width = 3,
                  box(
                    title = "chr Info", width = NULL, status = "primary", height = 240,
                    "Info"
                  )
                ),
                
                column(
                  width = 9,
                  
                  tabBox(
                    title = "Proportion Estimates by Chromosome",
                    width = NULL, height = 700, side = 'right', selected = 'Visual',
                    
                    tabPanel(
                      'Numeric',
                      withSpinner(tableOutput(
                        'sumchr'
                      ))
                    ),
                    tabPanel(
                      'Visual',
                      withSpinner(plotOutput(
                        'plotchr',
                        height = 650
                      ))
                    )
                  )
                )
                
              )
              
      ),
      
      tabItem(tabName = "readme",
              h2("Readme")
      )
      
    )
    
  )

)