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
                    title = "Block Bootstrapping", width = NULL, status = "primary",
                    'We use block bootstrapping to estimate error in our method.  We partition our data into 3357 one centiMorgan blocks 
                    and resample with replacement before estimating ancestry proportions.  We replicate this 1000 times for the plots 
                    and CIs shown here.'
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
                    title = "Random SNP Sample", width = NULL, status = "primary",
                    'We sample N random SNPs across all 22 chromosomes and evaluate ancestry proportions using our 
                    method.  We replicate the random sample 1000 times for the plots and CIs shown.  Here N can be varied 
                    to evaluate our method with difference numbers of SNPs sampled.'
                  ),
                  
                  box(
                    title = 'N Random SNPs', width = NULL, status = "primary",
                    
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
                    title = "Chromosome", width = NULL, status = "primary",
                    'We partition our data by chromosome and evaluate ancestry proportions using our method.  Because our method 
                    is deterministic, we only estimate ancestry on each chromosome once.'
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
              fluidRow(
                
                column(
                  width = 8,
                  box(
                    title = "ReadMe", width = NULL, status = "primary", height = 380,
                    'This data was aggregated from the publically available genetic databases: ',
                    a('1000 Genomes Project', href = "https://www.internationalgenome.org/"),
                    'and', 
                    a('Genome Aggregation Database', href = 'https://gnomad.broadinstitute.org/'), '.',
                    br(),
                    br(),
                    'Our reference dataset was created from 1000 Genomes Project and an allele frequency dataset for Native Americans 
                    provided by Dr. Chris Gignoux.  This reference panel was merged by rsID, reference and alternate alleles. 
                    Tri-allelic SNPs and SNPs with incomplete information were removed, leaving ~600,000 SNPs across all 22 
                    chromosomes in our reference panel.',
                    br(),
                    br(),
                    'Our tested dataset was created from the gnomAD database.  This AF data was matched to our reference dataset 
                    by rsID, reference and alternate alleles.  We had ~580,000 SNPs across all 22 chromosomes to evaluate using our method 
                    after cleaning and matching the data.',
                    br(),
                    br(),
                    'We estimate the ancestry proportions within gnomAD ancestry groups by using our algorithm in block bootstrapping, 
                    by chromosome, and a random SNP sample.  The results of our findings are displayed in various tabs in this Shiny app.'
                  )
                ),
                
                column(
                  width = 4,
                  box(
                    title = "Acknowledgements", width = NULL, status = "primary", height = 380,
                    
                    strong("This work was a collaborative effort by:"),
                    br(), 
                    "Ian S. Arriaga-Mackenzie, Gregory M. Matesi, Alexandria Ronco, Ryan Scherenberg, 
                    Andrew Zerwick, Yinfei Wu, James Vance, Jordan R. Hall, Christopher R. Gignoux, 
                    Megan Null, Audrey E. Hendricks",
                    br(),
                    strong("Additional Funding from:"),
                    br(),
                    "CU Denver Undergraduate Research Opportunity Program (UROP) and 
                    Education through Undergraduate Research and Creative Activities (EUReCA) program",
                    br(),
                    strong('Shiny App created and maintained by:'),
                    br(),
                    'Ian S. Arriaga MacKenzie'
                  )
                )
                
              ),
              
              fluidRow(
                
                column(
                  width = 8,
                  box(
                    title = "Disclaimer", width = NULL, status = "primary", height = 300,
                    
                    'Under no circumstances shall authors of this website and ancestry estimation algorithm be liable for 
                    any indirect, incidental, consequential, special or exemplary damages arising out of or in connection
                    with your access or use of or inability to access the ancestry estimation website or any associated software 
                    and tools and any third party content and services, whether or not the damages were foreseeable and whether or 
                    not the authors were advised of the possibility of such damages.  By using the ancestry estimation platform 
                    you agree to use it to promote scientific research, learning or health.'
                  )
                ),
                
                column(
                  width = 4,
                  img(src='CUdenverlogo.png', align = "Center", height = 150, width = 240)
                )
                
              )
      )
      
    )
    
  )

)