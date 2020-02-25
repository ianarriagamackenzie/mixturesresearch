ui = dashboardPage(
  
  skin = 'blue',
  
  dashboardHeader(
    
    title = 'gnomAD Ancestry Estimation',
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
      menuItem("Genome-wide Ancestry Proportions",
               icon = icon("chart-area"),
               startExpanded = TRUE,
               menuSubItem("Block Bootstrap",
                           tabName = "bb",
                           selected = TRUE),
               menuSubItem("Random SNP Sample",
                           tabName = "ran")
               ),
      menuItem("Ancestry Proportions by Chromosome", tabName = "chr", icon = icon("chart-bar")),
      menuItem("ReadMe", tabName = "readme", icon = icon("readme"))
      # menuItem("Github", icon = icon("code"),
      #          href = "https://github.com/hendriau/Mixtures",
      #          newtab = TRUE)
      
    )
    
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML(".main-sidebar { font-size: 12px; }")) # change the font size to 12
    ),
    
    tabItems(

      tabItem(tabName = "bb",
              
              fluidRow(
                
                column(
                  width = 3,
                  box(
                    title = "Block Bootstrapping", width = NULL, status = "primary",
                    'We use block bootstrapping to estimate error for the ancestry proportions. 
                    We resample 3,357 centiMorgan blocks 1,000 times for the plots and confidence intervals shown here.'
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
                    'We sample N random SNPs across the 22 autosomes to estimate ancestry proportions. 
                    We randomly sample 1,000 times for the plots and confidence intervals shown here. 
                    N can be varied to evaluate our method with different numbers of SNPs.'
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
                    'Estimated ancestry proportions by chromosome using all SNPs.'
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
                    title = "ReadMe", width = NULL, status = "primary", height = 450,
                    
                    'Our reference panel was created from ',
                    a('1000 Genomes Project', href = "https://www.internationalgenome.org/", target="_blank"),
                    ' (GRCh37/hg19) superpopulations (African, Non-Finish European, East Asian, South Asian) and an ',
                    a('Indigenous American population', href = "ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/technical/working/20130711_native_american_admix_train", target="_blank"),
                    ' (616,568 SNPs and 43 individuals, GRCh37/hg19). Tri-allelic SNPs and SNPs with missing 
                    allele frequency information were removed, leaving 613,298 SNPs across the 22 autosomes.',
                    br(),
                    br(),
                    'We estimate the ancestry proportions from ',
                    a('gnomAD V2', href = 'https://gnomad.broadinstitute.org/', target="_blank"),
                    '(GRCh37/hg19). After merging with our reference panel we checked for allele matching and strand flips. 
                    Our final dataset had 582,550 genome SNPs and 9,835 exome SNPs across the 22 autosomes.'
                  )
                ),
                
                column(
                  width = 4,
                  box(
                    title = "Acknowledgements", width = NULL, status = "primary", height = 450,
                    
                    strong("This work was a collaborative effort by:"),
                    br(), 
                    "Ian S. Arriaga-Mackenzie, Gregory M. Matesi, Alexandria Ronco, Ryan Scherenberg, 
                    Andrew Zerwick, Yinfei Wu, James Vance, Jordan R. Hall, Christopher R. Gignoux, 
                    Megan Null, Audrey E. Hendricks",
                    br(),
                    strong("Additional Funding from:"),
                    br(),
                    "CU Denver Undergraduate Research Opportunity Program (UROP)",
                    br(),
                    "Education through Undergraduate Research and Creative Activities (EUReCA) program",
                    br(),
                    strong('Shiny App created and maintained by:'),
                    br(),
                    'Ian S. Arriaga MacKenzie',
                    br(),
                    a(actionButton(inputId = "email1", label = "email", 
                                   icon = icon("envelope", lib = "font-awesome")),
                      href="mailto:IAN.ARRIAGAMACKENZIE@ucdenver.edu"),
                    br(),
                    strong('Principle Investigator:'),
                    br(),
                    'Dr. Audrey E. Hendricks',
                    br(),
                    a(actionButton(inputId = "email2", label = "email", 
                                   icon = icon("envelope", lib = "font-awesome")),
                      href="mailto:AUDREY.HENDRICKS@ucdenver.edu")
                  )
                )
                
              ),
              
              fluidRow(
                
                column(
                  width = 8,
                  box(
                    title = "Disclaimer", width = NULL, status = "primary",
                    
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