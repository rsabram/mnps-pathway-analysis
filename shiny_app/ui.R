dashboardPage(
  skin = 'blue',
  dashboardHeader(
    title = "MNPS Parent Tool"),
  dashboardSidebar(
    sidebarMenu(
      #menuItem('Overview',tabName = "overview",icon = icon('bookmark')),
      menuItem("Cluster Performace", tabName = "pathways", icon = icon("chart-line")),
      menuItem("School Comparisons", tabName = "data", icon = icon('balance-scale')),
      menuItem("Find A School", tabName = "select_a_school", icon = icon("school")),
      menuItem("Contact Me", tabName = "contact", icon = icon('at'))
    )),
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "overview",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(h1('Title'),
                               h3(strong('Project Summmary & Motivation')),
                               h3(strong('Key Data Questions')),
                               h3(strong('Data Sources')),
                               width = 12),
                  mainPanel()
                  )
              )
      ),
      tabItem(tabName = "pathways",
              title = "title",
              status = "primary", solidHeader = TRUE, width=3,
              fluidPage(
                sidebarLayout(
                  # Add a sidebar panel around the text and inputs
                  fluidRow(
                    sidebarPanel(
                      h2('Mastery Over Time'),
                      h4(em("This graph shows the median percentage of students achieving Level 3 or Level 4 proficiency on the TNReady ELA or Math assessment in 2017-2018. Median scores are grouped by school level, then again by cluster.")),
                      h5("Toggle to view scores for different subjects, or to switch to see the change in percent mastery as students progress from elementary school to their zoned middle and high school. Add or remove clusters as desired."),
                      radioButtons("graphType", label = "Graph Type",
                                   choices = list("Overall Mastery" = 1, "Change in % Mastery Through Pathway" = 2), 
                                   selected = 1),
                      radioButtons("graphSubject", label = "Subject",
                                   choices = list("ELA" = "ELA", "Math" = "Math"), 
                                   selected = "ELA"),
                      selectInput("graphCluster",
                                  label = "Clusters",
                                  choices = clusters,
                                  multiple = TRUE,
                                  selected = list("Pearl-Cohn","Hillsboro","Antioch","Cane Ridge","Hillwood","Glencliff","Whites Creek","Hunters Lane","Maplewood","McGavock","Overton")),
                      width = 10)
                  ),
                  # Add a main panel around the plot and table
                  fluidRow(mainPanel( 
                    fluidRow(
                      plotOutput("graphMastery",height = 500, width = 950))
                  )
              )
              )
              )
      ),
      
      tabItem(tabName = "select_a_school",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(h1('Find A School')),
                    fluidRow(em(h4('Find schools with the highest percent mastery for a specified grade level, subject, and student subgroup.'))),
                    selectInput("pickGrade", label = "Start by selecting the grade to filter by:", 
                                choices = grades, 
                                selected = 'All Grades'),
                   uiOutput("filterSubject"), 
                    selectInput("pickSubgroup", label = "Identify the primary student subgroup of interest:", 
                                choices = subgroups, 
                                selected = 1),
                    width =6
                  ),
                  mainPanel(
                    fluidRow(dataTableOutput("search"))
                  ),
                  ))),
      
      tabItem(tabName = "data",
              fluidPage(
                sidebarLayout(
                  fluidRow(
                    sidebarPanel(
                      h2('School Comparison'),
                      h4('Pick two schools to compare key statistics including subject-level mastery and demographic information.'),
                      selectInput("selectSchool1",
                                  label = "School 1",
                                  choices = schools,
                                  selected = 'All MNPS'),
                      selectInput("selectSchool2",
                                  label = "School 2",
                                  choices = schools,
                                  selected = 1),
                      width = 12
                  )),
                  fluidRow(
                             column(width = 6, align = "left",
                                    box(
                                      title = "School 1 Data", status = "primary", solidHeader = TRUE, height = 1300,
                                      width=NULL,
                                      fluidRow(valueBoxOutput("school_1_ela", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_math", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_male", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_female", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_white", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_black", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_hispanic", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_iep", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_frl", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_ell", width = 12))
                                    )
                             ),
                             column(width = 6, align = "left",
                                    box(
                                      title = "School 2 Data", status = "primary", solidHeader = TRUE, height = 1300,
                                      width=NULL, 
                                      fluidRow(infoBoxOutput("school_2_ela", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_math", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_male", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_female", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_white", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_black", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_hispanic", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_iep", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_frl", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_ell", width = 12))
                                    )
                             )
                           )
                    )
                  )
                  
                
              
              
      ),
      tabItem(tabName = "contact",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    h3('Rachael Abram'),
                    h4(email <- a("Email", href="mailto:rachaelshore@gmail.com?Subject=MNPS%20Pathway%20Tool", target="_blank")),
                    h4(linkedin <- a("LinkedIn", href="https://www.linkedin.com/in/rsabram/", target="_blank")),
                    h4(github <- a("GitHub", href="https://github.com/rsabram", target="_blank")),
                    width = 6
                  ),
                  mainPanel()
                )
              )
      )
)
  )
)



