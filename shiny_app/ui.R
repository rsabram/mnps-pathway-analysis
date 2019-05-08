dashboardPage(
  skin = 'blue',
  dashboardHeader(
    title = "MNPS Pathway Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Overview',tabName = "overview",icon = icon('bookmark')),
      menuItem("School Comparisons", tabName = "data", icon = icon('balance-scale')),
      menuItem("Cluster Performace", tabName = "pathways", icon = icon("chart-line")),
      menuItem("Search For A School", tabName = "select_a_school", icon = icon("school")),
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
                      h2('Change by cluster'),
                      h4('blah blah'),
                      radioButtons("graphType", label = "Graph Type",
                                   choices = list("Overall Mastery" = 1, "Change in Mastery" = 2), 
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
                    fluidRow(h1('test')),
                    fluidRow(h1('blah blah blah')),
                    selectInput("pickGrade", label = "Student grade level:", 
                                choices = grades, 
                                selected = 'All Grades'),
                   uiOutput("filterSubject"), 
                    selectInput("pickSubgroup", label = "Primary student subgroup:", 
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
                      h2('Select Two Schools to Compare'),
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
                    h4(email <- a("Email", href="mailto:rachaelshore@gmail.com?Subject=SAT%20Testing%20Site%20Analysis%20App", target="_blank")),
                    h4(linkedin <- a("LinkedIn", href="https://www.linkedin.com/in/rsabram/", target="_blank")),
                    h4(github <- a("GitHub", href="https://github.com/rsabram", target="_blank")), width = 6
                  ),
                  mainPanel()
                )
              )
      )
)
  )
)



