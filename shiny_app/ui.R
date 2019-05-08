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
                      h2('Explore Trends for Urban/Rural School Sites'),
                      h4('Select the population groups and variables you want to explore to visualize differences in the bar graph below.'),
                      checkboxGroupInput("checkGroup", label = strong("School Groups"), 
                                         choices = list("Rural, Not Testing Site"= "Rural, No Testing Sites", "Rural, Testing Site" = 'Rural, Testing Sites', "Urban, Not Testing Site" = "Urban, No Testing Sites", "Urban, Testing Site" = "Urban, Testing Sites"),
                                         selected = list("Rural, No Testing Sites","Urban, Testing Sites")),
                      selectInput("selectVar",
                                  label = "Variables",
                                  choices = list("Percentage of Students Receiving Free or Reduced Lunch" = "mean_pct_frl","Percentage of Seniors who Took SAT" = "mean_pct_tested", "Black Student Percentage" = "mean_pct_black", "White Student Percentage" = "mean_pct_white", "Male Student Percentage" = "mean_pct_male"),
                                  multiple = TRUE,
                                  selected = list("mean_pct_frl","mean_pct_tested")),
                      width = 10)
                  ),
                  # Add a main panel around the plot and table
                  fluidRow(mainPanel( 
                    tabsetPanel(
                      tabPanel("Plot", plotOutput("sites",height = 500, width = 950)), 
                      tabPanel("Table", tableOutput("table"))
                    )
                  )
                  ))
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
                                  selected = '1'),
                      width = 12
                  )),
                  fluidRow(
                             column(width = 6, align = "center",
                                    box(
                                      title = "School 1 Data", status = "primary", solidHeader = TRUE, height = 580,
                                      width=NULL,
                                      fluidRow(infoBoxOutput("school_1_ela", width = 12)),
                                      fluidRow(infoBoxOutput("school_1_math", width = 12))
                                      
                                    )
                             ),
                             column(width = 6, align = "center",
                                    box(
                                      title = "School 2 Data", status = "primary", solidHeader = TRUE, height = 580,
                                      width=NULL, 
                                      fluidRow(infoBoxOutput("school_2_ela", width = 12)),
                                      fluidRow(infoBoxOutput("school_2_math", width = 12))
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



