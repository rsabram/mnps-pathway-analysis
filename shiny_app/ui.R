dashboardPage(
  skin = 'blue',
  dashboardHeader(
    title = "MNPS Pathway Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Overview',tabName = "overview",icon = icon('bookmark')),
      menuItem("MNPS Master School Data", tabName = "data", icon = icon('database')),
      menuItem("Pathway Analysis", tabName = "pathways", icon = icon("chart-line")),
      menuItem("Select A School", tabName = "select_a_school", icon = icon("school")),
      menuItem("Contact Me", tabName = "contact", icon = icon('at'))
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(h1('Exploring SAT Testing Sites in South Carolina'),
                               h3(strong('Project Summmary & Motivation')),
                               h4('From 2014-2016, I taught high school math and computer science in rural South Carolina',here <- a("(here).", href="https://www.marion.k12.sc.us/Domain/12", target="_blank") , "I taught primarily juniors and seniors, and as a wide-eyed Teach for America corps member I was determined to ensure 100% of my students could apply, attend, and graduate from college. I quickly realized this was not reality for my students - about 30% of the graduating class attended college after graduation and 30% of that population would go on to complete their degree. One particular barrier to higher education that concerned me was access to a testing location for the ACT or SAT. These tests are required for admittance to colleges and universities, and often a prerequisite for scholarships. The closest testing centers to my students were an hour away. I remember waking up at 5:30 a.m. on a Saturday to drive one of my students from his house to a testing center, and then to pick him up again after he finished to drive him home. Here, I explore a possible correlation between the locations of SAT testing sites and assorted demographic and social determinant factors."),
                               h3(strong('Key Data Questions')),
                               h4('Is there a difference in demographic groups for high schools that are:'),
                               h4(tags$ul(tags$li("Rural vs. Urban"),tags$li("Testing Sites vs. Non Testing Sites"))),
                               h4('Is there a statistically significant difference in demographic groups, school quality, or SAT scores for high schools that are testing sites vs. those that are not?
                                  '),
                               h3(strong('Data Sources')),
                               h5(em('*all data is from 2018')),
                               h4(tags$ul(
                                 tags$li(locations <- a("SAT Testing Locations", href="https://collegereadiness.collegeboard.org/pdf/sat-domestic-code-list.pdf", target="_blank")),
                                 tags$li(report_card <- a("SC Report Cards", href="https://screportcards.com/", target="_blank")),
                                 tags$li(sat_scores <- a("SAT Scores by School", href="https://ed.sc.gov/data/test-scores/national-assessments/sat/", target="_blank")),
                                 tags$li(enrollment <- a("School Enrollment by Gender and Race", href="https://ed.sc.gov/data/other/student-counts/active-student-headcounts/", target="_blank")),
                                 tags$li(frl_rate <- a("School Poverty Index", href="https://screportcards.com/", target="_blank"))
                               )
                               ),
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
                    selectInput("pickSubject", label = h5("Tested subject:"), 
                                choices = subjects, 
                                selected = 1),
                    selectInput("pickGrade", label = h5("Student grade level:"), 
                                choices = grades, 
                                selected = 'All Grades'),
                    selectInput("pickSubgroup", label = h5("Primary student subgroup:"), 
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
                    sidebarPanel(h4('Review or download all South Carolina high school data below.'), downloadButton("downloadData", "Download"),
                                 width = 12)
                  ),
                  fluidRow(
                    mainPanel(dataTableOutput("school_data"),width = 12)
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



