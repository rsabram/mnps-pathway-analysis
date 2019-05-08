shinyServer(function(input, output) {

    

  output$search <- renderDataTable(
    mnps_tcap %>% 
      filter(Subject %in% input$filterSubject) %>% 
      filter(`Grade Level`%in% input$pickGrade) %>% 
      filter(Subgroup %in% input$pickSubgroup) %>% 
      arrange(desc(`Percent Mastered`)) %>% 
      select('School Name','Percent Mastered','Lowest Grade','Highest Grade','Cluster','Address','City','State','ZIP','Phone Number','Principal','Principal Email'),
    options = list(pageLength = 5,
                  lengthMenu = c(5,10, 15, 20)
  ))
  
  output$filterSubject <- renderUI({
      selectInput("filterSubject", "Tested Subject:", choices = mnps_tcap[mnps_tcap$'Grade Level' == input$pickGrade,"Subject"], selected = 1)
  })


  output$school_1_ela <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      filter(Subject == 'ELA') %>%
      select(`Percent Mastered`),
    "ELA Percent Mastery", icon = icon("book"), color = 'yellow', width = 12)
)
  
  
  output$school_1_math <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      filter(Subject == 'Math') %>%
      select(`Percent Mastered`),
    "Math Percent Mastery", icon = icon("calculator"), color = 'yellow', width = 12)
  )
  
  output$school_1_male <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      select(`% Male`),
    "Percent Male ", icon = icon("male"), color = 'blue', width = 12)
  )
  
  output$school_1_female <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      select(`% Female`),
    "Percent Female", icon = icon("female"), color = 'blue', width = 12)
  )
  
  output$school_1_white <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      select(`% White`),
    "Percent White", icon = icon("globe-americas"), color = 'green', width = 12)
  )
  
  output$school_1_black <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      select(`% African American`),
    "Percent African American", icon = icon("globe-americas"), color = 'green', width = 12)
  )
  
  output$school_1_hispanic <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      select(`% Hispanic`),
    "Percent Hispanic", icon = icon("globe-americas"), color = 'green', width = 12)
  )
  
  output$school_1_iep <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      select(`% Students with Disabilites`),
    "Percent of Students with Disabilties", icon = icon("users"), color = 'light-blue', width = 12)
  )
  
  output$school_1_frl <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      select(`% Economically Disadvantaged`),
    "Percent of Students Economically Disadvantaged",icon = icon("users"), color = 'light-blue', width = 12)
  )
  
  output$school_1_ell <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool1) %>% 
      select(`% ELL`),
    "Percent of English Language Learners", icon = icon("users"), color = 'light-blue', width = 12)
  )
  
  
  output$school_2_ela <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      filter(Subject == 'ELA') %>%
      select(`Percent Mastered`),
    "ELA Percent Mastery", icon = icon("book"), color = 'yellow', width = 12)
  )
  
  output$school_2_math <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      filter(Subject == 'Math') %>%
      select(`Percent Mastered`),
    "Math Percent Mastery", icon = icon("calculator"), color = 'yellow', width = 12)
  )
  
  output$school_2_male <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      select(`% Male`),
    "Percent Male ", color = 'blue',icon = icon("male"), width = 12)
  )
  
  output$school_2_female <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      select(`% Female`),
    "Percent Female", color = 'blue', icon = icon("female"), width = 12)
  )
  
  output$school_2_white <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      select(`% White`),
    "Percent White", color = 'green',icon = icon("globe-americas"), width = 12)
  )
  
  output$school_2_black <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      select(`% African American`),
    "Percent African American", color = 'green',icon = icon("globe-americas"), width = 12)
  )
  
  output$school_2_hispanic <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      select(`% Hispanic`),
    "Percent Hispanic", color = 'green', icon = icon("globe-americas"),width = 12)
  )
  
  output$school_2_iep <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      select(`% Students with Disabilites`),
    "Percent of Students with Disabilties", icon = icon("users"),color = 'light-blue', width = 12)
  )
  
  output$school_2_frl <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      select(`% Economically Disadvantaged`),
    "Percent of Students Economically Disadvantaged", icon = icon("users"),color = 'light-blue', width = 12)
  )
  
  output$school_2_ell <- renderValueBox(valueBox(
    compare_info %>% 
      filter(`School Name.x` %in% input$selectSchool2) %>% 
      select(`% ELL`),
    "Percent of English Language Learners", icon = icon("users"),color = 'light-blue', width = 12)
  )
  
  output$graphType <- renderPrint({ 
    input$radio 
    })
  
  output$graphSubject <- renderPrint({ 
    input$radio 
  })
  
  output$graphCluster <- renderPrint({ 
    input$radio 
  })
  
  output$graphMastery <- renderPlot({
    pathway_scores %>% 
      filter(GraphType %in% input$graphType) %>% 
      filter(Subject %in% input$graphSubject) %>%  
      filter(cluster %in% input$graphCluster) %>% 
    ggplot(
      aes(x = cluster, y = value, group = outcome, fill = outcome)
    ) +
      geom_bar(
        stat = "identity",
        position = position_dodge()
      ) +
      labs(x = 'Cluster', y = '% of Students') +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      scale_fill_brewer(name = 'School Type', palette = "Set1", labels = c("Elementary School", "Middle School", "High School"))
  })
  
  
  
  
  
  
  
  
  
  
  #########
  
  output$sites <- renderPlot({
    
    averages_by_site_and_location %>% 
      filter(type %in% input$checkGroup) %>% 
      filter(outcome %in% input$selectVar) %>% 
      ggplot(
        aes(x = outcome, y = value, group=type, fill = type)
      ) +
      geom_bar(
        stat = "identity",
        position = position_dodge()
      )  +
      labs(x = element_blank(), y = 'Percentage', title = 'Demographic Groups')  +
      ylim(0, 70) +
      scale_x_discrete(labels=c("mean_pct_black" = "Black Students", 
                                "mean_pct_white" = "White Students",
                                "mean_pct_male" = "Male Students",
                                "mean_pct_frl" = "Free & Reduced Lunch",
                                "mean_pct_tested" = "Took the SAT")) +
      scale_fill_brewer(name = 'Subgroup', palette = "Paired") 
  })
  
  output$table <- renderTable(
    averages_by_site_and_location %>% 
      filter(type %in% input$checkGroup) %>% 
      filter(outcome %in% input$selectVar) %>% 
      arrange(type)
  )
  
  output$school_data <- renderDataTable(
    all_school_info,
    options = list(
      columnDefs = list(list(width = '250px', targets = list(0)),
                        list(width = '100px', targets = list(1))),
      autoWidth=TRUE,
      scrollX=TRUE
    )
  )
  
  
  output$testing_average <- renderValueBox(valueBox(
    testing_site_t_tests %>% 
      filter(variable %in% input$ttestVar) %>% 
      select(mean_not_testing_sites),
    "Average for Non-Testing Sites", icon = icon("chalkboard"), color = "green"
  ))
  
  output$not_testing_average <- renderValueBox(valueBox(
    testing_site_t_tests %>% 
      filter(variable %in% input$ttestVar) %>% 
      select(mean_testing_sites), 
    "Average for Testing Sites", icon = icon("chalkboard-teacher"), color = "green"
  ))
  
  output$pvalue <- renderValueBox(valueBox(
    testing_site_t_tests %>% 
      filter(variable %in% input$ttestVar) %>% 
      select(p_value), 
    "P-Value", icon = icon("laptop"), color = "green"
  ))
  
  output$ttest <-renderValueBox({
    t <- testing_site_t_tests %>% 
      filter(variable %in% input$ttestVar) %>% 
      select(p_significance)
    
    if (t == 'Significant'){
      valueBox(testing_site_t_tests %>% 
                 filter(variable %in% input$ttestVar) %>% 
                 select(p_significance), 
               "T-Test Result", icon = icon("chart-line"), color = "green")
    }
    else {
      
      valueBox(testing_site_t_tests %>% 
                 filter(variable %in% input$ttestVar) %>% 
                 select(p_significance), 
               "T-Test Result", icon = icon("chart-line"), color = "red")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(all_school_info, file)
    }
  )
})

