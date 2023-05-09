# Calling required libraries & setting r environment
library(shiny)
library(tidyverse)

ui = fluidPage(
  
  # Application title (I added for aesthetics)
  titlePanel(
    h1("Spring 2023 PSY 8960 Final Exam - People Dashboard",
    h4("By Vivien Lee"))
  ),
  
  # Sidebar with a select input for 
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color:[#ADD8E6];}"),
      width = 2,
      # This is a selector for outcome variable
      selectInput("outcome_var", "Select Outcome Variable:",
                  selected = "None",
                  choices = c("Monthly Income" = "MonthlyIncome",
                              "Turnover Status" = "Attrition",
                              "Job Satisfaction" = "JobSatisfaction",
                              "None" = "None")),
      
      # Here we include a list of selectors for subsetting results 
      selectInput("department", "Department",
                  selected = "All",
                  choices = c("Sales", "Research & Development", "Human Resources", "All")),
      
      selectInput("gender", "Employee Gender",
                  selected = "All",
                  choices = c("Female", "Male", "All")),
      
      selectInput("education_field", "Field of Education",
                  selected = "All",
                  choices = c("Life Sciences", "Medical", "Marketing", "Technical Degree", "Human Resources", "Other", "All")),
      
      selectInput("job_role", "Job Role",
                  selected = "All",
                  choices = c("Sales Executive", "Research Scientist", "Laboratory Technician",
                              "Manufacturing Director", "Healthcare Representative", "Manager",
                              "Sales Representative", "Research Director", "Human Resources", "All"))
      
    ),
    # Display interactive histogram 
    mainPanel(width = 6,
      h2("Univariate Visualization"),
      plotOutput("histo"),
      textOutput("selected_outcome"),
      h2("Output Table"),
      tableOutput("output_tbl")
    )
  ) 
)


server = function(input, output){
  
  # Loading in the RDS object as data once 
  dashboard_data = readRDS("./people_dashboard_dat.rds")
  
  # Creating reactive data object for univariate visualization
  dataInput =  reactive({
  # 1. Selecting the outcome of interest & to slim down data even more
  dashboard_data = dashboard_data %>%
    select(input$outcome_var, Department, EducationField, Gender, JobRole)

  # 2. Subsetting data by selected columns 
  if(input$department != "All"){
    dashboard_data = dashboard_data %>% filter(Department == input$department)
  }
  if(input$gender != "All"){
    dashboard_data = dashboard_data %>% filter(Gender == input$gender)
  }
  if(input$education_field != "All"){
    dashboard_data = dashboard_data %>% filter(EducationField == input$education_field)
  }
  if(input$job_role != "All"){
    dashboard_data = dashboard_data %>% filter(JobRole == input$job_role)
  }
  return(dashboard_data)
  })
  
  output$selected_outcome = renderText({
    paste0("Selected variable: ", input$outcome_var)})
  
  # Rendering output ggplot to show histogram or bar graph
  output$histo = renderPlot({
    df = dataInput()
    
    # Creating histogram if selected outcome is not Turnover
    if(input$outcome_var != "Attrition"){
        hist_title = paste0("Histogram for outcome: ", input$outcome_var)
        ggplot(df, aes_string(x = input$outcome_var, fill = input$outcome_var)) +
          geom_histogram(bins = 30, fill = "lightblue") +
          theme_minimal() + 
          labs(title = hist_title) +
          theme(axis.text = element_text(size = 15),
                title = element_text(size = 15))
    }
    # Creating bar graph if selected outcome is Turnover
    else{
        bplot_title = paste0("Counts for ", input$outcome_var)
        ggplot(df, aes_string(x = input$outcome_var, fill = input$outcome_var)) +
          geom_bar(width = 0.7) + 
          theme_minimal() + 
          labs(title = bplot_title) + 
          theme(legend.position = "bottom",
                axis.text = element_text(size = 15),
                title = element_text(size = 15))
    }
  })
  
  
  # Rendering output table to display mean and sd
  output$output_tbl = renderTable({
    df_table = dashboard_data %>% 
      mutate(Attrition = ifelse(Attrition == "No", 0, 1)) 
    
    # Filtering to get subset columns for group_by()  
    subset_options = c("Department", "Gender", "EducationField", "JobRole")
    subset_input = c(input$department, input$gender,input$education_field, input$job_role)
    groupings = subset_options[subset_input != "All"] 
    
    if(input$outcome_var != "None"){ # Once the person has selected an outcome of interest
        if(length(groupings) == 0 ){ # When not subsetted by any group 
          df_table %>%
            summarize("Average" = mean(eval(sym(input$outcome_var)), na.rm = TRUE),
                      "Standard Deviation" = sd(eval(sym(input$outcome_var)), na.rm = TRUE))
        }else{ # When subsetted by one or more groups
          df_table %>%
            group_by(!!! syms(groupings)) %>% 
            summarize("Average" = mean(eval(sym(input$outcome_var)), na.rm = TRUE),
                      "Standard Deviation" = sd(eval(sym(input$outcome_var)), na.rm = TRUE))
        }
      
    }

  })
}

shinyApp(ui = ui , server = server)
