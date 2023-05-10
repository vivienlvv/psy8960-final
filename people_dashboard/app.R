# Calling required libraries & setting r environment
library(shiny)
library(tidyverse)

ui = fluidPage(
  
  # Application title (I added for aesthetics)
  titlePanel(
    h1("Spring 2023 PSY 8960 Final Exam - People Dashboard",
    h4("By Vivien Lee"))),
  
  
  # Sidebar with a select input for 
  sidebarLayout(
    sidebarPanel(
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
    # Display Plots and Tables
    mainPanel(
      width = 6,
      plotOutput("histo"),
      tableOutput("output_tbl")
    )
  ) 
)

server = function(input, output){
  
  # Loading in the RDS object as data  
  dashboard_data = readRDS("people_dashboard_dat.rds")

  
  # Rendering output ggplot to show histogram or bar graph
  output$histo = renderPlot({
    
    if(input$outcome_var != "None"){
      
      # We will slim down the dataset even further 
      
      ## 1. Selecting the outcome of interest & to slim down data even more
      df = dashboard_data %>%
        select(input$outcome_var, Department, EducationField, Gender, JobRole)
      ## 2. Subsetting data by selected columns 
      if(input$department != "All"){
        df = df %>% filter(Department == input$department)
      }
      if(input$gender != "All"){
        df = df %>% filter(Gender == input$gender)
      }
      if(input$education_field != "All"){
        df = df %>% filter(EducationField == input$education_field)
      }
      if(input$job_role != "All"){
        df = df %>% filter(JobRole == input$job_role)
      }
      
      
      # Creating univariate visualizations
      
      ## Creating histogram if selected outcome is Monthly Income
      if(input$outcome_var == "MonthlyIncome"){
        hist_title = "Histogram for Monthly Income"
        ggplot(df, aes_string(x = input$outcome_var, fill = input$outcome_var)) +
          geom_histogram(bins = 30, fill = "lightblue") +
          theme_minimal() + 
          labs(title = hist_title,
               x = "Monthly Income", y = "Frequency") +
          theme(axis.text = element_text(size = 15),
                title = element_text(size = 15))
      } ## Creating bar graph if selected outcome is Turnover or Job Satisfaction
      else{
        x_axis_title = ifelse(input$outcome_var == "Attrition", "Turnover", "Job Satisfaction")
        bplot_title = paste0("Bar graph for ", x_axis_title)
        ggplot(df, aes_string(x = input$outcome_var, fill = input$outcome_var)) +
          geom_bar(width = 0.7, fill = "lightblue") + 
          theme_minimal() + 
          labs(title = bplot_title,
               x = x_axis_title, y = "Employee Count") + 
          theme(legend.position = "bottom",
                axis.text = element_text(size = 15),
                title = element_text(size = 15))
      }
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
    
    # Once the person has selected an outcome of interest
    if(input$outcome_var != "None"){ 
        if(length(groupings) == 0 ){ # When not subsetted by any group 
          df_table %>%
            # eval() and sym() are used becasue shiny is not evaluating input$outcome_var properly in the context of df_table
            summarize("Average" = mean(eval(sym(input$outcome_var)), na.rm = TRUE),
                      "Standard Deviation" = sd(eval(sym(input$outcome_var)), na.rm = TRUE))
        }else{ # When subsetted by one or more groups
          df_table %>%
            ## I used syms() to turn the subset vector into symbols 
            ### !!! is used to evaluate a list of expressions
            group_by(!!! syms(groupings)) %>% 
            summarize("Average" = mean(eval(sym(input$outcome_var)), na.rm = TRUE),
                      "Standard Deviation" = sd(eval(sym(input$outcome_var)), na.rm = TRUE))
        }
    }
  })
}

shinyApp(ui = ui , server = server)
# rsconnect::deployApp('./people_dashboard/')