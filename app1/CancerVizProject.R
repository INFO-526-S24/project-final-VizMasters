library(shiny)
library(shinythemes)
library(shiny)
library(tidyverse)
library(janitor)

# Read the CSV file
data <- read.csv("data/01_annual-number-of-deaths-by-cause.csv", stringsAsFactors = FALSE)
data <-clean_names(data)

clean_column_names <- function(names) {
  cleaned_names <- gsub("^deaths_|_sex_both_age_all_ages_number$", "", names)
  return(cleaned_names)
}

colnames(data) <- clean_column_names(colnames(data))
dropdown_choices <- colnames(data)[4:length(colnames(data))]

# Print column names and dropdown choices for debugging
print("Column Names:")
print(colnames(data))
print("Dropdown Choices:")
print(dropdown_choices)

getwd()
# Define UI for the shiny app
ui <- fluidPage(theme = shinytheme("cyborg"),
                
                # App title
                div(
                  style = "display: flex; align-items: center;",
                  titlePanel("Cause of Deaths Dashboard")
                ),
                
                
                # Tabbed layout
                tabsetPanel(
                  # First tab
                  tabPanel("Causes of death",
                           # Sidebar panel
                           sidebarPanel(
                             # Input: Select a year
                             selectInput("year1", "Select Year:",
                                         choices = unique(data$year),
                                         selected = unique(data$year)[1]),
                             # Input: Select a cause of death
                             selectInput("cause1", "Select Cause of Death:",
                                         choices = dropdown_choices,
                                         selected = dropdown_choices[1])
                           ),
                           # Main panel
                           mainPanel(
                             # Output: Bar plot
                             plotOutput("bar_plot1", height = "400px")
                           )),
                  
                  # Second tab
                  tabPanel("Cancer deaths by type",
                           # Sidebar panel
                           sidebarPanel(
                             # Input: Select a year
                             selectInput("year2", "Select Year:",
                                         choices = unique(data$year),
                                         selected = unique(data$year)[1]),
                             # Input: Select a cause of death
                             selectInput("cause2", "Select Cause of Death:",
                                         choices = dropdown_choices,
                                         selected = dropdown_choices[1])
                             #Country
                           ),
                           # Main panel
                           mainPanel(
                             # Output: Line plot
                             plotOutput("line_plot2", height = "400px")
                           )),
                  # Add more tabs as needed
                  # third tab
                  tabPanel("Deaths from cancer(by age)",
                           # Sidebar panel
                           sidebarPanel(
                             # Input: Select a year
                             selectInput("year2", "Select Year:",
                                         choices = unique(data$year),
                                         selected = unique(data$year)[1]),
                             # Input: Select a cause of death
                             selectInput("cause2", "Select Cause of Death:",
                                         choices = dropdown_choices,
                                         selected = dropdown_choices[1])
                           ),
                           # Main panel
                           mainPanel(
                             # Output: Line plot
                             plotOutput("line_plot2", height = "400px")
                           )),
                  # 4th tab
                  tabPanel("Cancer death rate by age group,",
                           # Sidebar panel
                           sidebarPanel(
                             # Input: Select a year
                             selectInput("year2", "Select Year:",
                                         choices = unique(data$year),
                                         selected = unique(data$year)[1]),
                             # Input: Select a cause of death
                             selectInput("cause2", "Select Cause of Death:",
                                         choices = dropdown_choices,
                                         selected = dropdown_choices[1])
                           ),
                           # Main panel
                           mainPanel(
                             # Output: Line plot
                             plotOutput("line_plot2", height = "400px")
                           )),
                  
                  # 5th tab
                  tabPanel("Share of population with cancer,",
                           # Sidebar panel
                           sidebarPanel(
                             # Input: Select a year
                             selectInput("year2", "Select Year:",
                                         choices = unique(data$year),
                                         selected = unique(data$year)[1]),
                             # Input: Select a cause of death
                             selectInput("cause2", "Select Cause of Death:",
                                         choices = dropdown_choices,
                                         selected = dropdown_choices[1])
                           ),
                           # Main panel
                           mainPanel(
                             # Output: Line plot
                             plotOutput("line_plot2", height = "400px")
                           ))
                ),
                
                # Custom CSS styles
                tags$head(
                  tags$style(
                    HTML("
      /* Styling for app title */
      .navbar-brand {
       font-size: 30px;
       font-weight: bold;
       color: #333;
      }
      
      /* Styling for tab labels */
      .nav-tabs > li > a {
       font-size: 18px;
       font-weight: bold;
       color: #333;
      }
      
              /* Styling for sidebar panel */
              .sidebarPanel {
            background-color: #f5f5f5;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
            width: 100px; /* Adjust the width as needed */
        }
              
              /* Styling for main panel */
              .mainPanel {
               padding: 20px;
              
              }
              
              /* Styling for plot outputs */
              .plot-output {
               border: 2px solid #ddd;
               border-radius: 10px;
               box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
               background-color: #fff;
              }
              
              /* Styling for dropdown menus */
        .selectize-input {
            border: 1px solid #ced4da;
            border-radius: 5px;
            padding: 8px;
            background-color: #fff;
        }
        
        .selectize-dropdown {
            border: 1px solid #ced4da;
            border-radius: 5px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
            background-color: #fff;
            color: #333;
        }
        
        .selectize-dropdown-content {
            padding: 8px;
        }
        
        .selectize-dropdown-content .option {
            padding: 5px 10px;
        }
        
        .selectize-dropdown-content .highlighted {
            background-color: #f0f0f0;
        }

      ")
                  )
                )
)

# Define server logic
server <- function(input, output) {
  
  # Filter data based on user inputs and create bar plot for first tab
  output$bar_plot1 <- renderPlot({
    if (input$cause1 %in% dropdown_choices) {
      filtered_data <- data %>%
        filter(year == input$year1) %>%
        group_by(entity) %>%
        summarize(deaths = sum(!!sym(input$cause1), na.rm = TRUE)) %>%
        arrange(desc(deaths))
      
      ggplot(filtered_data, aes(x = reorder(entity, deaths), y = deaths)) +
        geom_bar(stat = "identity") +
        labs(title = paste("Deaths due to", input$cause1, "in", input$year1),
             x = "Entity",
             y = "Number of Deaths") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    } else {
      ggplot() + 
        geom_blank() +
        labs(title = "Invalid Selection", 
             subtitle = paste("The selected cause of death '", input$cause1, "' does not exist in the dataset. Please choose a different cause."),
             x = "", y = "")
    }
  })
  
  
  # Filter data based on user inputs and create line plot for second tab
  output$line_plot2 <- renderPlot({
    if (input$cause2 %in% dropdown_choices) {
      filtered_data <- data %>%
        filter(year == input$year2) %>%
        group_by(entity) %>%
        summarize(deaths = sum(!!sym(input$cause2), na.rm = TRUE)) %>%
        arrange(desc(deaths))
      
      ggplot(filtered_data, aes(x = entity, y = deaths, group = 1)) +
        geom_line() +
        labs(title = paste("Trend of deaths due to", input$cause2, "in", input$year2),
             x = "Entity",
             y = "Number of Deaths") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    } else {
      ggplot() + 
        geom_blank() +
        labs(title = "Invalid Selection", 
             subtitle = paste("The selected cause of death '", input$cause2, "' does not exist in the dataset. Please choose a different cause."),
             x = "", y = "")
    }
  })
}

# Run the shiny app
shinyApp(ui = ui, server = server)