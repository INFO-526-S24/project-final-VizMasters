
# Loading Packages
if (!require("pacman"))
  iunstall::packages("pacman")

pacman::p_load(tidyverse,
               here,
               janitor,
               scales,shiny,shinythemes,shinyjs )

library(tidyverse) 
library(janitor)
library(scales)
library(shiny)
library(shinythemes)
library(shinyjs)
source("functions_data_wrangling.R")
source("functions_draw_plots.R")




# Load your dataframes here

df1 <- fun_DataFile_01()
df2 <- fun_DataFile_02()
df3 <- fun_DataFile_03()
df4 <- fun_DataFile_04()
df5 <- fun_DataFile_05()

head(df2)
# Define the UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                shinyjs::useShinyjs(),
  titlePanel("Causes of death - Analysis"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$head(
        tags$style(
          HTML("
          .sidebar {
            background-color: #222222; /* Base color */
            color: #ffffff; /* Text color */
            border-radius: 5px; /* Rounded corners */
            background-image: linear-gradient(to bottom, #222222 0%, #333333 100%); /* Optional subtle gradient */
          }
          .sidebar select {
            background-color: #555555;
            color: #ffffff;
            border-color: #555555;
            border-radius: 3px; /* Rounded corners for inputs */
            padding: 5px 10px; /* Adjust padding for better spacing */
          }
          .sidebar select:focus {
            border-color: #66afe9;
          }
          .selectize-control .selectize-input { /* Hover effect for select inputs */
            background-color: #EBEBEB; /* Slight background change on hover */
          }
          /* Targeting select label text */
          .sidebar label {
            color: #ffffff; /* Set white color for select labels */
          }
        ")
        )
      ),
      selectInput("country", "Select Country", unique(c(df1$country, df2$country,df3$country)),
                  selected = NULL,
                  multiple = FALSE,
                  selectize = TRUE,
                  size = NULL
                  ),
      selectInput("year", "Select Year", unique(c(df1$year, df2$year,df3$year)),
                  selected = NULL,
                  multiple = FALSE,
                  selectize = TRUE,
                  size = NULL),
      uiOutput("causes_of_death_ui")
      
      
      
    ),
    mainPanel(
      tabsetPanel(
        id = "mainTabset",
        tabPanel("Causes of death", plotOutput("plot1")),
        tabPanel("Death by cancer", plotOutput("plot2")),
        tabPanel("Plot 3", plotOutput("plot3")),
        tabPanel("Death rate by Age group", plotOutput("plot4")),
        tabPanel("Population of Cancer by Age group", plotOutput("plot5")),
        tabPanel("Plot 6", plotOutput("plot6")),
        tabPanel("Plot 7", plotOutput("plot7")),
        tabPanel("Plot 8", plotOutput("plot8"))
      ),
    )
  )
)


  
server <- function(input, output) {
 
  # Function to filter data
  filter_data <- function(df) {
    if ("country" %in% colnames(df)) {
      df %>%
        filter(country %in% input$country, year %in% input$year)
    } else {
      df %>%
        filter(Entity %in% input$country)
    }
  }
  
  # Plot functions
  plot_data1 <- reactive({ fun_plot_01_DataFile_01(filter_data(df1),input$country,input$year, input$causes_of_death) })
  plot_data2 <- reactive({ fun_plot_02_DataFile_02(filter_data(df2),input$country,input$year) })
  plot_data3 <- reactive({ fun_plot_03_DataFile_03(filter_data(df3),input$country,input$year) })
  plot_data4 <- reactive({ fun_plot_04_DataFile_04(filter_data(df4), input$country, input$year) })
  plot_data5 <- reactive({ fun_plot_05_DataFile_05(filter_data(df5), input$country, input$year) })
  
  # Render plots
  output$plot1 <- renderPlot({
    plot_data <- plot_data1()
    options(repr.plot.width = 8, repr.plot.height = 6)
    plot_data +
      ggtitle(paste("Causes of death in", input$country, " for the year", input$year))
  })
  
  output$plot2 <- renderPlot({
    plot_data <- plot_data2()
    options(repr.plot.width = 8, repr.plot.height = 6)
    plot_data +
      ggtitle(paste("Causes of death by cancers in", input$country, " for the year", input$year))
  })
  
  output$plot3 <- renderImage({
    plot_data <- plot_data3()
    anim <- gganimate::animate(plot_data, nframes = 100, fps = 10, end_pause = 10)
    outfile <- tempfile(fileext='.gif')
    gganimate::anim_save(outfile, anim)
    list(src = outfile, contentType = 'image/gif')
  }, deleteFile = TRUE)
  
  output$plot4 <- renderPlot({ plot_data4() })
  
  
  output$plot5 <- renderPlotly({
    plot_data <- plot_data5()
    options(repr.plot.width = 8, repr.plot.height = 6)
    #plot_data +
      ggplotly(plot_data, tooltip = "text") |>
      layout(legend = list(orientation = "h", y = -0.3, yanchor = "bottom", x = 0.5, xanchor = "center", title = list(text = " Age Group ", x = 0.5)))
    
  })
  
  

  
  
  
  
  
  output$causes_of_death_ui <- renderUI({
    if (input$mainTabset == "Causes of death") {
      selectInput("causes_of_death", 
                  "Select cause of death", 
                  choices = c("Select cause of death" = "", unique(df1$causes_of_death)),
                  selected = NULL,  # No default selection
                  multiple = FALSE,
                  selectize = TRUE,
                  size = NULL
      )
    }
  })
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)



