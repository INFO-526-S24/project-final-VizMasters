
# Loading Packages
if (!require("pacman"))
  iunstall::packages("pacman")

pacman::p_load(tidyverse,
               here,
               janitor,
               scales,shiny,shinythemes)

library(tidyverse) 
library(janitor)
library(scales)
library(shiny)
library(shinythemes)
source("functions_data_wrangling.R")
source("functions_draw_plots.R")




# Load your dataframes here

df1 <- fun_DataFile_01()
df2 <- fun_DataFile_02()
df3 <- fun_DataFile_03()

head(df2)
# Define the UI
ui <- fluidPage(theme = shinytheme("cyborg"),
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
                  size = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Causes of death", plotOutput("plot1")),
        tabPanel("Death by cancer", plotOutput("plot2")),
        tabPanel("Plot 3", plotOutput("plot3")),
        tabPanel("Plot 4", plotOutput("plot4")),
        tabPanel("Plot 5", plotOutput("plot5")),
        tabPanel("Plot 6", plotOutput("plot6")),
        tabPanel("Plot 7", plotOutput("plot7")),
        tabPanel("Plot 8", plotOutput("plot8")),
        tabPanel("Plot 9", plotOutput("plot9"))
      ),
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Function to filter data
  filter_data <- function(df) {
    df %>%
      filter(country %in% input$country, year %in% input$year)
  }
  
  # Plot functions
  plot_data1 <- reactive({ fun_plot_01_DataFile_01(filter_data(df1),input$country,input$year) })
  plot_data2 <- reactive({ fun_plot_02_DataFile_02(filter_data(df2),input$country,input$year) })
  plot_data3 <- reactive({ fun_plot_03_DataFile_03(filter_data(df3),input$country,input$year) })
  #plot_data4 <- reactive({ plot_function4(filter_data(df4)) })
  #plot_data5 <- reactive({ plot_function5(filter_data(df5)) })
  #plot_data6 <- reactive({ plot_function6(filter_data(df6)) })
  #plot_data7 <- reactive({ plot_function7(filter_data(df7)) })
  #plot_data8 <- reactive({ plot_function8(filter_data(df8)) })
  #plot_data9 <- reactive({ plot_function9(filter_data(df9)) })
  
  # Render plots
  #output$plot1 <- renderPlot({ plot_data1() })
  
  
output$plot1 <- renderPlot({
    plot_data <- plot_data1()
    # Increase plot size
    options(repr.plot.width = 8, repr.plot.height = 6)
    # Plot with title
    plot_data +
      ggtitle(paste("Causes of death by cancers in", input$country, "in the year", input$year))
})
  
  
output$plot2 <- renderPlot({
  plot_data <- plot_data2()
  # Increase plot size
  options(repr.plot.width = 8, repr.plot.height = 6)
  # Plot with title
  plot_data +
    ggtitle(paste("Causes of death by cancers in", input$country, "in the year", input$year))
})

output$plot3 <- renderImage({
  plot_data <- plot_data3()
  # Generate animation
  anim <- gganimate::animate(plot_data, nframes = 100, fps = 10, end_pause = 10)
  # Save animation as a temporary file
  outfile <- tempfile(fileext='.gif')
  gganimate::anim_save(outfile, anim)
  # Return the path to the saved animation file
  list(src = outfile, contentType = 'image/gif')
}, deleteFile = TRUE)

  #output$plot3 <- renderPlot({ plot_data3() })
  #output$plot4 <- renderPlot({ plot_data4() })
  #output$plot5 <- renderPlot({ plot_data5() })
  #output$plot6 <- renderPlot({ plot_data6() })
  #output$plot7 <- renderPlot({ plot_data7() })
  #output$plot8 <- renderPlot({ plot_data8() })
  #output$plot9 <- renderPlot({ plot_data9() })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
