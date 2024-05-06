library(tidyverse) 
library(janitor)
library(scales)
library(ggplot2)
library(plotly)
# Function : plot_01
# Plot :  Horizontal bar
# Data File : 01_Annual_Number_Of_death_by_cause

fun_plot_01_DataFile_01 <- function(objdf,intyear,strcountry)#,strCauseOfDeath)
{
  
  # cat("Year",intyear, "Country",dQuote(strcountry))
  strtitle = cat("Causes of death in ",strcountry," in the year ", intyear )
 #  
 # #Filter cause of death 
 #  if (causeofDeath != "") {
 #    objdf <- objdf %>%
 #      filter(causes_of_death %in% causeofDeath)
 #  }
  
  
  df_condition <- objdf |>
    arrange(desc(deaths)) |>
    mutate(causes_of_death = factor(causes_of_death,
                                    levels = rev(unique(causes_of_death))))
  
  
                          
 
  
  # 
  # Generating the plot 
  plot_01 <- ggplot(data = df_condition, aes(x = causes_of_death, y = deaths)) +
    geom_bar(stat = "identity", fill = "#3365FF", alpha = 0.7,width = 0.9) +  # Add transparency
    #geom_text(
      #aes(label = round(deaths, 1)),  # Round deaths to 1 decimal place
      #position = position_dodge(width = 0.9),
      #hjust = -0.1,
      #size = 4,
      #color = "black"  # Ensure text is visible against dark background
    #) +
    #scale_y_continuous() +
    scale_y_continuous(labels = label_number(big.mark = ","))+
    
    coord_flip() +  # Flips x and y axes (optional, adjust as needed)
    theme_minimal() +
    theme(
      axis.text.x = element_text(),  # Rotate x-axis labels for readability
      plot.background = element_rect(fill = "black"),   # Set black background
      plot.title = element_text(color = "white"),       # Set white title text
      axis.text = element_text(color = "white"),        # Set white text color for labels
      axis.title.x = element_text(color = "white"),     # Set white x-axis title
      axis.title.y = element_text(color = "white"),      # Set white y-axis title
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = strtitle,
      y = "Deaths",  # Clarify y-axis label
      x = "Causes of Death"
    )
  
  
  
  return(plot_01)
  
}


# Function : plot_02
# Plot -  Horizontal bar 
# Data File : 02_total-cancer-deaths-by-type

fun_plot_02_DataFile_02 <- function(objdf,intyear,strcountry)
{
  
  
  
}


fun_plot_02_DataFile_02 <- function(objdf,intyear,strcountry)
{
  
  strtitle = cat("Causes of death by cancers in ",strcountry,"in the year ", intyear )
  
  df_condition <- objdf 
   
  
  plot_02 <- ggplot(data = df_condition, aes(x = reorder(cancertype, deaths), y = deaths)) +
    geom_bar(stat = "identity", fill = "#3365FF", alpha = 0.7,width = 0.9) +
    geom_text(
      aes(label = deaths),
      position = position_dodge(width = 0.9),
      hjust = -0.1,
      size = 4
    ) +
    coord_flip() +
    scale_y_continuous(labels = label_number(big.mark = ","))+
    theme_minimal()+
    theme(
      plot.background = element_rect(fill = "black"),
      panel.grid.major = element_line(color = "black", size = 0.5),
      panel.grid.minor = element_line(color = "black", size = 0.25),
      text = element_text(color = "white"),                 # Changes color of all text by default
      axis.title = element_text(color = "white"),           # Specifically changes the axis titles
      axis.text = element_text(color = "white"),            # Changes the axis tick text
      plot.title = element_text(color = "white")
    ) +
    labs(
      title = strtitle,
      y = "Deaths",
      x = "Type of cancers"
    )
  
  
  return(plot_02)
  
}

fun_plot_03_DataFile_03 <- function(objdf,intyear,strcountry)
{
  
  plot_03 <- ggplot() +
    geom_sf(data = objdf, aes(fill = Deaths_Neoplasms), color = "black", size = 0.5) +
    scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = NULL, labels = scales::label_number(suffix = "% ")) +
    labs(title = "Count of cancer cases per 100 people in the population") +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color = "white"),
      axis.text = element_text(color = "black"),        # Set white text color for labels
      axis.title.x = element_text(color = "white"),     # Set white x-axis title
      axis.title.y = element_text(color = "white"),  
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(color = "white")
      
      
          
      
    )
  
  #plot_03 <- ggplotly(p, tooltip = "text", dynamicTicks = TRUE, highlight = "plotly_selected")
  
  

  return(plot_03)
  
}

fun_plot_04_DataFile_04 <- function(df, strcountry, intyear) {
  filtered_data <- df %>%
    filter(country == strcountry, year == as.numeric(intyear)) %>%
    pivot_longer(
      cols = c(`70+ years`, `50-69 years`, `15-49 years`, `5-14 years`, `Under 5 years`),
      names_to = "age_group",
      values_to = "rate"
    )
  
  plot_04 <- ggplot(filtered_data, aes(x = reorder(age_group, rate), y = rate, fill = age_group)) +
    geom_bar(stat = "identity") +
    labs(
      title = paste("Death Rates from Neoplasms in ",strcountry,  " for ", intyear),
      x = "Age Group",
      y = "Death Rate per 100,000 People"
    ) +
    scale_fill_viridis_d() +
    theme_minimal() +
    coord_flip() +
    theme(
      
      plot.background = element_rect(fill = "black"),
      plot.title = element_text(color = "white"),
      axis.text = element_text(color = "white"),        # Set white text color for labels
      axis.title.x = element_text(color = "white"),     # Set white x-axis title
      axis.title.y = element_text(color = "white"),
      panel.grid.major = element_line(color = "black", size = 0.5),
      panel.grid.minor = element_line(color = "black", size = 0.25),
      #axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(color = "white"),
      legend.box = "horizontal"
    )
  
  return(plot_04)
}

fun_plot_05_DataFile_05 <- function(df) {
  

  
  # Optionally, you can also convert values_to to numeric to ensure it only contains finite numbers
  
  plot_5 <-ggplot(df, aes(x = year, y = prevalence / 1e6, color = age_group, group = age_group, text= paste("Year:", Year, "<br>Number Of People:", prevalence))) +
    geom_line(size=1.5) +
    geom_point(size=2.5) +
    scale_colour_viridis_d() +
    scale_x_continuous(breaks = seq(min(df$Year), max(df$Year), by = 5)) +
    scale_y_continuous(labels = label_number(suffix = "M")) +
    labs(title = "Trends in Cancers Prevalence by Age Group around the world",
         x = "Year",
         y = "Number of People with Cancers") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "black"),
          plot.title = element_text(color = "white"),
          axis.text = element_text(color = "white"),        # Set white text color for labels
          axis.title.x = element_text(color = "white"),     # Set white x-axis title
          axis.title.y = element_text(color = "white"),
          panel.grid.major = element_line(color = "white", size = 0.25),
          panel.grid.minor = element_line(color = "black", size = 0.25),
          legend.title = element_text("Age Group"),
          
          legend.text = element_text(color = "white"),
          legend.position = NULL)
  
  
  
  return(plot_5)
  
  
  
}
