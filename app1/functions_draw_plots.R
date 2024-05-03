library(tidyverse) 
library(janitor)
library(scales)
library(ggplot2)
# Function : plot_01
# Plot :  Horizontal bar
# Data File : 01_Annual_Number_Of_death_by_cause

fun_plot_01_DataFile_01 <- function(objdf,intyear,strcountry)
{
  
  # cat("Year",intyear, "Country",dQuote(strcountry))
  strtitle = cat("Causes of death in ",strcountry," in the year ", intyear )
 
  
  df_condition <- objdf |>
    arrange(desc(deaths)) |>
    mutate(causes_of_death = factor(causes_of_death,
                                    levels = rev(unique(causes_of_death))))
  
  
                          
 
  
  # 
  # Generating the plot 
  plot_01 <- ggplot(data = df_condition, aes(x = causes_of_death, y = deaths / 1e6)) +
    geom_bar(stat = "identity", fill = "#3365FF", alpha = 0.7,width = 0.9) +  # Add transparency
    geom_text(
      aes(label = round(deaths, 1)),  # Round deaths to 1 decimal place
      position = position_dodge(width = 0.9),
      hjust = -0.1,
      size = 4,
      color = "black"  # Ensure text is visible against dark background
    ) +
    scale_y_continuous(labels = label_number(suffix = "M")) +
    coord_flip() +  # Flips x and y axes (optional, adjust as needed)
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
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
      y = "Deaths (Millions)",  # Clarify y-axis label
      x = "Causes of Death"
    )
  
  
  
  return(plot_01)
  
}


# Function : plot_02
# Plot -  Horizontal bar 
# Data File : 02_total-cancer-deaths-by-type

fun_plot_02_DataFile_02 <- function(objdf,intyear,strcountry)
{
  
  strtitle = cat("Causes of death by cancers in ",strcountry,"in the year ", intyear )
  
  df_condition <- objdf |>
    arrange(desc(deaths)) |>
    mutate(causes_of_death = factor(cancertype,
                                    levels = rev(unique(cancertype))))
  
  
  df_condition <- objdf |>
    
    arrange(desc(deaths)) |>
    mutate(cancertype = factor(cancertype,
                               levels = rev(unique(cancertype))))
  
  # glimpse(df_condition)
  
  # Generating the plot    
  plot_02 <-ggplot(data = df_condition, aes(x = cancertype,y = deaths / 1e6 )) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(
      aes(label = deaths),
      position = position_dodge(width = 0.5),
      hjust = -0.1,
      size = 4
    ) +
    scale_y_continuous(labels = label_number(suffix = "M")) +
    coord_flip() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1,size =15 ),  # Rotate x-axis labels for readability
      plot.background = element_rect(fill = "black"),   # Set black background
      plot.title = element_text(color = "white"),       # Set white title text
      axis.text = element_text(color = "white"),        # Set white text color for labels
      axis.title.x = element_text(color = "white"),     # Set white x-axis title
      axis.title.y = element_text(color = "white"),      # Set white y-axis title
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      
    )      +
    labs(
      title = strtitle,
      y = "Deaths",
      x = "Causes of death")
  
  return(plot_02)
  
}


fun_plot_02_DataFile_02 <- function(objdf,intyear,strcountry)
{
  
  strtitle = cat("Causes of death by cancers in ",strcountry,"in the year ", intyear )
  
  df_condition <- objdf |>
    arrange(desc(deaths)) |>
    mutate(causes_of_death = factor(cancertype,
                                    levels = rev(unique(cancertype))))
  
  
  df_condition <- objdf |>
    
    arrange(desc(deaths)) |>
    mutate(cancertype = factor(cancertype,
                               levels = rev(unique(cancertype))))
  
  # glimpse(df_condition)
  
  # Generating the plot    
  plot_02 <-ggplot(data = df_condition, aes(x = cancertype,y = deaths / 1e6 )) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(
      aes(label = deaths),
      position = position_dodge(width = 0.5),
      hjust = -0.1,
      size = 4
    ) +
    scale_y_continuous(labels = label_number(suffix = "M")) +
    coord_flip() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1,size =15 ),  # Rotate x-axis labels for readability
      plot.background = element_rect(fill = "black"),   # Set black background
      plot.title = element_text(color = "white"),       # Set white title text
      axis.text = element_text(color = "white"),        # Set white text color for labels
      axis.title.x = element_text(color = "white"),     # Set white x-axis title
      axis.title.y = element_text(color = "white"),      # Set white y-axis title
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      
    )      +
    labs(
      title = strtitle,
      y = "Deaths",
      x = "Causes of death")
  
  return(plot_02)
  
}

fun_plot_03_DataFile_03 <- function(objdf,intyear,strcountry)
{
  

  
  plot_03 <- ggplot() +
    geom_sf(data = objdf, aes(fill = Deaths_Neoplasms), color = "black", size = 0.5) +
    scale_fill_gradient(low = "#fff7bc", high = "#cc4c02", name = "Neoplasm Deaths") +
    labs(title = "Share of population with cancer") +  
    theme(legend.position = "bottom") +
    transition_time(Year) +  
    ease_aes('linear')+
    theme_void()
  
  
  
  return(plot_03)
  
}