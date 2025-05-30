setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/")
if(!require('gganimate')) {
  install.packages('gganimate')
  library('gganimate')
}
if(!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}
if(!require('sf')) {
  install.packages('sf')
  library('sf')
}

# Load necessary libraries
library(pacman)
p_load(tidyverse, lubridate, ggplot2, patchwork, writexl)

# Define country lists
block <- pays <- c("ALGERIA", "BURKINA FASO", "COTE D IVOIRE", "MAURITANIA", "MALI", "GUINEA", "GHANA", "TOGO", "BENIN", "SIERRA LEONE", "LIBERIA", "GUINEA-BISSAU", "THE GAMBIA", "SENEGAL", "NIGERIA", "NIGER", "CAMEROON", "CHAD", "CENTRAL AFRICAN REPUBLIC", "DEMOCRATIC REPUBLIC OF THE CONGO", "CONGO", "GABON", "EQUATORIAL GUINEA", "RWANDA", "BURUNDI", "ANGOLA", "KENYA", "ERITREA", "ETHIOPIA", "SOUTH SUDAN", "UGANDA", "TANZANIA", "MALAWI", "ZAMBIA", "MOZAMBIQUE", "ZIMBABWE", "BOTSWANA", "NAMIBIA", "ESWATINI (KINGDOM)", "LESOTHO(KINGDOM", "SOUTH AFRICA", "MADAGASCAR", "COMOROS", "SEYCHELLES", "MAURITIUS", "CAPE VERDE", "SAO TOME AND PRINCIPE", "UNITED REPUBLIC OF TANZANIA")

# Load shapefiles
all_countries <- read_rds("global.ctry.rds")
all_provinces <- read_rds("global.prov.rds")
all_districts <- read_rds("global.dist.rds")

AFRO_layer <- all_countries |> 
  filter(WHO_REGION == "AFRO")
Africa <- all_countries |> 
  filter(WORLD_CONTINENTS == "AFRICA")

country_layer <- all_countries |>
  filter(ADM0_NAME %in% block)
province_layer <- all_provinces |>
  filter(ADM0_NAME %in% block)
district_layer <- all_districts |>
  filter(ADM0_NAME %in% block)

data <- read_csv("C:/Users/TOURE/Mes documents/REPOSITORIES/SCOPE/scope_.csv")
x<-6
y<-3
# Get the maximum month label
max_month_label <- max(data$round_start_date)
year <- year(data$round_start_date)

# Add floor_date column
data <- data |> mutate(floor_date = floor_date(round_start_date, "month"))

# Calculate the last six months
last_six_months <- seq(max(data$floor_date), by = "-1 month", length.out = x)
data <- data |> 
  filter(floor_date %in% last_six_months)

# map the scope
plots_list <- list()

# Dynamically set the vector to take the unique values of the month column
unique_months <- unique(data$month)

for (selected_month_yr in unique_months) {
  A <- data |>
    filter(month == selected_month_yr) |> 
    filter(country %in% pays) |>
    dplyr::select(country, province, district, vaccine.type)
  
  # join the spatial layer of provinces
  LQAS_performance2 <- left_join(district_layer, A, relationship = "many-to-many",  
                                 by = c("ADM0_NAME" = "country",
                                        "ADM2_NAME" = "district")) |> 
    filter(vaccine.type != "NA")
  
  # plot the result of the analysis
  last_6 <- ggplot() + 
    geom_sf(data = Africa, linewidth = 0.5, color = "black", fill = "grey") +
    geom_sf(data = province_layer, color = NA, fill = "white") +
    geom_sf(data = LQAS_performance2, aes(fill = vaccine.type), color = NA) +
    geom_sf(data = country_layer, linewidth = 0.5, color = "black", fill = NA) +
    scale_fill_manual(values = c("nOPV2" = "cornflowerblue", "bOPV" = "yellow3")) +
    theme_void() +
    labs(fill = paste0("vaccine type"), 
         title = paste("scope in", month(selected_month_yr, label = TRUE), year)) +
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5))
  
  plots_list[[selected_month_yr]] <- last_6
}

# Update the title of the last plot
plots_list[[length(plots_list)]] <- plots_list[[length(plots_list)]] +
  ggtitle(paste("Scope as of", max_month_label))

# Filter out any NULL or invalid entries
valid_plots_list <- Filter(function(x) inherits(x, "ggplot"), plots_list)

# Combine the plots
multiplot <- wrap_plots(valid_plots_list, ncol = y)

# Add source and production date at the bottom of the last plot
multiplot_with_caption <- multiplot + labs(caption = paste("Source: AFR0 SIA calendar, Production date:", Sys.Date()))

# Display the plots
multiplot_with_caption

