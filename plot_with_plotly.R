# Reference: 
# https://plot.ly/r/getting-started/#initialization-for-online-plotting
# https://plotly-book.cpsievert.me/index.html

# What is plotly
## Plotly is an R package for creating interactive web-based graphs via the open source 
## JavaScript graphing library plotly.js. As of version 2.0 (November 17, 2015), 
## Plotly graphs are rendered locally through the htmlwidgets framework.

# The official website: If we choose Community plan, there are 25 Public Charts Support.
## https://plot.ly/settings/subscription

# 1. Install plotly Package...
if(!require('plotly')) install.packages('plotly')
library(plotly)

# 2-1. Initialization for Offline Plotting
p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
p

# 2-2. Initialization for Online Plotting

## 2-2-1. Create a free Plotly account: https://plot.ly/api_signup

## 2-2-2. ave your authentication credentials: https://plot.ly/settings/api
## Find your authentication API keys in your online settings. Set them in your R session with:
## Sys.setenv("plotly_username"="your_plotly_username")
## Sys.setenv("plotly_api_key"="your_api_key")

## This is my plotly's setting
Sys.setenv("plotly_username"="chinhungtseng")
Sys.setenv("plotly_api_key"="TeC8zB9fWnyHm6z1aHv2")

# 3. Publish your graphs to Plotly with api_create
# Use filename to title the file in your Plotly account.
# api_create(p, filename = "r-docs-midwest-boxplots")
api_create(p, filename = "r-docs-midwest-boxplots")

# 4. (optional) - Suppress auto open
# When following the instructions above, api_create(p) will auto open the created URL in the browser. 
# To suppress this behavior, you can update your browser options in R:
options(browser = 'false')

# 5. Embed Graphs in website
# https://help.plot.ly/embed-graphs-in-websites/

# ---------------------------------------------------------------------
# Use the TalkingData dataset draw some graph...
# Load libraries
if(!require(tidyverse)) install.packages('tidyverse')
if(!require(lubridate)) install.packages('lubridate')

library(tidyverse)
library(lubridate)

# Disable e notation
options(scipen = 999)

# Set file's path: you can set your path here.
path <- 'data/'

# Load dataset and count the loading time
## train.csv
system.time({
  train <- data.table::fread(input = paste(path, 'train_sample.csv', sep = ''),
                 header = TRUE,
                 sep = ',')
})

system.time({
  train_new <- train %>% 
    as_tibble() %>% 
    mutate(
      click_time = as.POSIXct(strptime(click_time, "%Y-%m-%d %H:%M:%S")),
      attributed_time = as.POSIXct(strptime(attributed_time, "%Y-%m-%d %H:%M:%S")),
      ip = as.factor(ip), 
      is_attributed = is_attributed == 1,
      # convert features below to factor property.
      ip = as.factor(ip), 
      app = as.factor(app),
      device = as.factor(device),
      os = as.factor(os),
      channel = as.factor(channel),
      # # split date-time
      click_day = day(click_time),
      click_hour = hour(click_time),
      click_minute = minute(click_time),
      click_second = second(click_time)
    )
})

# ip_count
ip_count_barchart <- train_new %>% 
  group_by(ip) %>% 
  summarise(count = n()) %>% 
  filter(dense_rank(desc(count)) <= 30) %>% 
  ggplot(mapping = aes(x = reorder(ip, count), y = count, fill = ip)) +
  geom_bar(stat = 'identity', show.legend = FALSE) + 
  coord_flip()

# Convert original plot to plotly
ip_count_barchart <- ggplotly(ip_count_barchart)

# Create plot API and upload to the official website
api_create(ip_count_barchart, filename = "ip_count_barchart")






