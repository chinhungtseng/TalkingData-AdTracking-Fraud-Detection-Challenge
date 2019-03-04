# Load libraries
if(!require(data.table)) install.packages('data.table')
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

## test.csv
# system.time({
#   test <- data.table::fread(input = paste(path, 'test.csv', sep = ''),
#               header = TRUE,
#               sep = ',')
# })

# Quick look up dataset
str(train); # str(test)

# Split click_time and attributed_time to day, hour, minute, second
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
       # attr_day = day(attributed_time),
       # attr_hour = hour(attributed_time),
       # attr_minute = minute(attributed_time),
       # attr_second = second(attributed_time)
     )
})

# Exploratory Data Analysis

## train.csv: count_ip vs is_attributed
train_new %>% count(is_attributed)
ggplot(data = train_new) + 
  geom_bar(mapping = aes(x = is_attributed, fill = is_attributed))

# train.csv: date vs is_attributed
train_new %>% 
  ggplot(mapping = aes(x = click_time)) +
  geom_freqpoly(mapping = aes(color = is_attributed), bins = 500)

# train.csv: 
## ip_count
train_new %>% count(ip) %>% arrange(desc(n))

train_new %>% 
  group_by(ip) %>% 
  summarise(count = n()) %>% 
  filter(dense_rank(desc(count)) <= 30) %>% 
  ggplot(mapping = aes(x = reorder(ip, count), y = count, fill = ip)) +
  geom_bar(stat = 'identity', show.legend = FALSE) + 
  coord_flip()

## app_count
train_new %>% count(app) %>% arrange(desc(n))

train_new %>% 
  group_by(app) %>% 
  summarise(count = n()) %>% 
  filter(dense_rank(desc(count)) <= 10) %>% 
  ggplot(mapping = aes(x = reorder(app, desc(count)), y = count, fill = app)) +
  geom_bar(stat = 'identity') +
  coord_polar()

## device_count
train_new %>% count(device) %>% arrange(desc(n))

train_new %>% 
  group_by(device) %>% 
  summarise(count = n()) %>% 
  filter(dense_rank(desc(count)) <= 10) %>% 
  ggplot(mapping = aes(x = reorder(device, desc(count)), y = count, fill = device)) +
  geom_bar(stat = 'identity')

## os_count
train_new %>% count(os) %>% arrange(desc(n))

train_new %>% 
  group_by(os) %>% 
  summarise(count = n()) %>% 
  filter(dense_rank(desc(count)) <= 10) %>% 
  ggplot(mapping = aes(x = reorder(os, desc(count)), y = count, fill = os)) +
  geom_bar(stat = 'identity') +
  coord_polar()










# --------------------------------------------------------
# worse plot...QQ
train_new %>% 
  group_by(click_day, click_hour) %>% 
  mutate(
    click_count = n(),
    click_time = as.factor(as.POSIXct(strptime(click_time, "%Y-%m-%d %H")))
    ) %>% 
  ggplot(mapping = aes(x = click_time, y = click_count, color = click_day)) + 
  geom_bar(stat = 'identity', show.legend = FALSE)
# --------
categorial_var_count <- train_new %>% 
  summarise(
    ip = n_distinct(ip), 
    app = n_distinct(app),
    os = n_distinct(os), 
    device = n_distinct(device), 
    channel = n_distinct(channel)
  ) %>% t() %>% as.tibble()

categorial_var_count$V2 <- c('ip', 'app', 'os', 'device', 'channel')
categorial_var_count$V2 <- as.factor(categorial_var_count$V2)

ggplot(data = categorial_var_count, mapping = aes(x = reorder(V2, V1), y = V1, fill = V2)) + 
  geom_bar(stat = 'identity') + 
  coord_cartesian(ylim = c(0, 50))

# ------------------------------------------------------------------------------------
train_new %>% 
  group_by(ip) %>% 
  summarise(
    num_is_attributed = sum(is_attributed),
    num_click_time = n_distinct(click_time)
  ) %>% 
  arrange(desc(num_is_attributed))




