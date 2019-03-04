# https://cran.r-project.org/web/packages/sparklyr/sparklyr.pdf

# Install packages we'll use
if(!require(pacman)) install.packages('pacman')
library(pacman)

# If you don't have this packages, the p_load function will auto install it all
pacman::p_load(sparklyr, dplyr, DBI, ggplot2) 

# Install spark with version 2.1.0 
spark_install(version = "2.1.0")

# Load library
library(sparklyr)
library(dplyr)
library(DBI)
library(ggplot2)

# Unable e-notation
options(scipen=999)

# Recommended properties form RStudio with sparklry official web
conf <- spark_config()
conf$`sparklyr.cores.local` <- 4
conf$`sparklyr.shell.driver-memory` <- "16G"
conf$spark.memory.fraction <- 0.9

# Connecting to Spark
sc <- spark_connect(master = "local", 
                    version = "2.1.0",
                    config = conf)

# Spark web console: the url is http://127.0.0.1:4040/jobs/ 
spark_web(sc)

# ----------------------------------------------------------
# Connecting to Spark with Spark clusters
# conf <- spark_config()
# conf$spark.executor.memory <- "300M"
# conf$spark.executor.cores <- 2
# conf$spark.executor.instances <- 3
# conf$spark.dynamicAllocation.enabled <- "false"
# 
# sc <- spark_connect(master = "yarn-client", 
#                     spark_home = "/usr/lib/spark/",
#                     version = "2.1.0",
#                     config = conf)
# ----------------------------------------------------------

# Load data into spark and conut the time of loading data
path = 'data/'

system.time({
  train_tbl <- spark_read_csv(sc, name = 'train', 
                            path = paste(path, 'train_sample.csv', sep = ''), 
                            header = TRUE, 
                            delimiter = ",")})

system.time({
  test_tbl <- spark_read_csv(sc, name = 'test', 
                              path = paste(path, 'test.csv', sep = ''), 
                              header = TRUE, 
                              delimiter = ",")})


# Quick look up the data
src_tbls(sc)

head(train_tbl, 5)
sdf_dim(train_tbl)


# Disconnect from Spark
spark_disconnect(sc)

# ----------------------------------------------------------------
train_tbl %>% 
  group_by(os) %>% 
  summarise(count = n()) %>% 
  filter(dense_rank(desc(count)) <= 10) %>% 
  ggplot(mapping = aes(x = reorder(os, desc(count)), y = count, fill = os)) +
  geom_bar(stat = 'identity') 




