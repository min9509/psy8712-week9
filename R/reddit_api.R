# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(httr)
library(jsonlite)

## Data Import and Cleaning
# Import data from Reddit by using JSON
url <- "https://www.reddit.com/r/rstats.json"
rstats_data <- fromJSON(url)

# Creating rstats_tbl by using mutate and select function
rstats_tbl <- rstats_data$data$children %>%
  mutate(post = data$title,
    upvotes = data$ups,
    comments = data$num_comments) %>%
  select(post, upvotes, comments)

## Visualization
# Visualize the relationship between upvotes and comments
ggplot(rstats_tbl, aes(x = upvotes, y = comments)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Number of Upvotes", y = "Number of Comments", title = "The Scatterplot between Upvotes and Comments")



?fromJSON
