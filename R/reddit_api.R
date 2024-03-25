#### Script Settings and Resources #### 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(httr)
library(jsonlite)

#### Data Import and Cleaning #### 
# Import data from Reddit by using JSON
url <- "https://www.reddit.com/r/rstats.json"
rstats_data <- fromJSON(url)

# Creating rstats_tbl by using mutate and select function
rstats_tbl <- rstats_data$data$children %>%
  mutate(post = data$title,
    upvotes = data$ups,
    comments = data$num_comments) %>%
  select(post, upvotes, comments)

#### Visualization #### 
# Visualize the relationship between upvotes and comments
ggplot(rstats_tbl, aes(x = upvotes, y = comments)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Number of Upvotes", y = "Number of Comments", title = "The Scatterplot between Upvotes and Comments")

#### Analysis #### 
# Analyze the correlation coefficient and p-value by using cor.test
cor_test <- cor.test(rstats_tbl$upvotes, rstats_tbl$comments)
cor_test$estimate
cor_test$p.value

#### Publication #### 
# "The correlation between upvotes and comments was r(23) = 0.20, p = 0.34. This test was not statistically significant."

# For publication, prepare df, cor, p, and sig by using $, round, ifelse
df <- cor_test$parameter
cor <- sprintf("%.2f", cor_test$estimate)
p <- sprintf("%.2f", cor_test$p.value)
sig <- ifelse(cor_test$p.value < 0.05, "was", "was not")

# Output the results in the desired format by using sprintf
sprintf("The correlation between upvotes and comments was r(%d) = %s, p = %s. This test %s statistically significant.",
                       df, cor, p, sig)
