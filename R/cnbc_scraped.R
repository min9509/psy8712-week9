#### Script Settings and Resources #### 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(rvest)

#### Data Import and Cleaning #### 
# Define the URLs for Business, Investing, Tech, and Politics pages
cnbc_html <- c(
  "Business" = "https://www.cnbc.com/business/",
  "Investing" = "https://www.cnbc.com/investing/",
  "Tech" = "https://www.cnbc.com/technology/",
  "Politics" = "https://www.cnbc.com/politics/"
)

# Create an empty data frame to store results
cnbc_tbl <- tibble()

# Create loop through each URL to scrape headlines by using read_html
for (source in names(cnbc_html)) {
  # Read the HTML content of the page
  cnbc_page <- read_html(cnbc_html[source])
  
  # Extract headlines and calculate lengths by using _nodes and _text based on the url
  headlines <- cnbc_page %>%
    html_nodes(".Card-title") %>%
    html_text(trim = TRUE)
  
  # Calculates the length (number of words) for each headline by using sapply
  lengths <- sapply(headlines, function(x) length(unlist(strsplit(x, "\\s+"))))
  
  # Create a data frame 
  variables_tbl <- tibble(
    headline = headlines,
    length = lengths,
    source = source
  )
  
  # Combine cnbc_tbl and source_tbl to cnbc_tbl
  cnbc_tbl <- bind_rows(cnbc_tbl, variables_tbl)
}

# Checking the results
cnbc_tbl

#### Visualization #### 
# Visualize the relationship between source and length
ggplot(cnbc_tbl, aes(x = source, y = length, fill = source)) +
  geom_bar(stat = "identity") + 
  labs(x = "Section", y = "Number of Length", title = "Bar Plot of Headline Length by Source") 

#### Analysis #### 
# Runs an ANOVA to get F-value and p-value (using summary to get ANOVA table)
aov_result <- aov(length ~ source, data = cnbc_tbl)
aov_table <- summary(aov_result)
aov_table

#### Publication #### 
# Extract F-value, degrees of freedom, and p-value from the ANOVA table
F_value <- aov_table[[1]]$"F value"[1]
dfn <- aov_table[[1]]$Df[1]
dfd <- aov_table[[1]]$Df[2]
p_value <- aov_table[[1]]$"Pr(>F)"[1]
significant <- ifelse(p_value < 0.05, "was", "was not")

F_value
dfn
dfd
p_value

# Format the F-statistic, degrees of freedom, and p-value by using sprintf
formatted_F <- sprintf("%.2f", F_value)
formatted_df <- sprintf("%.0f", dfn)
formatted_dfd <- sprintf("%.0f", dfd)
formatted_p <- sprintf("%.2f", p_value)

formatted_F
formatted_df
formatted_dfd
formatted_p

# Output the results in the desired format by using sprintf
sprintf("The results of an ANOVA comparing lengths across sources was F(%s, %s) = %s, p = %s. This test %s statistically significant.",
                  formatted_df, formatted_dfd, formatted_F, formatted_p, significant)

