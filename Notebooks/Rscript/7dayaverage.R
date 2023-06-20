install.packages("tidyverse")
install.packages('slider')
install.packages('tidyquant')

library(slider)
library(tidyverse)
library(logistf)
library(tidyquant)

dataset <- read.csv('Notebooks/Data/out.csv')
dataset[dataset == ''] <- 'Unknown'
dataset[is.na(dataset)] <- 'Unknown'
dataset$admission_date <- as.Date(dataset$admission_date)
# make dataset of daily counts
daily_counts <- dataset %>% 
  count(admission_date, name = "new_cases")
#summary(daily_counts)
#head(daily_counts, 20)

rolling <- daily_counts %>% 
  mutate(                                # create new columns
    # Using slide_dbl()
    ###################
    reg_7day = slide_dbl(
      new_cases,                         # calculate on new_cases
      .f = ~sum(.x, na.rm = T),          # function is sum() with missing values removed
      .before = 6),                      # window is the ROW and 6 prior ROWS
    
    # Using slide_index_dbl()
    #########################
    indexed_7day = slide_index_dbl(
      new_cases,                       # calculate on new_cases
      .i = admission_date,       # indexed with date_onset 
      .f = ~sum(.x, na.rm = TRUE),     # function is sum() with missing values removed
      .before = days(6))               # window is the DAY and 6 prior DAYS
  )
plt <- ggplot(data = rolling) +
  geom_line(mapping = aes(x = admission_date, y = indexed_7day), size = 1) + 
  ylab('Cases') + 
  xlab('Date')
plt
