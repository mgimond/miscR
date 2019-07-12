# Summarize amounts by week over a multi-year period
# and extract the Monday date for each week

library(dplyr)
library(lubridate)

# Sample data
Date   <- sample(seq(as.Date('2015/01/05'), as.Date('2017/01/01'), by="day"), 2000, replace = TRUE)
amount <- sample(1:2000)
df <- data.frame(Date, amount)

# Define starting date
day.s <- as.Date('2015/01/04')

# Run pipe
df %>% 
  mutate(week_frac = (Date - day.s)/dweeks(1),  # Get the week fraction from start date
         week_inc = floor(week_frac)) %>%       # Remove fraction component
  arrange(Date) %>% 
  group_by(week_inc) %>% 
  summarise(sum.amount = sum(amount, na.rm = TRUE)) %>% 
  mutate(Monday = dweeks(week_inc) + day.s +1,  # Get the Monday date for that week
         Check = wday(Monday, label = TRUE))    # Check that this date falls indeed on a Monday
