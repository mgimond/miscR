library(lubridate)
library(dplyr)    # All data manipulation with %>% 
library(tidyr)    # Used to convert table to long
library(ggplot2)  
library(scales)   # Makes labeling in ggplot2 easier

# Define the web link
fed <- "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H15&series=bf17364827e38702b42a58cf8eaa3f78&lastobs=&from=&to=&filetype=csv&label=include&layout=seriescolumn&type=package"

# Grab the first row only (this will store the column names)
header <- unlist(read.csv(fed, nrows = 1, check.names = FALSE, header=FALSE))

# Construct a dataframe named "rates" omitting the text in the first 5 rows
rates <- read.csv(fed, skip=6, header=FALSE, na.strings = "ND")

# I pulled the recession data from a website. Here, I'm building the dataframe
recession = read.table(textConnection(
  "Start, End
1969-12-01, 1970-11-01
1973-11-01, 1975-03-01
1980-01-01, 1980-07-01
1981-07-01, 1982-11-01
1990-07-01, 1991-03-01
2001-03-01, 2001-11-01
2007-12-01, 2009-06-01"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

# Number of months corresponding to each of the interest rates in the
# data frame by column
term_months <- factor(c(1,3,6,12,24,36,60,84,120,240,360))

# Create rates lookup tables
# (this will store the full column name, term_months, and default column name)
rate_lookup <- data.frame( name = header[-1], term = term_months,
                           colname = paste0("V", 2:12))

# Data manipulation
# This generates a long (tidy) version of the table that will work
# well with ggplot2 and many statisticcal packagfes in R. I also
# assigns the long column name to each record for documentation
rates2 <- rates %>% 
  mutate(Date = ymd(V1)) %>%  # Create date object
  select( -V1) %>%           
  gather(key = "rate_type", value = "rate", -Date) %>% 
  left_join(rate_lookup , by = c("rate_type" = "colname"))


# Here, 'term' is assigned to the color aesthetic 
# (but it's hard to read with so many overlapping lines)
ggplot(rates2, aes(x=Date, y=rate, col=term)) + geom_line()

# Let's facet the plots instead (note that this takes up much of the screen)
# I'm also adding the recession bars. 
# Since two separate dataframes are used in this plot, I need to 
# explicitly assign each data frame to each geom function
ggplot() + 
  geom_line(data=rates2, aes(x=Date, y=rate)) + 
  facet_wrap(~ term) +
  geom_rect(data=recession, aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill="red", alpha=0.1)


# Once the table is in long form (rates2), you can easily manipulate it to do whatever.
# For example, to calculate the difference between 5 year and 2 year rates:
rates_diff <- rates2 %>%
  select(Date, rate, term) %>% 
  filter(term == 24 | term == 60) %>% 
  spread(key = term, value = rate) %>% 
  mutate(diff = `60` - `24`) %>% 
  filter( !is.na(diff)) 

ggplot() + 
  geom_line(data = rates_diff, aes(x = Date, y = diff)) +
  geom_hline(yintercept = 0) + 
  geom_rect(data=recession, aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill="red", alpha=0.1, col = "red")


# To control date interval and range along the x-axis
ggplot() + 
  geom_line(data = rates_diff, aes(x = Date, y = diff)) +
  geom_hline(yintercept = 0) + 
  geom_rect(data=recession, aes(xmin=Start, xmax=End, ymin=-Inf, ymax=+Inf), 
            fill="red", alpha=0.1, col = "red") +
  scale_x_date(date_breaks = "2 year", 
               labels = date_format("%Y"),
               limits = as.Date(c('1980-01-01','2019-01-01')))
