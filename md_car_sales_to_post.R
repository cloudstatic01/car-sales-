#library calls

library(tidyverse)
library(dplyr)
library(plotly)
library(ggplot2)
library(knitr)

#getting the data

res = httr::GET("https://opendata.maryland.gov/resource/un65-7ipd.json")
df = jsonlite::fromJSON(httr::content(res, as = "text"), flatten = TRUE)
df = df %>%
  rename_all(tolower)
df = as_tibble(df)

#getting the data ready to plot

df = df %>%
  mutate(month = factor(
    month,
    levels = c("JAN", "FEB", "MAR", "APR",
               "MAY", "JUN", "JUL", "AUG", "SEP",
               "OCT", "NOV", "DEC")),
    month = as.numeric(month),
    new = parse_number(new),
    used = parse_number(used),
    total_sales_new = parse_number(total_sales_new),
    total_sales_used = parse_number(total_sales_used))

#These functions plot and save the data

df <- df %>%
  mutate(mysize = 0.5)
df = df %>%
  mutate(mysize = ifelse(year %in% c(2008, 2009, 2020), 0.75, mysize))

# create one plot
g = df %>%
  ggplot(
    aes(x = month, colour = factor(year),
        alpha = I(mysize),
        size = mysize
    )
  ) +
  scale_size(range = c(0.5, 1.5), guide="none") +
  scale_x_continuous(breaks=1:12) +
  scale_y_continuous(labels = scales::comma)

gnew = g +
  geom_line(aes(y = new)) +
  labs(title = "Fig. 1: Total New Units Sold Per Month" ,
       y= "Total Units",
       x="Month", colour="Year")
gnew

gused = g +
  geom_line(aes(y = used)) +
  labs(title = "Fig. 2: Total Used Units Sold Per Month" ,
       y= "Total Units" , x="Month", colour="Year")
gused

gsale = g + labs(y= "Total Dollar Amount" , x="Month", colour="Year")
gnewsale = gsale +
  geom_line(aes(y = total_sales_new)) +
  labs(title = "Fig. 3: Total New Car Sales ($) Per Month")
gnewsale

gsale = g + labs(y= "Total Dollar Amount" , x="Month", colour="Year")
gusedsale = gsale +
  geom_line(aes(y = total_sales_used)) +
  labs(title = "Fig. 4: Total Used Car Sales ($) Per Month")
gusedsale


#This section calculates the percent change from
# month to month except for January of each year

long = df %>%
  select( -one_of("mysize")) %>%
  gather(metric, value, -year, -month) %>%
  group_by(year, metric) %>%
  arrange(year, metric, month) %>%
  mutate(lag_value = lag(value),
         month1_value = value[month == 1])
long = long %>%
  ungroup() %>%
  # here is our percentage!
  mutate(
    value_m1 = round((value / month1_value - 1)* 100),
    value = round((value / lag_value - 1 )* 100, 1),
  )

wide = long  %>%
  select(-one_of(c("month1_value", "lag_value", "value_m1"))) %>%
  spread(metric, value)

recent_years = wide %>%
  filter(year %in% c(2018, 2019, 2020), month %in% c(1, 2, 3, 4, 5, 6))

recent_year2 = df %>%
  filter(year %in% c(2018, 2019))





g = long %>%
  ggplot(
    aes(x = month,
        colour = factor(year),
    )
  ) +
  scale_size(range = c(0.5, 1.5), guide="none") +
  scale_x_continuous(breaks=1:12) +
  facet_wrap(~ metric, scales = "free_x")

g + geom_line(aes(y = value))

g + geom_line(aes(y = value_m1))