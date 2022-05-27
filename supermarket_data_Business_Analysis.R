supermarket <- read.csv("supermarket_sales.csv")

library(tidyverse)
library(lubridate)
library(scales)
# install.packages("hrbrthemes")
library(hrbrthemes)
# install.packages("extrafont")
library(extrafont)

supermarket %>% view()
glimpse(supermarket)

## Revise Date and Time columns
supermarket <- supermarket %>% 
  mutate(Date=mdy(Date),weekday = wday(Date,label = TRUE))

## Analysis

# Summary
sales_by_day <- supermarket %>% 
  group_by(weekday) %>% 
  summarise(Total_Sales = sum(Total))
# Visualization
sales_by_day %>% 
  ggplot(aes(reorder(weekday, Total_Sales), Total_Sales, fill = weekday)) +
  geom_col(show.legend = FALSE, color = 'black') +
  geom_text(aes(label = comma(Total_Sales)), size = 3, hjust=1, color = 'black') +
  scale_y_comma() +
  scale_fill_brewer(palette = 'Paired') +
  coord_flip()+
  theme_classic() +
  labs(title = 'Total Sales Breakdown by Weekday and Time', x = 'hour', y = 'total sale')

sales_by_day_hour <- supermarket %>% 
  group_by(weekday,Time) %>% 
  summarise(Total_Sales = sum(Total)) %>% 
  ungroup()
# head(sales_by_day_hour)

sales_by_day_hour %>% 
  mutate(Time = fct_reorder(Time,Total_Sales)) %>% 
  ggplot(aes(Time,Total_Sales, fill = weekday)) +
  geom_col(show.legend = FALSE, color = 'black') + 
  geom_text(aes(label = comma(Total_Sales)), size = 3, hjust = 1, color = 'black') +
  scale_y_comma() +
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(~weekday, scales = 'free_y') +
  coord_flip() +
  theme_classic() +
  labs(title = 'Total Sales Breakdown by Weekday and Time', x = "Hour",y = "Total Sales")
  
supermarket %>% 
  group_by(Monthly = floor_date(Date, unit = "1 week"), Gender) %>% 
  summarise(Total_Sales = sum(Total)) %>% 
  ggplot(aes(Monthly, Total_Sales)) +
  geom_line(aes(color = Gender), size = 2)+
  theme_light()+
  scale_y_comma()
