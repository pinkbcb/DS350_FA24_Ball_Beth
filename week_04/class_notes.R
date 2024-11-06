

library(readdr)
library(tidyverse)
library(readxl)
library(rio)

rcw_dat <- read_csv("https://byuistats.github.io/M335/data/rcw_reshape.csv")


wide_data = rcw_dat %>% 
  pivot_wider(names_from = Department, 
              values_from = Count,
              values_fill = 0
              )
wide_data


data <- rcw_dat %>% 
  mutate(Continent = NA) # Create an empty 'Continent' column
data





long_data = wide_data %>% 
  pivot_longer(cols = c(Chem:PHY),
               names_to = "Department", values_to = "Count"
               ) %>% 
  separate(col = Semester_Date,into =  c("month", "day", "Year"), sep = "/") %>% 
  mutate( yr_sem = paste(Year, month, sep = "-"))
long_data



ggplot(long_data, aes(x = yr_sem,
                      y = Count,
                      color = Department,
                      group = Department)) +
  geom_line()










heights_data <- read_excel("Height.xlsx")

data <- heights_data %>% 
  mutate(Continent = NA) 
data















