---
title: "Reducing Gun Deaths"
format: html
keep-md: true
warning: false
fig-width: 12
editor_options: 
  chunk_output_type: console
---



### Week 3: Task 2

**Task: Reducing Gun Deaths**


::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(RColorBrewer)
```
:::

::: {.cell}

```{.r .cell-code}
og_gun_data <- read_csv('https://raw.githubusercontent.com/fivethirtyeight/guns-data/refs/heads/master/full_data.csv') %>%
  mutate(month = as.numeric(month))
```
:::


Summarizing the FiveThirtyEight Article

When most people talk about gun deaths, from my experience, it is typically about the police, terrorists, or school shootings. And if they do talk at all about the demographics of these people, it is often that they are Black. Although that is part of the death count, it is not even close to all of it. I think, as people, we tend to group things in our minds that are simpler and, without realizing, think they are the same. However, the series of charts shown in the article tells a very different story. The article shows the breakdown of gun deaths in the USA by depicting each death as one dot. It then shows the breakdown of percentages of where the deaths came from. From the media, you would think that most gun deaths are murders, but the charts and data show a very different story. Most gun deaths are from suicides, and 85% of those are men. Presenting the data like this is a very impactful and powerful way of showing the information as it is.


::: {.cell}

```{.r .cell-code}
gun_deaths_by_intent <- og_gun_data %>%
  group_by(intent) %>%
  summarise(num_deaths = n()) %>%
  mutate(percentage = num_deaths / sum(num_deaths) * 100)  

ggplot(gun_deaths_by_intent, aes(x = "", y = num_deaths, fill = intent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Gun Deaths by Intent", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) +  
  scale_fill_brewer(palette = "Set3") +  
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5),  
            color = "black")  
```

::: {.cell-output-display}
![](task_2_files/figure-html/unnamed-chunk-3-1.png){width=1152}
:::
:::

::: {.cell}

```{.r .cell-code}
og_gun_data <- og_gun_data %>%
  mutate(season = case_when(
    month %in% c(12, 01, 02) ~ "Winter",
    month %in% c(03, 04, 05) ~ "Spring",
    month %in% c(06, 07, 08) ~ "Summer",
    month %in% c(09, 10, 11) ~ "Fall"
  ))

head(og_gun_data)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 6 × 12
   ...1  year month intent  police sex     age race     hispanic place education
  <dbl> <dbl> <dbl> <chr>    <dbl> <chr> <dbl> <chr>       <dbl> <chr> <chr>    
1     1  2012     1 Suicide      0 M        34 Asian/P…      100 Home  BA+      
2     2  2012     1 Suicide      0 F        21 White         100 Stre… Some col…
3     3  2012     1 Suicide      0 M        60 White         100 Othe… BA+      
4     4  2012     2 Suicide      0 M        64 White         100 Home  BA+      
5     5  2012     2 Suicide      0 M        31 White         100 Othe… HS/GED   
6     6  2012     2 Suicide      0 M        17 Native …      100 Home  Less tha…
# ℹ 1 more variable: season <chr>
```


:::
:::


The following three charts show the number of deaths by sex, separated by season. These charts show very clearly that males die from guns far more than females do. They also show some slight trends around when these deaths occur. For males, the most deaths happen in the summer, and the colder it is, the fewer deaths there are. Females are similar but have a notable difference. Their lowest death season is still the winter, but fall is almost as high in death counts as the summer, and spring remains in the middle.

Based on these graphs, it shows when and to whom they could target their commercials: to men in the summer and warmer seasons.


::: {.cell}

```{.r .cell-code}
gun_deaths_by_season = og_gun_data %>%
  group_by(season, sex) %>%
  summarise(num_deaths = n())

ggplot(gun_deaths_by_season, aes(x = season, y = num_deaths, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gun Deaths by Season and Sex", x = "Season", y = "Number of Deaths") +
  scale_fill_manual(values = c("M" = "lightblue", "F" = "lightpink")) +  
  theme_minimal()
```

::: {.cell-output-display}
![](task_2_files/figure-html/unnamed-chunk-5-1.png){width=1152}
:::
:::

::: {.cell}

```{.r .cell-code}
female_deaths = gun_deaths_by_season %>%
  filter(sex == "F")

female_plot <- ggplot(female_deaths, aes(x = season, y = num_deaths, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gun Deaths by Season and Sex (Females)", x = "Season", y = "Number of Deaths") +
  scale_fill_manual(values = c("F" = "lightpink")) +  
  theme_minimal()

male_deaths = gun_deaths_by_season %>%
  filter(sex == "M")

male_plot <- ggplot(male_deaths, aes(x = season, y = num_deaths, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gun Deaths by Season and Sex (Males)", x = "Season", y = "Number of Deaths") +
  scale_fill_manual(values = c("M" = "lightblue")) +  
  theme_minimal()

combined_plot <- female_plot + male_plot + plot_layout(nrow = 1)
print(combined_plot)
```

::: {.cell-output-display}
![](task_2_files/figure-html/unnamed-chunk-6-1.png){width=1152}
:::
:::


This chart shows the number of gun deaths in the US separated by season and age. It plainly shows that the highest number of deaths is among 25-34 year-olds by a large margin. Next is the 65+ group, followed by 45-55. The 18-25 and 35-45 age groups have about the same rates of death, varying a little by season. Then there's the 55-65 group, and finally, way lower than everyone else by a large margin, is the under-18 group. However, the frequency of deaths doesn't seem to vary much by season based on age.

Based on this chart, it shows the most at-risk age groups, which could help them target their commercials.


::: {.cell}

```{.r .cell-code}
# Data manipulation
gun_deaths_by_age_season <- og_gun_data %>%
  mutate(age_group = case_when(
    age < 18 ~ "Under 18",
    age >= 18 & age < 25 ~ "18-25",
    age >= 25 & age < 35 ~ "25-35",
    age >= 35 & age < 45 ~ "35-45",
    age >= 45 & age < 55 ~ "45-55",
    age >= 55 & age < 65 ~ "55-65",
    age >= 65 ~ "65+",
    TRUE ~ "Unknown"
  )) %>%
  group_by(season, age_group) %>%
  summarise(num_deaths = n()) %>%
  mutate(age_group = factor(age_group, levels = c("Under 18", "18-25", "25-35", "35-45", "45-55", "55-65", "65+", "Unknown")),
         season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")))  # Season ordering here

# Plot
ggplot(gun_deaths_by_age_season, aes(x = season, y = num_deaths, fill = age_group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gun Deaths by Season and Age Group", x = "Season", y = "Number of Deaths") +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal()
```

::: {.cell-output-display}
![](task_2_files/figure-html/unnamed-chunk-7-1.png){width=1152}
:::
:::


This chart shows the number of deaths in the US by season of the year and the specific manner of death. It is very clear from the graphic that suicide is the most frequent cause of death, followed by homicide, then accidental. There seems to be a very notable pattern with all types of death: there are more in the summer months. This could be because people have more support during the holidays. However, this trend does not hold true for accidental deaths. They stay pretty much the same across the seasons, with slightly more accidental deaths in the winter.

This chart tells them what types of gun deaths should be targeted for prevention.


::: {.cell}

```{.r .cell-code}
gun_deaths_by_intent_season <- og_gun_data %>%
  group_by(season, intent) %>%
  summarise(num_deaths = n()) %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")))

ggplot(gun_deaths_by_intent_season, aes(x = season, y = num_deaths, color = intent, group = intent)) +
  geom_line(size = 1) +
  geom_point(size = 3) +  
  labs(title = "Gun Deaths by Season and Intent", x = "Season", y = "Number of Deaths") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
```

::: {.cell-output-display}
![](task_2_files/figure-html/unnamed-chunk-8-1.png){width=1152}
:::
:::
