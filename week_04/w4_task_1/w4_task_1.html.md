---
title: "Task: Clean and Reformat (aka tidy) Stock Data"
subtitle: "Course DS 350"
author: "Bethany Ball"
keep-md: true
format:
  html:
    self-contained: true
    page-layout: full
    title-block-banner: true
    toc: true
    toc-depth: 3
    toc-location: body
    number-sections: false
    html-math-method: katex
    code-fold: true
    code-summary: "Show the code"
    code-overflow: wrap
    code-copy: hover
    code-tools:
        source: false
        toggle: true
        caption: See code
execute: 
  warning: false
    
---


::: {.cell}

```{.r .cell-code}
# Load necessary libraries
library(readr)
library(tidyverse)
library(readxl)
library(rio)
library(dplyr)
library(tidyr)
library(DT)


# Read in the RDS file from GitHub
stock_data = read_rds("https://github.com/byuistats/data/raw/master/Dart_Expert_Dow_6month_anova/Dart_Expert_Dow_6month_anova.RDS")
```
:::

::: {.cell}

```{.r .cell-code}
separated_data <- stock_data %>%
  mutate(
    month = tolower(sub(".*-(.*)\\d{4}", "\\1", contest_period)),  
    year = sub(".*?(\\d{4})$", "\\1", contest_period)   
  ) %>%
  select(contest_period, month, year, value, variable)

print(separated_data)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 300 × 5
   contest_period             month     year  value variable
   <chr>                      <chr>     <chr> <dbl> <chr>   
 1 January-June1990           june      1990   12.7 PROS    
 2 February-July1990          july      1990   26.4 PROS    
 3 March-August1990           august    1990    2.5 PROS    
 4 April-September1990        september 1990  -20   PROS    
 5 May-October1990            october   1990  -37.8 PROS    
 6 June-November1990          november  1990  -33.3 PROS    
 7 July-December1990          december  1990  -10.2 PROS    
 8 August1990-January1991     january   1991  -20.3 PROS    
 9 September1990-February1991 february  1991   38.9 PROS    
10 October1990-March1991      march     1991   20.2 PROS    
# ℹ 290 more rows
```


:::
:::

::: {.cell}

```{.r .cell-code}
filtered_data <- separated_data %>%
  filter(variable == "DJIA")

final_data <- filtered_data %>%
  select(-contest_period)
```
:::

::: {.cell}

```{.r .cell-code}
pivot_table = final_data %>% 
  pivot_wider(names_from = year, 
                values_from = value,
                )
pivot_table
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 14 × 11
   month variable `1990` `1991` `1992` `1993` `1994` `1995` `1996` `1997` `1998`
   <chr> <chr>     <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
 1 june  DJIA        2.5   17.7    3.6    7.7   -6.2   16     10.2   16.2   15  
 2 july  DJIA       11.5    7.6    4.2    3.7   -5.3   19.6    1.3   20.8    7.1
 3 augu… DJIA       -2.3    4.4   -0.3    7.3    1.5   15.3    0.6    8.3  -13.1
 4 sept… DJIA       -9.2    3.4   -0.1    5.2    4.4   14      5.8   20.2  -11.8
 5 octo… DJIA       -8.5    4.4   -5      5.7    6.9    8.2    7.2    3     NA  
 6 nove… DJIA      -12.8   -3.3   -2.8    4.9   -0.3   13.1   15.1    3.8   NA  
 7 dece… DJIA       -9.3    6.6    0.2   NA      3.6    9.3   15.5   -0.7   NA  
 8 janu… DJIA       NA     -0.8    6.5   -0.8   11.2    1.8   15     19.6   -0.3
 9 febr… DJIA       NA     11      8.6    2.5    5.5   NA     15.6   20.1   10.7
10 march DJIA       NA     15.8    7.2    9      1.6    7.3   18.4    9.6    7.6
11 april DJIA       NA     16.2   10.6    5.8    0.5   12.8   14.8   15.3   22.5
12 may   DJIA       NA     17.3   17.6    6.7    1.3   19.5    9     13.3   10.6
13 dec.  DJIA       NA     NA     NA      8     NA     NA     NA     NA     NA  
14 febu… DJIA       NA     NA     NA     NA     NA      3.2   NA     NA     NA  
```


:::
:::

::: {.cell}

```{.r .cell-code}
cleaned_data <- pivot_table %>%
  mutate(month = case_when(
    is.na(month) ~ "february",  
    month == "dec." ~ "december", 
    TRUE ~ month  
  )) %>%
  mutate(month = factor(month, levels = c("january", "february", "march", "april", "may", 
                                          "june", "july", "august", "september", 
                                          "october", "november", "december"))) 

final_combined_data <- cleaned_data %>%
  group_by(month) %>%
  summarise(across(where(is.numeric), ~ ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))),  
            across(where(is.character), ~ first(.)))  

final_combined_data <- final_combined_data %>%
  filter(!is.na(month)) %>%
  arrange(month)

final_combined_data
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 12 × 11
   month `1990` `1991` `1992` `1993` `1994` `1995` `1996` `1997` `1998` variable
   <fct>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <chr>   
 1 janu…   NA     -0.8    6.5   -0.8   11.2    1.8   15     19.6   -0.3 DJIA    
 2 febr…   NA     11      8.6    2.5    5.5   NA     15.6   20.1   10.7 DJIA    
 3 march   NA     15.8    7.2    9      1.6    7.3   18.4    9.6    7.6 DJIA    
 4 april   NA     16.2   10.6    5.8    0.5   12.8   14.8   15.3   22.5 DJIA    
 5 may     NA     17.3   17.6    6.7    1.3   19.5    9     13.3   10.6 DJIA    
 6 june     2.5   17.7    3.6    7.7   -6.2   16     10.2   16.2   15   DJIA    
 7 july    11.5    7.6    4.2    3.7   -5.3   19.6    1.3   20.8    7.1 DJIA    
 8 augu…   -2.3    4.4   -0.3    7.3    1.5   15.3    0.6    8.3  -13.1 DJIA    
 9 sept…   -9.2    3.4   -0.1    5.2    4.4   14      5.8   20.2  -11.8 DJIA    
10 octo…   -8.5    4.4   -5      5.7    6.9    8.2    7.2    3     NA   DJIA    
11 nove…  -12.8   -3.3   -2.8    4.9   -0.3   13.1   15.1    3.8   NA   DJIA    
12 dece…   -9.3    6.6    0.2    8      3.6    9.3   15.5   -0.7   NA   DJIA    
```


:::
:::

::: {.cell}

```{.r .cell-code}
datatable(final_combined_data, options = list(lengthMenu = c(12, 10, 20)), extensions = "Responsive")
```

::: {.cell-output-display}


```{=html}
<div class="datatables html-widget html-fill-item" id="htmlwidget-6368e465eab240721075" style="width:100%;height:auto;"></div>
<script type="application/json" data-for="htmlwidget-6368e465eab240721075">{"x":{"filter":"none","vertical":false,"extensions":["Responsive"],"data":[["1","2","3","4","5","6","7","8","9","10","11","12"],["january","february","march","april","may","june","july","august","september","october","november","december"],[null,null,null,null,null,2.5,11.5,-2.3,-9.199999999999999,-8.5,-12.8,-9.300000000000001],[-0.8,11,15.8,16.2,17.3,17.7,7.6,4.4,3.4,4.4,-3.3,6.6],[6.5,8.6,7.2,10.6,17.6,3.6,4.2,-0.3,-0.1,-5,-2.8,0.2],[-0.8,2.5,9,5.8,6.7,7.7,3.7,7.3,5.2,5.7,4.9,8],[11.2,5.5,1.6,0.5,1.3,-6.2,-5.3,1.5,4.4,6.9,-0.3,3.6],[1.8,null,7.3,12.8,19.5,16,19.6,15.3,14,8.199999999999999,13.1,9.300000000000001],[15,15.6,18.4,14.8,9,10.2,1.3,0.6,5.8,7.2,15.1,15.5],[19.6,20.1,9.6,15.3,13.3,16.2,20.8,8.300000000000001,20.2,3,3.8,-0.7],[-0.3,10.7,7.6,22.5,10.6,15,7.1,-13.1,-11.8,null,null,null],["DJIA","DJIA","DJIA","DJIA","DJIA","DJIA","DJIA","DJIA","DJIA","DJIA","DJIA","DJIA"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>month<\/th>\n      <th>1990<\/th>\n      <th>1991<\/th>\n      <th>1992<\/th>\n      <th>1993<\/th>\n      <th>1994<\/th>\n      <th>1995<\/th>\n      <th>1996<\/th>\n      <th>1997<\/th>\n      <th>1998<\/th>\n      <th>variable<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"lengthMenu":[12,10,20],"columnDefs":[{"className":"dt-right","targets":[2,3,4,5,6,7,8,9,10]},{"orderable":false,"targets":0},{"name":" ","targets":0},{"name":"month","targets":1},{"name":"1990","targets":2},{"name":"1991","targets":3},{"name":"1992","targets":4},{"name":"1993","targets":5},{"name":"1994","targets":6},{"name":"1995","targets":7},{"name":"1996","targets":8},{"name":"1997","targets":9},{"name":"1998","targets":10},{"name":"variable","targets":11}],"order":[],"autoWidth":false,"orderClasses":false,"responsive":true}},"evals":[],"jsHooks":[]}</script>
```


:::
:::
