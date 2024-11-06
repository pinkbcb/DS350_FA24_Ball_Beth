---
title: "Task: Country Heights"
subtitle: "Course DS 350"
author: "Bethany Ball"
keep-md: true
fig-width: 12  
fig-height: 10
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
    
editor_options: 
  chunk_output_type: console
---


::: {.cell}

```{.r .cell-code}
library(haven)
library(foreign)
library(readr)
library(dplyr)
library(ggplot2)
library(foreign)

#library(pacman)

#pacman ::(haven, foreign, readr, dplyr, ggplot2, foreign, tidyverse, rio, pander)
```
:::

::: {.cell}

```{.r .cell-code}
german_conscr = read_dta("germanconscr.dta")
# bdec - birth year
# "height" - cm
# all M
german_conscr
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1,382 × 5
   gebger      bdec height   age co   
   <chr>      <dbl>  <dbl> <dbl> <chr>
 1 brueckenau  1850   170.    21 de-se
 2 brueckenau  1850   156.    21 de-se
 3 brueckenau  1850   172.    21 de-se
 4 brueckenau  1850   168.    21 de-se
 5 brueckenau  1850   167.    21 de-se
 6 brueckenau  1850   160.    21 de-se
 7 brueckenau  1850   161.    21 de-se
 8 brueckenau  1850   162.    21 de-se
 9 brueckenau  1850   162.    21 de-se
10 brueckenau  1850   170.    21 de-se
# ℹ 1,372 more rows
```


:::

```{.r .cell-code}
german_prison = read_dta("germanprison.dta")
#"bdec" - birth year
#"height" - cm
# all M
german_prison
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 477 × 4
     age  bdec co    height
   <dbl> <dbl> <chr>  <dbl>
 1    22  1850 de       156
 2    26  1850 de       168
 3    26  1850 de       161
 4    25  1850 de       159
 5    26  1850 de       175
 6    24  1850 de       173
 7    24  1850 de       173
 8    24  1850 de       172
 9    24  1850 de       161
10    21  1850 de       175
# ℹ 467 more rows
```


:::

```{.r .cell-code}
Heights_south_east <- read.dbf("Heights_south-east/B6090.dbf")#Birth Year: "ALTER" 
#Height: "CMETER"
#sex: "F" 5 = m, 6 = f
head(Heights_south_east)
```

::: {.cell-output .cell-output-stdout}

```
  RECNO   SJ REG GEBGER SL GEBJZ GEWERBE BERUF STD_ELT F  Z V S GEBJ CMETER
1  3669 1773   2    lau  0   173    <NA>     0    <NA> 6  1 0 0 1733 177.55
2  3670 1773   2     ma  0   173    <NA>     0    <NA> 5  8 0 0 1737 165.39
3  3671 1773   2    bre  0   175    <NA>     0    <NA> 5 11 0 0 1750 172.68
4  3672 1773   2    neu  0   173    <NA>     0    <NA> 5 11 0 0 1734 172.68
5  3673 1773   2    alz  0   173    <NA>     0    <NA> 6  0 2 0 1734 176.33
6  3674 1773   2     hd  0   173    <NA>     0    <NA> 5  6 0 0 1737 160.52
  MASS ALTER ALTERGR ARMGATT ZOLLABRD ZOLLVRD   KOM2 REGIM  COMP GREN LEIB OFF1
1    v    40       6       0       73    73.0 B03H18 06.ir riedl    0    0    1
2    v    36       6       0       68    68.0 B03H18 06.ir riedl    0    0    1
3    v    23       6       0       71    71.0 B03H18 06.ir riedl    0    0    0
4    v    39       6       0       71    71.0 B03H18 06.ir riedl    0    0    0
5    v    39       6       0       72    72.5 B03H18 06.ir riedl    0    0    0
6    v    36       6       0       66    66.0 B03H18 06.ir riedl    0    0    0
  OFF2 OPF PF A16 A17 A18 A19 A20 A21 A22 A50 G2529 G3034 G3539 G4044 G4549
1    0   0  1   0   0   0   0   0   0   0   0     0     1     0     0     0
2    0   0  1   0   0   0   0   0   0   0   0     0     0     1     0     0
3    1   0  1   0   0   0   0   0   0   0   0     0     0     0     0     0
4    1   0  1   0   0   0   0   0   0   0   0     0     1     0     0     0
5    1   0  1   0   0   0   0   0   0   0   0     0     1     0     0     0
6    1   0  1   0   0   0   0   0   0   0   0     0     0     1     0     0
  G5054 G5559 G6064 G6569 G7074 COMPOS
1     0     0     0     0     0      1
2     0     0     0     0     0      2
3     1     0     0     0     0      3
4     0     0     0     0     0      4
5     0     0     0     0     0      5
6     0     0     0     0     0      6
```


:::

```{.r .cell-code}
BLS = read_csv("https://raw.githubusercontent.com/hadley/r4ds/main/data/heights.csv")
#"height" in
#sex (male, female)
#birthday all: 1950
BLS
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1,192 × 6
    earn height sex       ed   age race    
   <dbl>  <dbl> <chr>  <dbl> <dbl> <chr>   
 1 50000   74.4 male      16    45 white   
 2 60000   65.5 female    16    58 white   
 3 30000   63.6 female    16    29 white   
 4 50000   63.1 female    16    91 other   
 5 51000   63.4 female    17    39 white   
 6  9000   64.4 female    15    26 white   
 7 29000   61.7 female    12    49 white   
 8 32000   72.7 male      17    46 white   
 9  2000   72.0 male      15    21 hispanic
10 27000   72.2 male      12    26 white   
# ℹ 1,182 more rows
```


:::

```{.r .cell-code}
UWNSD = read_sav("main05022005.sav")
#RT216I height of respondent, inches
#doby DATE OF BIRTH, YEAR
#RE35 male = 1, female = 2, other(drop/na)
UWNSD
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 7,277 × 2,317
   CASE  TYPE  CASEID CASENUM DOBM     DOBY  RT1A1    RT1A2    RT1A3    RT1B1   
   <chr> <chr> <chr>    <dbl> <dbl+lb> <dbl> <dbl+lb> <dbl+lb> <dbl+lb> <dbl+lb>
 1 00008 S     00008S       8 10 [OCT… 15     5       NA       NA        3      
 2 00058 R     00058R      58  2 [FEB… 40     3        0 [NON…  1        2      
 3 00058 S     00058S      58  2 [FEB… 40     0 [NON… NA       NA        0 [NON…
 4 00079 S     00079S      79 11 [NOV… 12     7       NA       NA        0 [NON…
 5 00082 S     00082S      82  7 [JUL… 61     4       NA        1        4      
 6 00120 R     00120R     120 10 [OCT… 54    14        0 [NON…  0 [NON…  7      
 7 00120 S     00120S     120 11 [NOV… 54     1       10        2        0 [NON…
 8 00133 R     00133R     133 10 [OCT… 51    18        0 [NON…  3       18      
 9 00146 R     00146R     146  4 [APR… 52     1        0 [NON… NA        2      
10 00146 S     00146S     146  4 [APR… 51     0 [NON… 20       NA        0 [NON…
# ℹ 7,267 more rows
# ℹ 2,307 more variables: RT1B2 <dbl+lbl>, RT1B3 <dbl+lbl>, RT1C1 <dbl+lbl>,
#   RT1C2 <dbl+lbl>, RT1C3 <dbl+lbl>, RT1D1 <dbl+lbl>, RT1D2 <dbl+lbl>,
#   RT1D3 <dbl+lbl>, RT1E1 <dbl+lbl>, RT1E2 <dbl+lbl>, RT1E3 <dbl+lbl>,
#   RT1F1 <dbl+lbl>, RT1F2 <dbl+lbl>, RT1F3 <dbl+lbl>, RT1G1 <dbl+lbl>,
#   RT1G2 <dbl+lbl>, RT1G3 <dbl+lbl>, RT1H1 <dbl+lbl>, RT1H2 <dbl+lbl>,
#   RT1H3 <dbl+lbl>, RT1I1 <dbl+lbl>, RT1I2 <dbl+lbl>, RT1I3 <dbl+lbl>, …
```


:::

```{.r .cell-code}
# colnames(Heights_south_east)
```
:::

::: {.cell}

```{.r .cell-code}
# Wrangle datasets
german_conscr <- german_conscr %>%
  mutate(birth_year = bdec, height.cm = height, height.in = height / 2.54, study = "German Conscripts") %>%
  select(birth_year, height.in, height.cm, study)

german_prison <- german_prison %>%
  mutate(birth_year = bdec, height.cm = height, height.in = height / 2.54, study = "German Prison") %>%
  select(birth_year, height.in, height.cm, study)
```
:::

::: {.cell}

```{.r .cell-code}
library(dplyr)

# Update Heights_south_east dataset
Heights_south_east <- Heights_south_east %>%
  mutate(
    birth_year = 1800 + ALTER,  
    height.cm = CMETER, 
    height.in = CMETER / 2.54, 
    study = "Heights South-East"
  ) %>%
  select(birth_year, height.in, height.cm, study)

head(Heights_south_east)
```

::: {.cell-output .cell-output-stdout}

```
  birth_year height.in height.cm              study
1       1840  69.90157    177.55 Heights South-East
2       1836  65.11417    165.39 Heights South-East
3       1823  67.98425    172.68 Heights South-East
4       1839  67.98425    172.68 Heights South-East
5       1839  69.42126    176.33 Heights South-East
6       1836  63.19685    160.52 Heights South-East
```


:::
:::

::: {.cell}

```{.r .cell-code}
BLS <- BLS %>%
  filter(sex == "male") %>%
  mutate(birth_year = 1950, height.in = height, height.cm = height * 2.54, study = "BLS") %>%
  select(birth_year, height.in, height.cm, study)
```
:::

::: {.cell}

```{.r .cell-code}
# RT216F (feet)
# RT216I (inches)
UWNSD_FI <- UWNSD %>% 
  filter(
    RT216F != "-1" & RT216F != "-2" & RT216F != "." &
    RT216I != "-1" & RT216I != "-2" & RT216I != "25" & 
    RT216I != "35" & RT216I != "45" & RT216I != "50" & RT216I != "85"
  ) %>%
  mutate(height_in = as.numeric(RT216F) * 12 + as.numeric(RT216I))
```
:::

::: {.cell}

```{.r .cell-code}
library(dplyr)

UWNSD <- UWNSD_FI %>%
  mutate(
    birth_year = DOBY,
    height.in = height_in,
    height.cm = height_in * 2.54,  
    study = "UWNSD"
  ) %>%
  select(birth_year, height.in, height.cm, study)

#print(UWNSD)

# include the full year
UWNSD <- UWNSD %>%
  mutate(
    birth_year = ifelse(birth_year <= 5, 2000 + birth_year, 1900 + birth_year)
  )

print(UWNSD)
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 7,255 × 4
   birth_year height.in height.cm study
        <dbl>     <dbl>     <dbl> <chr>
 1       1940        60      152. UWNSD
 2       1940        67      170. UWNSD
 3       1912        65      165. UWNSD
 4       1961        66      168. UWNSD
 5       1954        62      157. UWNSD
 6       1954        71      180. UWNSD
 7       1951        64      163. UWNSD
 8       1952        67      170. UWNSD
 9       1951        69      175. UWNSD
10       1949        64      163. UWNSD
# ℹ 7,245 more rows
```


:::
:::

::: {.cell}

```{.r .cell-code}
# Combine datasets
combined_data <- bind_rows(german_conscr, german_prison, Heights_south_east, BLS, UWNSD)

library(ggplot2)
library(plotly)

plot_1 <- ggplot(combined_data, aes(x = birth_year, y = height.cm, color = study)) +
  geom_point() +
  labs(title = "Height Distribution Across Studies",
       x = "Birth Year", y = "Height (cm)") +
  theme_minimal()

ggplotly(plot_1)
```

::: {.cell-output-display}


```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-e3064311703797cef108" style="width:100%;height:541px;"></div>
```


:::
:::

::: {.cell}

```{.r .cell-code}
german_conscr
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 1,382 × 4
   birth_year height.in height.cm study            
        <dbl>     <dbl>     <dbl> <chr>            
 1       1850      66.8      170. German Conscripts
 2       1850      61.6      156. German Conscripts
 3       1850      67.9      172. German Conscripts
 4       1850      66.2      168. German Conscripts
 5       1850      65.6      167. German Conscripts
 6       1850      63.1      160. German Conscripts
 7       1850      63.2      161. German Conscripts
 8       1850      63.9      162. German Conscripts
 9       1850      63.7      162. German Conscripts
10       1850      67.0      170. German Conscripts
# ℹ 1,372 more rows
```


:::

```{.r .cell-code}
german_prison
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 477 × 4
   birth_year height.in height.cm study        
        <dbl>     <dbl>     <dbl> <chr>        
 1       1850      61.4       156 German Prison
 2       1850      66.1       168 German Prison
 3       1850      63.4       161 German Prison
 4       1850      62.6       159 German Prison
 5       1850      68.9       175 German Prison
 6       1850      68.1       173 German Prison
 7       1850      68.1       173 German Prison
 8       1850      67.7       172 German Prison
 9       1850      63.4       161 German Prison
10       1850      68.9       175 German Prison
# ℹ 467 more rows
```


:::

```{.r .cell-code}
head(Heights_south_east)
```

::: {.cell-output .cell-output-stdout}

```
  birth_year height.in height.cm              study
1       1840  69.90157    177.55 Heights South-East
2       1836  65.11417    165.39 Heights South-East
3       1823  67.98425    172.68 Heights South-East
4       1839  67.98425    172.68 Heights South-East
5       1839  69.42126    176.33 Heights South-East
6       1836  63.19685    160.52 Heights South-East
```


:::

```{.r .cell-code}
BLS
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 505 × 4
   birth_year height.in height.cm study
        <dbl>     <dbl>     <dbl> <chr>
 1       1950      74.4      189. BLS  
 2       1950      72.7      185. BLS  
 3       1950      72.0      183. BLS  
 4       1950      72.2      183. BLS  
 5       1950      69.5      177. BLS  
 6       1950      68.0      173. BLS  
 7       1950      67.6      172. BLS  
 8       1950      67.8      172. BLS  
 9       1950      69.6      177. BLS  
10       1950      73.1      186. BLS  
# ℹ 495 more rows
```


:::

```{.r .cell-code}
UWNSD
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 7,255 × 4
   birth_year height.in height.cm study
        <dbl>     <dbl>     <dbl> <chr>
 1       1940        60      152. UWNSD
 2       1940        67      170. UWNSD
 3       1912        65      165. UWNSD
 4       1961        66      168. UWNSD
 5       1954        62      157. UWNSD
 6       1954        71      180. UWNSD
 7       1951        64      163. UWNSD
 8       1952        67      170. UWNSD
 9       1951        69      175. UWNSD
10       1949        64      163. UWNSD
# ℹ 7,245 more rows
```


:::

```{.r .cell-code}
combined_data
```

::: {.cell-output .cell-output-stdout}

```
# A tibble: 27,866 × 4
   birth_year height.in height.cm study            
        <dbl>     <dbl>     <dbl> <chr>            
 1       1850      66.8      170. German Conscripts
 2       1850      61.6      156. German Conscripts
 3       1850      67.9      172. German Conscripts
 4       1850      66.2      168. German Conscripts
 5       1850      65.6      167. German Conscripts
 6       1850      63.1      160. German Conscripts
 7       1850      63.2      161. German Conscripts
 8       1850      63.9      162. German Conscripts
 9       1850      63.7      162. German Conscripts
10       1850      67.0      170. German Conscripts
# ℹ 27,856 more rows
```


:::
:::



Paragraph on Data Wrangling

The data wrangling process involved importing five datasets in various formats, including Stata, DBF, CSV, and SPSS. Each dataset required careful cleaning, standardizing column names, and converting heights between inches and centimeters. For datasets that did not have a clear birth year, assumptions were made (e.g., for the BLS data, a birth year of 1950 was assumed). One challenge was identifying the birth year in the German soldier data, where column names were in German. After using Google Translate, the correct column (geburtsjahr) was identified. Overall, I had to ensure consistency in column names (birth_year, height.in, height.cm, and study) before combining the datasets.


Analysis and Comparison

The data suggests differences in average heights across birth years and geographic regions. While the datasets show an increase in height over time, especially in 20th-century data, there are variations. For example, the German conscripts from the 19th century appear shorter than those in the mid-20th century BLS data. This aligns with historical trends showing a gradual increase in human height due to better nutrition, health, and living conditions.

In comparison to the previous task, this analysis confirms the trend that humans have become taller over the centuries, but the rate of growth and regional differences highlight the complexity of height distribution. Some discrepancies could arise due to differences in data collection methods, sample populations, or societal conditions during the studied periods.
Conclusion

Overall, the data supports the notion that humans have been getting taller over time, though the extent of height increase varies based on geography and historical context. This increase likely results from improved nutrition and health care over time. However, it’s important to consider the limitations of historical datasets and the potential biases that may affect this conclusion.










