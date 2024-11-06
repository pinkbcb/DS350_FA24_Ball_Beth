---
title: "Task: Idaho Water"
subtitle: "Course DS 350"
author: "Bethany Ball"
keep-md: true
fig-width: 14
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
editor: 
  markdown: 
    wrap: 72
editor_options: 
  chunk_output_type: console
---


::: {.cell}

```{.r .cell-code}
pacman::p_load(directlabels, gapminder, tidyverse, ggrepel, ggthemes, rio, plotly, lubridate, scales, patchwork, USAboundaries, sf, ggplot2)
```
:::

::: {.cell}

```{.r .cell-code}
#wells <- st_read("Wells/Wells.shp") 
#dams <- st_read("Idaho_Dams/Dam_Safety.shp")
#rivers <- st_read("water/hyd250.shp")
#idaho_boundary <- st_read("shp/County-AK-HI-Moved-USA-Map.shp")
```
:::

::: {.cell}

```{.r .cell-code}
pacman::p_load(downloader, sf, fs, tidyverse)

data_thing = function(path){
river_path <- path
df <- tempfile(); uf <- tempfile()
download(river_path, df, mode = "wb")
unzip(df, exdir = uf)
out_put <- read_sf(uf)
file_delete(df); dir_delete(uf)

return(out_put)
}
```
:::

::: {.cell}

```{.r .cell-code}
dams =  data_thing('https://byuistats.github.io/M335/data/Idaho_Dams.zip')
rivers = data_thing("https://byuistats.github.io/M335/data/water.zip")
wells = data_thing("https://byuistats.github.io/M335/data/Wells.zip")
idaho_boundary = data_thing("https://byuistats.github.io/M335/data/shp.zip")
```
:::

::: {.cell}

```{.r .cell-code}
wells_filtered <- wells %>% filter(Production > 5000)
dams_filtered <- dams %>% filter(SurfaceAre > 50)
rivers_filtered <- rivers %>% filter(FEAT_NAME %in% c("Snake River", "Henrys Fork"))
```
:::

::: {.cell}

```{.r .cell-code}
# Ensure the CRS is consistent (switching to EPSG:4326)
new_projection <- st_crs(4326)

# Transform all datasets to the new CRS
wells_filtered <- st_transform(wells_filtered, new_projection)
dams_filtered <- st_transform(dams_filtered, new_projection)
rivers_filtered <- st_transform(rivers_filtered, new_projection)
idaho_boundary <- st_transform(idaho_boundary, new_projection)

# Filter and merge Idaho counties into one boundary
idaho_boundary <- idaho_boundary %>% 
  filter(StateName == "Idaho") %>% 
  st_union()
```
:::

::: {.cell}

```{.r .cell-code}
idaho_map <- ggplot() +
  geom_sf(data = idaho_boundary, fill = "lightgrey", color = "black") +
  geom_sf(data = wells_filtered, color = "blue", size = 1, alpha = 0.6) +
  geom_sf(data = dams_filtered, color = "red", size = 2, shape = 18, alpha = 0.8) +
  geom_sf(data = rivers_filtered, color = "darkblue", size = 1) +
  labs(title = "Idaho Water System Features",
       subtitle = "Showing selected wells, large dams, and rivers") +
  theme_minimal() +
  coord_sf(xlim = c(-117, -111), ylim = c(42, 49))  # Adjust as needed for Idaho bounds

print(idaho_map)
```

::: {.cell-output-display}
![](w8_task_2_files/figure-html/unnamed-chunk-7-1.png){width=1344}
:::
:::