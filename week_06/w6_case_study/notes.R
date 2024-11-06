```{r}
library(Lahman)
library(priceR)
library(dplyr)
```

```{r}
joined_data_1 = People %>%
  inner_join(Salaries, by = "playerID") %>% 
  select(nameFirst, nameLast, nameGiven, playerID, salary, yearID) 


head(joined_data_1)
```

```{r}
joined_data_2 <- joined_data_1 %>%
  inner_join(CollegePlaying, by = "playerID", "yearID")

head(joined_data_2)
```

```{r}
library(dplyr)

# Assuming your data frames are named joined_data_1 and CollegePlaying
combined_data <- left_join(joined_data_1, CollegePlaying, by = "playerID")

# View the resulting combined data
head(combined_data)

```



```{r}
joined_data_3 <- joined_data_2 %>%
  inner_join(Schools, by = "schoolID") %>%
  select(name_full, nameFirst, nameLast, nameGiven, playerID, salary, yearID, schoolID)

# Check the result
head(joined_data_3)

```


```{r}

adjust_for_inflation(price = joined_data_3$salary, from_date = joined_data_3$salary, country = "US", to_date = 2021) 

```







```{r}
library(Lahman)
library(priceR)
library(dplyr)
```


```{r}
joined_2nd_1 = Schools %>%
  inner_join(CollegePlaying, by = "schoolID") %>% 
  select(schoolID, name_full, playerID, yearID) 

head(joined_2nd_1)
```

```{r}
joined_2nd_2 <- joined_2nd_1 %>%
  inner_join(People, by = "playerID")

head(joined_2nd_2)

```

```{r}
joined_2nd_3 <- joined_2nd_2 %>%
  inner_join(Salaries, by = "playerID", "yearID")

head(joined_2nd_3)
```

```{r}
joined_2nd_4 = joined_2nd_3 %>% 
  select(name_full)
```








