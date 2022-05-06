---
  title: "Exercise 2"
output:
  html_document:
  df_print: paged
---
  
  # Preparation
  Managed to setup Github credentials.

# Tasks and Inputs

## Task 1: Import data
### Load necessary Packages
```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(lubridate)
library(sf)
library(terra)
library(ggpubr)
library(tmap)

theme_set(
  theme_classic()
)
```

### Import downloaded csv
```{r}
ws_be <- read_csv("0_rawdata/wildschwein_BE.csv")
ws_be_sf <- st_as_sf(ws_be, coords = c("E", "N"), crs = 2056, remove = FALSE)
```

## Taks 2: Getting an overview
Lets first have a look at the difftime function
```{r}
ws_be_sf %>% 
  group_by(TierID) %>% 
  mutate(., timelag = as.integer(difftime(lead(DatetimeUTC),DatetimeUTC), units = "secs")) -> ws_be_sf
```
Now we have our data with a timelag - lets examine it a bit closer.

**How many individuals were tracked**
  ```{r}
ws_be_sf %>% 
  st_drop_geometry() %>% 
  group_by(TierID, TierName) %>% 
  summarise(pings = n(),
            start_time = min(DatetimeUTC), 
            end_time = max(DatetimeUTC),
            sample_interval = mean(timelag, na.rm = T),
            tracking_time = sum(timelag, na.rm = T)) %>% 
  mutate(tracking_time_days = days(day(seconds_to_period(tracking_time)))) 

```
Now lets create visualisations
```{r}
ggplot(data = ws_be_sf) +
  geom_line(aes(x = DatetimeUTC, y = TierID)) -> p1

ggplot(data = ws_be_sf) +
  geom_bar(aes(x = timelag, fill = TierID), stat = "count") 
```

## Task 3: Deriving movement parameters - Speed
```{r}
ws_be_sf %>% 
  mutate(steplength = sqrt((E - lead(E,1))^2 + (N - lead(N,1))^2),
         speed = steplength/timelag)

```
The units are meters per second.

## Task 4: Cross-scale movement analysis
Load the data:
  ```{r}
caro <- read_csv("0_rawdata/caro60.csv")

caro_sf <- st_as_sf(caro, coords = c("E", "N"), crs = 2056, remove = FALSE)

caro_sf %>% 
  slice(seq(from=1, to = 200, by = 3)) -> caro_3
caro_sf %>% 
  slice(seq(from=1, to = 200, by = 6)) -> caro_6
caro_sf %>% 
  slice(seq(from=1, to = 200, by = 9)) -> caro_9
```

Calculate timelag, steplength and speed
```{r}
calculator <- function(df_input, interval){
  df_input %>% 
    mutate(timelag = as.numeric(difftime(lead(DatetimeUTC),DatetimeUTC), units = "secs"), 
           steplength = sqrt((E - lead(E,1))^2 + (N - lead(N,1))^2),
           speed = steplength/timelag,
           interval = interval) -> df_output
  
  return(df_output)
} 

calculator(caro_sf, "1 minute") -> caro
calculator(caro_3, "3 minutes") -> caro_3
calculator(caro_6, "6 minutes") -> caro_6
calculator(caro_9, "9 minutes") -> caro_9

rbind(caro, caro_3, caro_6, caro_9) -> caro_merge

```

```{r}
ggplot(data = caro_merge) +
  geom_line(aes(x = DatetimeUTC, y = speed, color = interval))

caro_merge %>% 
  filter(interval == "1 minute" | interval  == "3 minutes") %>% 
  ggplot(data = ., aes(x = E, y = N, color = interval, alpha = interval)) +
  geom_path() +
  geom_point() +
  scale_alpha_discrete(range = c(0.4, 1)) -> p1

caro_merge %>% 
  filter(interval == "1 minute" | interval  == "6 minutes") %>% 
  ggplot(data = ., aes(x = E, y = N, color = interval, alpha = interval)) +
  geom_path() +
  geom_point() +
  scale_alpha_discrete(range = c(0.4, 1)) -> p2

caro_merge %>% 
  filter(interval == "1 minute" | interval  == "9 minutes") %>% 
  ggplot(data = ., aes(x = E, y = N, color = interval, alpha = interval)) +
  geom_path() +
  geom_point() +
  scale_alpha_discrete(range = c(0.4, 1)) -> p3

ggarrange(p1, p2, p3)
```
# Task 5: Deriving movement parameters II: Rolling window functions
```{r}
library(zoo)
rollmean(caro$speed, k = 3)

```

