---
title: "Practice Exam Tucker Skaar"
author: "Tucker Skaar"
date: "3/2/2020"
output: rmarkdown::github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(nycflights13)
library(lubridate)
```



```{r question 1}

weather %>% mutate(day_of_year = yday(time_hour)) %>% 
  group_by(origin, day_of_year) %>% 
  summarize(temp = mean(temp, na.rm=T)) %>% 
  ggplot(aes(x = day_of_year, y = temp)) +
  geom_line()+facet_wrap(~origin)

```

```{r question 2}

matrix = weather %>% mutate(day_of_year = yday(time_hour)) %>% 
  group_by(origin, day_of_year) %>% 
  summarize(temp = mean(temp, na.rm=T)) %>% 
  pivot_wider(names_from=day_of_year, values_from = temp)

matrix
```

```{r question 3}

performance_set = flights %>% mutate(day_of_year = yday(time_hour)) %>% 
  group_by(origin, day_of_year) %>% 
  summarize(performance = mean(dep_delay<60, na.rm = T)) 

performance_set

```

```{r question 4}
weather_set = weather %>% mutate(day_of_year = yday(time_hour)) %>% 
  group_by(origin, day_of_year) %>% 
  summarize(precipitation = sum(precip, na.rm=T), 
            visible = min(visib, na.rm=T), 
            meanWind = mean(wind_speed, na.rm=T)) 

weather_set
```

```{r question 5}

perform_data = left_join(performance_set, weather_set)

perform_model = lm(performance ~ origin+precipitation+visible+meanWind, data = perform_data)

summary(perform_model)
```

```{r question 6}

data_EWR = filter(perform_data, origin == "EWR")
fit_EWR = lm(performance ~ precipitation+visible+meanWind, data = data_EWR)
summary(fit_EWR)
```
