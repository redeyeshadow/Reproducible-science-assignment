---
title: "Reproducibility code"
output: html_notebook
---

###load packages
```{r}
library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(dplyr))
library(janitor)
library(dplyr)
library(ragg)
library(svglite)
```

###set working directory
```{r}
setwd('PenguinProjects')
```

###defining functions

##cleaning the data
```{r}
cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
}
```

##subsetting the data
```{r}
remove_empty_culmen_length <- function(penguins_clean) {
  penguins_clean %>%
    filter(!is.na(culmen_length_mm)) %>%
    filter(!is.na(body_mass_g)) %>%
  select(culmen_length_mm, body_mass_g)
}
```

##function to plot linear model
```{r}
penguin_graph <- function(graph_data){
  graph_data %>%
    ggplot(aes(x = culmen_length_mm, y = body_mass_g)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    x = "Culmen Length (mm)",
    y = "Body Mass (g)",
    title = "Culmen Length against Body Mass in penguins") +
  theme_bw()
}
```

##saving
```{r}
# Save the plot as a png and define the size, resolution, and scaling
save_penguin_graph_png <- function(graph_data, filename, size, res, scaling){
  agg_png(filename, width = size, 
                    height = size, 
                    units = "cm", 
                    res = res, 
                    scaling = scaling)
  graph <- penguin_graph(graph_data)
  print(graph)
  dev.off()
}

# Save the plot as a svg and define the size and scaling
save_penguin_graph_svg <- function(graph_data, filename, size, scaling){
    size_inches = size/2.54
    svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
    graph <- penguin_graph(graph_data)
    print(graph)
    dev.off()
}
```

###load in raw data and function from files
```{r}
penguins_raw
penguins_raw <- read.csv("~/Documents/Computing and statistics/Third year/PenguinProjects/data_raw/penguins_raw.csv")
# source("~/Documents/Computing and statistics/Third year/PenguinProjects/functions/cleaning.R")
```

###cleaning data
```{r}
penguins_clean <- cleaning(penguins_raw)
graph_data <- remove_empty_culmen_length(penguins_clean)
```


###statisical test on data: linear regression model between culmen length and body mass qnd confidence intervals 
```{r}
penguin_mod1 <- lm(culmen_length_mm ~ body_mass_g, graph_data)
summary(penguin_mod1)
confint(penguin_mod1)
```

###plotting the linear model
```{r}
graph <- penguin_graph(graph_data)
graph
```
