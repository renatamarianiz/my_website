---
categories:
- "R"
- "ggplot"
date: "2017-10-31T21:28:43-05:00"
description: "Reproduction of the plot of the 2016 contributors in the United States"
draft: false
image: pic10.jpg
keywords: ""
slug: ipsum
title: "2016 California Contributors plots""
---

```{r, setup, warning=FALSE, message=FALSE, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, warning=FALSE, message=FALSE, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
```


In this challenge we will reproduce the plots of the 2016 California Contributors, shown below.

```{r challenge2, echo=FALSE, out.width="100%"}

knitr::include_graphics(here::here("images", "challenge2.png"), error = FALSE)

```

Firstly, we will import the CA contributors dataset and the zip code data base and merge both.

```{r, load_CA_data, warnings= FALSE, message=FALSE}

# Load dataframes
CA_contributors_2016 <- vroom::vroom(here::here("data","CA_contributors_2016.csv"))
zip_code <- vroom::vroom(here::here("data","zip_code_database.csv"))

# Changing the column zip of our zip_code dataframe to a numeric value
zip_code <- zip_code %>% 
  mutate(zip = as.numeric(zip))

# Merging dataframes
CA_contributors_2016 <- CA_contributors_2016 %>% 
  inner_join(zip_code, by =  "zip")

```

Secondly, we will clean our dataframe and select/calculate the variables that will be used in our analysis.

```{r, clean_data}

CA_contributors_2016_cleaned <- CA_contributors_2016 %>%
  group_by(cand_nm,primary_city) %>% 
  summarise(total_amt = sum(contb_receipt_amt)) %>% 
  select(candidate = cand_nm, city = primary_city, money_raised = total_amt)

```

Then, we will plot Trump's and Hillary's top 10 contribution cities graphs separately, and change its aesthetics.

```{r, hillary_trump_top10}
library(scales)

# Plot Hillary's top 10 contribution cities
hillary <- CA_contributors_2016_cleaned %>%
  filter(candidate == "Clinton, Hillary Rodham") %>%
  top_n(10, wt = money_raised) %>% 
  ggplot(aes(x = money_raised, y = reorder(city, money_raised))) + geom_col(fill = "dodgerblue3") +   
  facet_grid(.~candidate) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_continuous(labels = scales::dollar_format()) + 
  theme_update(plot.title = element_text(hjust = 0.5)) + #center the title
  theme(strip.background = element_rect(fill = "grey"), 
        stip.text = element_text(size = 8, color = "black"),
  ) + 
  theme_bw()

# Plot Trump's top 10 contribution cities
trump <- CA_contributors_2016_cleaned %>%
  filter(candidate == "Trump, Donald J.") %>%
  top_n(10, wt = money_raised) %>% 
  ggplot(aes(x = money_raised, y = reorder(city, money_raised))) + geom_col(fill = "brown3") +   
  facet_grid(.~candidate) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_continuous(labels = scales::dollar_format()) + 
  theme_update(plot.title = element_text(hjust = 0.5)) + #center the title
  theme(strip.background = element_rect(fill = "grey"), 
        stip.text = element_text(size = 8, color = "black"),
  ) + 
  theme_bw()

```

Now using patchwork it is possible to join both graphs in a singular visualization

```{r,hillary_trump}
library(patchwork)

# Combine them together and add a main title/x axis
hillary_trump_top10 <- hillary + trump +
  plot_annotation(title = "Where did candidates raise most money?", caption = "Amount Raised") 
  theme_update(plot.title = element_text(size = 10, hjust = 0.1, face = "bold"),
               aspect.ratio = 9/4,
               plot.caption = element_text(hjust = 0.55, vjust = 0.5, face = "bold"))

# Save it as a picture and set the correct proportion
ggsave("hillary_trump_top10.jpg", plot = hillary_trump_top10, width = 9, height = 4, path = here::here("images"))

# Show created picture
knitr::include_graphics(here::here("images", "hillary_trump_top10.jpg"))

```  

What if instead of analysing only 2 candidates we decided to see the top 10 cities for the top 10 candidates. Bellow we will build this plot.

```{r, top10_candidates_plots}
library(scales)
library(tidytext)

# Create a data frame with the top 10 candidates
top10_candidate <- CA_contributors_2016_cleaned %>% 
  group_by(candidate) %>% 
  summarise(money_raised_person = sum(money_raised)) %>% 
  top_n(10, wt = money_raised_person) 

# Get a list of the names of the top 10 candidates
top10_namelist <- top10_candidate$candidate

# Create the plot - first it is necessary to filter the dataframe per top10 candidates, then find the top 10 cities and then insert it in ggplot

top10_plotS <- CA_contributors_2016_cleaned %>% 
  filter(candidate%in% top10_namelist) %>%
  group_by(candidate) %>% 
  top_n(10,wt = money_raised) %>%
  ungroup %>% 
  mutate(candidate = as.factor(candidate),
         city = reorder_within(city, money_raised, candidate)) %>% 
  ggplot(aes(x = city,y = money_raised))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~candidate, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0))+ 
  labs(title = "Top 10 candidates ranked by money raised",x=NULL,y=NULL) +
  theme_bw()+
 #scale_x_continuous(labels = scales::dollar_format()) +
  NULL

# Save plot as a picture
ggsave("top10plot.jpg",plot=top10_plotS,width = 20,height = 15, path = here::here("images"))

# Place picture in code
knitr::include_graphics(here::here("images", "top10plot.jpg"))

```

