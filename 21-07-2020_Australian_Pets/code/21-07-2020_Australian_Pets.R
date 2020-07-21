####################################################################################
# Title: RSPCA rescued pets outcomes per Australian State (1999-2018)
# Autor: @pedro_drocha
# Purpouse: TidyTuesday Visualization - Week 30 - Australian Pets
# Date: 21-07-2020
####################################################################################

################# PACKAGES #########################################################
library(tidyverse) 
library(tidytuesdayR) 
library(ggalluvial) 
library(extrafont)

################### IMPORTING DATA #################################################
tuesdata <- tidytuesdayR::tt_load(2020, week = 30)

animal_outcomes <- tuesdata$animal_outcomes

animal_complaints <- tuesdata$animal_complaints

brisbane_complaints <- tuesdata$brisbane_complaints

######################### DATA TRANSFORMATION ######################################
animal_outcomes %>% 
  pivot_longer(
    ACT:Total,
    names_to = "state_or_territory",
    values_to = "N"
  ) %>%  
  mutate( 
    state_or_territory = case_when(
      state_or_territory == "ACT"   ~ "Australian Capital Territory",
      state_or_territory == "NSW"   ~ "New South Wales",
      state_or_territory == "NT"    ~ "Northern Territory",
      state_or_territory == "QLD"   ~ "Queensland",
      state_or_territory == "SA"    ~ "South Australia",
      state_or_territory == "TAS"   ~ "Tasmania",
      state_or_territory == "VIC"   ~ "Victoria",
      state_or_territory == "WA"    ~ "Western Australia",
      state_or_territory == "Total" ~ "Australia"
      )
    ) -> animal_outcomes_tidy


animal_outcomes_tidy %>% 
  group_by(state_or_territory, animal_type, outcome, year) %>% 
  summarise(N = sum(N, na.rm = T)) -> animal_outcomes_tidy_summarised

animal_outcomes_tidy_summarised %>% 
  filter(state_or_territory != "Australia") %>% 
  filter(outcome != "Currently In Care") %>% 
  filter(animal_type %in% c("Cats","Dogs")) %>% 
  filter(year != 2016) %>% 
  mutate(year = case_when(
    year < 2009 ~ "1999-2008",
    year >= 2009 ~ "2009-2018"
  )) %>% 
  mutate(
    outcome = case_when(
      outcome == "In Stock" ~ "Other",
      outcome == "Transferred" ~ "Other",
      TRUE ~ outcome
    )
  ) -> alluv
  
######################### PLOT #####################################################
ggplot(
  data = alluv,
    aes(axis1 = year,
        axis2 = state_or_territory,
        axis3 = outcome,
        y = N)
) +
  scale_x_discrete(
    limits = c("Year", "State", "Outcome"), 
    expand = c(.2, .05)
  ) +
  scale_fill_manual(values = c("#746c84","#dda670")) +
  geom_alluvium(aes(fill = animal_type), width = 1/12) +
  geom_stratum(fill = "#DFDFDF") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat(stratum)), 
    size = 3, 
    family = "Book Antiqua", 
    colour = "#282423"
  ) + 
  labs(
    title = "RSPCA rescued pets outcomes per Australian State (1999-2018)", 
    caption = "TidyTuesday #30 | @pedro_drocha | Data: Royal Society for the Prevention of Cruelty to Animals"
  ) +
  theme_void() +
  theme(
    text = element_text(colour = "#282423"),
    plot.background = element_rect(fill = "#F1F1F1"),
    legend.title = element_blank(),
    legend.position = c(.95, .5),
    legend.text = element_text(family = "Book Antiqua"),
    plot.title = element_text(hjust = 0.5,family = "Book Antiqua", face = "bold", size = 20),
    plot.caption  = element_text(hjust = 0.5, family = "Book Antiqua", face = "italic"),
    plot.margin = margin(4, 4, 4, 4)
  )

######################### SAVING PLOT ################################################
ggsave("21-07-2020_Australian-Pets/plot/21-07-2020.png",
       plot = last_plot())
