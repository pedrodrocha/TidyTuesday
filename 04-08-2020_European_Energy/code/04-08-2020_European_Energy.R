################################################################################
# Title: A long road ahead for 100% renewable energy production in Europe
# Author: @pedro_drocha
# Purpose: TidyTuesday Visualization - Week 32 - European Energy
# Date: 04-08-2020
################################################################################

# ===================== PACKAGES ===============================================
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
library(extrafont)
library(pdftools)
# ===================== DATA IMPORT ============================================
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)

energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals

# ===================== DATA PREP. =============================================

# ---- It seens that Greece and UK are NA for country_name
energy_types %>% 
  filter(is.na(country_name))

country_totals %>% 
  filter(is.na(country_name))

# ---- Adding Greece and UK to country_name
energy_types %>% 
  mutate(
    country_name = case_when(
      country == "EL" ~ "Greece",
      country == "UK" ~ "United Kingdom",
      TRUE ~ country_name
    )
  ) -> energy_types

country_totals %>% 
  mutate(
    country_name = case_when(
      country == "EL" ~ "Greece",
      country == "UK" ~ "United Kingdom",
      TRUE ~ country_name
    )
  ) -> country_totals


# --- Tidying the Years 
energy_types %>% 
  pivot_longer(
    5:7,
    names_to = "year",
    values_to = "energy_production"
  ) -> energy_types


country_totals %>% 
  pivot_longer(
    5:7,
    names_to = "year",
    values_to = "energy_production"
  ) -> country_totals


# ----- Excluding Level 2 (for country totals): thanks a lot 
# ----- Tom Mock (@thomas_mock) and Yobanny Sámano (@ysamano28) for the heads-up
energy_types %>% 
  filter(level == "Level 1") -> energy_types

# ----- Country Totals
country_totals %>% 
  filter(type == "Total net production") %>% 
  rename(total = energy_production) %>% 
  select(country_name, year, total) -> country_totals


# ----- Renewable Energy: Hydro, Wind, Solar, and Geothermal

`%notin%` <- Negate(`%in%`)

energy_types %>% 
  mutate( 
    is_renewable = case_when( 
     
      type %in% c("Hydro", "Wind", "Solar", "Geothermal") ~ "yes",
      type %notin% c("Hydro", "Wind", "Solar", "Geothermal") ~ "no"
       
    )
  ) -> energy


energy %>% 
  select(-type, -level, -country) %>% 
  group_by(country_name, year, is_renewable) %>% 
  summarise(prod_renewable = sum(energy_production)) %>% 
  filter(is_renewable == "yes") %>% 
  select(-is_renewable) %>% 
  left_join(country_totals,.) %>% 
  mutate(percent = round((prod_renewable / total) * 100,1)) -> energy_renewable


# ===================== SETUP ==================================================

# ----- 100% mark 2016
tibble(country_name = unique(energy$country_name),
       total_percent = 100) -> tot_percent
tot_percent -> tot_percent %>% 
  mutate(country_name = glue("<b style='color:#787878'>{country_name}</b>"))

# ---- Base for axis color 2017

energy_renewable2016 <- energy_renewable %>% 
  filter(year == 2016) %>% 
  rename(percent2016 = percent) %>% 
  select(country_name, percent2016)

# ----- 100% mark 2017

energy_renewable %>% 
  filter(year == 2017) %>% 
  left_join(energy_renewable2016,.) %>% 
  mutate(
      color = case_when(
    
        percent > percent2016 ~ "#096700",
        percent < percent2016 ~ "#670000",
        percent == percent2016 ~ "#787878"
    
    ),
      name = glue("<b style='color:{color}'>{country_name}</b>")
  ) %>% 
  select(name) %>% 
  mutate(tot_percent = 100) -> tot_percent2017


# ---- Base for axis color 2018
energy_renewable2017 <- energy_renewable %>% 
  filter(year == 2017) %>% 
  rename(percent2017 = percent) %>% 
  select(country_name, percent2017)


# ----- 100% mark 2018
energy_renewable %>% 
  filter(year == 2018) %>% 
  left_join(energy_renewable2017,.) %>% 
  mutate(
    color = case_when(
      
      percent > percent2017 ~ "#096700",
      percent < percent2017 ~ "#670000",
      percent == percent2017 ~ "#787878"
      
    ),
    name = glue("<b style='color:{color}'>{country_name}</b>")
  ) %>% 
  select(name) %>% 
  mutate(tot_percent = 100) -> tot_percent2018


# ===================== PLOT ===================================================

# ---- 2017
energy_renewable %>% 
  filter(year == 2017) %>% 
  left_join(energy_renewable2016,.) %>% 
  mutate(
    color = case_when(
      
      percent > percent2016 ~ "#096700",
      percent < percent2016 ~ "#670000",
      percent == percent2016 ~ "#787878"
      
      
    ),
    name = glue("<b style='color:{color}'>{country_name}</b>")
  ) %>%
  ggplot() +
  geom_segment(aes(reorder(x = name, percent/100), xend = name,
                   y = 0, yend = percent/100,
                   color = color)) +
  geom_point(data = tot_percent2017,
             aes(x = name, y = tot_percent/100), 
             size = 3, alpha = .2) +
  geom_segment(data = tot_percent2017,
               aes(x = name, xend = name,
                   y = 0, yend = tot_percent/100),
               linetype = "dashed", alpha = 0.4) +
  geom_point(aes(reorder(x = name, percent/100),
                 y = percent/100,
                 color = color)) +
  geom_richtext(aes(reorder(x = name, percent/100),
                    y = percent/100,
                    label = glue("**{round(percent)}%**"),
                    color = color), hjust = -.001, size = 3,
                fill = NA, label.color = NA,
                family = "Garamond") + 
  scale_color_manual(values = c("#096700","#670000","#787878")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(y = "Electricity generation by renewable source (%)",
       title = "**2017**") +
  theme_minimal() +
  theme(axis.text.y  = element_markdown(family = "Garamond"),
        axis.text.x  = element_markdown(family = "Garamond"),
        axis.title.x = element_markdown(family = "Garamond"),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(hjust = .5,family = "Garamond")) -> a



# ---- 2018
energy_renewable %>% 
  filter(year == 2018) %>% 
  left_join(energy_renewable2017,.) %>% 
  mutate(
    color = case_when(
      
      percent > percent2017 ~ "#096700",
      percent < percent2017 ~ "#670000",
      percent == percent2017 ~ "#787878"
      
      
    ),
    name = glue("<b style='color:{color}'>{country_name}</b>")
  ) %>% 
  ggplot() +
  geom_segment(aes(reorder(x = name, percent/100), xend = name,
                   y = 0, yend = percent/100,
                   col = color)) +
  geom_point(data = tot_percent2018,
             aes(x = name, y = tot_percent/100), 
             size = 3, 
             alpha = .2) +
  geom_segment(data = tot_percent2018,
               aes(x = name, xend = name,
                   y = 0, yend = tot_percent/100),
               linetype = "dashed", alpha = 0.4) +
  geom_point(aes(reorder(x = name, percent/100),
                 y = percent/100,
                 color = color)) +
  geom_richtext(aes(reorder(x = name, percent/100),
                y = percent/100,
                label = glue("**{round(percent)}%**"),
                color = color), hjust = -.000001, size = 3,
                fill = NA, label.color = NA) + 
  scale_color_manual(values = c("#096700","#670000","#787878")) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(y = "Electricity generation by renewable source (%)",
       title = "**2018**") +
  theme_minimal() +
  theme(axis.text.y  = element_markdown(family = "Garamond"),
        axis.text.x  = element_markdown(family = "Garamond"),
        axis.title.x = element_markdown(family = "Garamond"),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.title = element_markdown(hjust = .5,family = "Garamond")) -> b

plot <- a + b +
  plot_annotation(title = "**A long road ahead for 100% renewable energy 
                  production in Europe**",
                  subtitle ="According to the WWF, there is no path to 
                  protecting the climate without dramatically changing how we 
                  produce and use electricity.Renewable energy minimizes carbon 
                  pollution and has a much lower impact on our environment.<br>
                  Europe, however, is years away from trully setting aside 
                  fossil fuels and European countries are struggling to increase 
                  the % of electricity generation by renewable sources.<br>The
                  plot shows the share of renewable sources in the total net 
                  energy production for each European country in 2017 and 2018.
                  <br>In <b style='color:#096700'>green</b> are countries that 
                  increased the share in comparison to the previous year, while 
                  in <b style='color:#670000'>red</b> are countries that 
                  decreased the share.",
                  caption = "TidyTuesday #32 | @pedro_drocha | Data: Eurostat",
                  theme = theme(
    plot.background = element_rect(fill = "#F7F8F8"),
    plot.title = element_markdown(family = "Garamond", hjust = .5, size = 20),
    plot.subtitle = element_markdown(family = "Garamond", hjust = .5, size = 12),
    plot.caption  = element_text(size = 11,hjust = 0.5, family = "Garamond")))


# ===================== SAVING  ================================================

ggsave("2020-08-04_European_Energy.pdf",
       height = 8,
       width = 16,
       plot = plot,
       device = cairo_pdf)

pdf_convert(pdf = "2020-08-04_European_Energy.pdf",
            format = "png", dpi = 400)


