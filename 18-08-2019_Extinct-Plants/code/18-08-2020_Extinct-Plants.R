################################################################################
# Title: Number of Extinct Plant Species by Period (1900-2020)
# Author: Pedro D. Rocha (@pedro_drocha)
# Purpose: TidyTuesday Visualization - Week 34 - Extinct Plants
# Date: 18-08-2020
################################################################################

# ---- Packages

library(tidyverse)
library(ggtext)
library(extrafont)
library(pdftools)

# ---- Data

tuesdata <- tidytuesdayR::tt_load(2020, week = 34)
plants <- tuesdata$plants


# ---- Labels

lab1 <- "The first half of the 20th Century was the period with<br> the highest
number of extinctions. Specifically between<br> <b style='color:#c29ba3'>1940
and 1959</b>, 74 plant species ceased to exist in<br> the wild."

lab2 <- "From 1940 to 1999</b> there was a substantial<br> drop in the number
of species extincted.<br> The lowest number of the time series, 44, was<br>
registered in the <b style='color:#8a9ba7'> last two decades of the<br> 20th
Century.</b> "

lab3 <- "In the <b style='color:#f7e5b7'>21th Century</b> the number of
extinct <br> species started to increase
again. For the<br> next decades is urgent the promotion of<br> public policies
with the goal of reversing <br>this trend."

# ---- Plot

plot <- plants %>%
  select(1:6) %>%
  distinct() %>%
  filter(!is.na(year_last_seen),
         !str_detect(year_last_seen,"Before")) %>%
  group_by(year_last_seen) %>%
  count() %>%
  ggplot(aes(x = year_last_seen, y = n, group = 1)) +
  geom_path(lwd = .5, color = "grey50") +
  geom_point(color = "grey50") +
  geom_point(aes(x = "1940-1959", y = 74), color = "#c29ba3", size = 3) +
  geom_point(aes(x = "1980-1999", y = 44), color = "#8a9ba7", size = 3) +
  geom_point(aes(x = "2000-2020", y = 52), color = "#f7e5b7", size = 3) +
  geom_text(aes(x = "1900-1919", y = 70, label = 70),
            vjust = -.7,
            color = "grey50",
            family = "Corbel") +
  geom_text(aes(x = "1920-1939", y = 70, label = 70),
            vjust = -.7,
            color = "grey50",
            family = "Corbel") +
  geom_text(aes(x = "1940-1959", y = 74, label = 74),
            vjust = -1,
            color = "#c29ba3",
            fontface = "bold",
            family = "Corbel") +
  geom_text(aes(x = "1960-1979", y = 60, label = 60),
            vjust = -.7,
            hjust = .1,
            color = "grey50",
            family = "Corbel") +
  geom_text(aes(x = "1980-1999", y = 44, label = 44),
            vjust = 1.4,
            color = "#8a9ba7",
            fontface = "bold",
            family = "Corbel") +
  geom_text(aes(x = "2000-2020", y = 52, label = 52),
            vjust = -1,
            color = "#f7e5b7",
            fontface = "bold",
            family = "Corbel") +
  geom_textbox(data = tibble(x = "1900-1919",
                             y = 64,
                             label = lab1),
               aes(x = x, y = y, label = label),
               inherit.aes = F,
               size = 4,
               lineheight = 1.5,
               height = NULL,
               width = NULL,
               fill = NA,
               box.colour = NA,
               hjust = 0.043,
               color = "grey20",
               family = "Corbel") +
  geom_textbox(data = tibble(x = "1920-1939",
                             y = 43,
                             label = lab2),
               aes(x = x, y = y, label = label),
               inherit.aes = F,
               size = 4,
               lineheight = 1.5,
               height = NULL,
               width = NULL,
               fill = NA,
               box.colour = NA,
               hjust = -.1,
               vjust = 0,
               color = "grey20",
               family = "Corbel") +
  geom_textbox(data = tibble(x = "1980-1999",
                             y = 55,
                             label = lab3),
               aes( x = x, y = y, label = label),
               inherit.aes = F,
               size = 4,
               lineheight = 1.5,
               height = NULL,
               width = NULL,
               fill = NA,
               box.colour = NA,
               hjust = .2,
               vjust = 0,
               color = "grey20",
               family = "Corbel") +
  scale_y_continuous(breaks = seq(0,80,10)) +
  labs(x = "PERIOD THE PLANT WAS LAST SEEN",
       y = "NUMBER OF PLANTS",
       title = "**Number of Extinct Plant Species by Period (1900-2020)
       <br><br>**",
       caption = "TidyTuesday #34 | @pedro_drocha | Data: International Union
       for Conservation of Nature (IUCN)") +
  theme_minimal() +
  theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(hjust = -.085,
                                      color = "grey10",
                                      family = "Corbel"),
        plot.caption = element_markdown(hjust = -.085, color = "grey20",
                                        margin = margin(b = 2,t = 2),
                                        family = "Corbel"),
        axis.text.y = element_blank(),
        axis.text.x = element_markdown(margin = margin(t = 8),
                                       family = "Corbel"),
        axis.title.x = element_markdown(hjust = 0.93,
                                        margin = margin(t = 15),
                                        color = "grey20",
                                        family = "Corbel"),
        axis.title.y = element_markdown(hjust = 1,
                                        margin = margin(r = 15,l = 1),
                                        color = "grey20",
                                        family = "Corbel"),
        plot.background = element_rect(color = "#F9F9F9"))

# ---- Saving

ggsave("18-08-2020_extinctPlants.pdf",
       height = 5.34,
       width = 9.65,
       plot = last_plot(),
       device = cairo_pdf)

pdf_convert(pdf = "18-08-2020_extinctPlants.pdf",
            format = "png", dpi = 400)
