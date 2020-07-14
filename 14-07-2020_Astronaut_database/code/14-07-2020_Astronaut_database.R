# =========================================
# PACOTES

library(tidyverse)
library(extrafont)
library(ggimage)
library(ggtext)
library(png)
library(grid)

# =========================================
# DADOS

# Dataset
astronauts <- tidytuesdayR::tt_load(2020, week = 29)[[1]]

# Transformação
astronauts %>% 
  mutate(
    age = astronauts$year_of_mission - astronauts$year_of_birth,
    occupation = if_else(occupation == "flight engineer","Flight engineer",occupation),
    occupation = if_else(occupation == "Other (Journalist)","Journalist",occupation),
    occupation = if_else(occupation == "pilot","Pilot",occupation),
    occupation = if_else(occupation == "Other (space tourist)","Space tourist",occupation),
    occupation = if_else(occupation == "Other (Space tourist)","Space tourist",occupation),
    occupation = if_else(occupation == "commander","Commander",occupation),
    occupation = if_else(occupation == "spaceflight participant","Spaceflight participant",occupation),
    military_civilian = if_else(military_civilian == "civilian","Civilian","Military")
  ) -> astronauts

astronauts %>% 
  group_by(year_of_mission) %>% 
  summarise(mean_age = mean(age)) -> mean_age

# =========================================
# SETUP


# Imagens
loadfonts(device = "win") 
img <- readPNG(here::here("img", "spaceship.png"))
rast <- rasterGrob(img, interpolate = TRUE)



# Textbox
glennjr <- "<p style='color:#BFC9CA; font-size:12pt'><b>John Herschel Glenn Jr.</b> was the first American to orbit the Earth in 1962...<br>...and also the oldest human to fly to space at age 77</p>"

# Curves
curve1 <- tibble(x = 1997.5,
                 y = 77.3,
                 xend = 1975, 
                 yend = 70)

curve2 <- tibble(x = 1962,
                 y = 42,
                 xend = 1970,
                 yend = 59)

# =========================================
# PLOT

ggplot(astronauts) +
  annotation_custom(rast,
                    xmin = 1970, xmax = 1975,
                    ymin = 66, ymax = 70) +
  geom_point(aes(x = year_of_mission,
                 y = age,
                 color = military_civilian), 
             size = 2.5,
             shape = 10) +
  geom_line(data = mean_age,
            aes(x = year_of_mission,
                y = mean_age),
            col = "#BFC9CA", 
            linetype = "dotted",
            alpha = .7,
            size = 2)  +
  annotate(geom = "text",
           x = 2021,
           y = 46,
           label = "mean age",
           color = "#BFC9CA",
           family = "Garamond",
           face = "bold"
           ) +
  scale_x_continuous(breaks = seq(1960,2020,10))+
  theme_void() +
  scale_color_manual(values = c("#F38155","#A11936")) +
  geom_textbox(
    data = tibble(
      x = 1975,
      y = 60,
      label = glennjr
    ),
    aes(
      x = x, y = y, label = gleenjr
    ),
    inherit.aes = F,
    family = "Garamond",
    size = 4,
    lineheight = 1.5,
    height = NULL,
    width = NULL,
    fill = NA,
    box.colour = NA,
    hjust = .5,
    vjust = 0
  ) + 
  geom_curve(data = curve1,
             aes(x = x,xend = xend,
                 y = y, yend = yend),
             curvature = 0.3,
             color = "#4D4D4D",
             linetype = "dashed") +
  geom_curve(data = curve2,
             aes(x = x, xend = xend,
                 y = y, yend = yend),
             curvature = -0.3,
             color = "#4D4D4D",
             linetype = "dashed") +
  labs(x = "Year", y = "Age",
       title = "Civilian and Military astronauts age at the time they flew to space",
       subtitle = "",
       caption = "Source: @pedro_drocha with data from Stavnichuk and Corlett (2020)") + 
  theme(
    axis.text = element_text(colour ="#E1FFFC",
                               family = "Garamond",
                               size = 12,
                               face = "bold.italic"),
    plot.margin = margin(4, 4, 4, 4),
    legend.position = c(.95, 1),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.box.margin = margin(3, 3, 3, 3),
    legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(colour ="#BFC9CA", 
                               family = "Garamond",
                               size = 12),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "#011936"),
    axis.line = element_line(colour = '#E1FFFC',size = 2),
    axis.text.y = element_text(margin = margin(0,4.5,0,0)),
    axis.text.x = element_text(margin = margin(4,0,0,0)),
    axis.title.y.left = element_text(colour ="#F4FFFD", 
                                family = "Garamond",
                                size = 14,
                                vjust = 0.95,
                                hjust = 0.2,
                                face = "bold.italic"),
    axis.title.x = element_text(colour ="#F4FFFD",
                                family = "Garamond",
                                face = "bold.italic",
                                size = 14,
                                hjust = 0.9),
    plot.title = element_text(colour = "#E1FFFC",
                              family = "Garamond",
                              face = "bold.italic",
                              size = 22,
                              hjust= 0.5),
    plot.subtitle = element_text(colour = "#AFAFAF"),
    plot.caption = element_text(colour = "#E1FFFC",
                                family = "Garamond",
                                face = "italic",
                                hjust = 0.5,
                                size = 10)
    ) -> plot



ggsave("14-07-2020_Astronaut-Database/plot/14-07-2020_Astronaut-Database.png",
       plot = plot)



