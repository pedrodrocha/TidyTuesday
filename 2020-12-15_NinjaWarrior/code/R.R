# ---- Packages

library(tidyverse)
library(png)
library(grid)
library(pdftools)
library(ggtext)
library(showtext)

# ---- Data

ninja_warrior <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-12-15/ninja_warrior.csv')


# ---- Prep
ninja_warrior %>%
  mutate(
    obstacle_name = case_when(
      obstacle_name == 'Warped Wall / Mega Wall' ~ 'Warped Wall',
      obstacle_name == 'Giant Log Grip' ~ 'Log Grip',
      TRUE ~ obstacle_name
    )
  ) %>%
  group_by(obstacle_order) %>%
  count() %>%
  rename('n_order' = n) -> order_total

ninja_warrior %>%
  mutate(
    obstacle_name = case_when(
      obstacle_name == 'Warped Wall / Mega Wall' ~ 'Warped Wall',
      obstacle_name == 'Giant Log Grip' ~ 'Log Grip',
      TRUE ~ obstacle_name
    )
  ) %>%
  group_by(obstacle_name,  obstacle_order)%>%
  count() %>%
  arrange(obstacle_order, -n) %>%
  ungroup() %>%
  left_join(.,order_total) %>%
  mutate(perc = round(n/n_order * 100)) %>%
  group_by(obstacle_order) %>%
  slice_head(n = 1) -> top_obstacle_per_order

# ---- Icons
trophy <- readPNG(here::here('2020-12-15_NinjaWarrior/trophy_icon.png'))
trophy_raster <- rasterGrob(trophy, interpolate=FALSE)


jump <- readPNG(here::here('2020-12-15_NinjaWarrior/jump_icon.png'))
jump_raster <- rasterGrob(jump, interpolate=FALSE)

ladder <- readPNG(here::here('2020-12-15_NinjaWarrior/ladder_icon.png'))
ladder_raster <- rasterGrob(ladder, interpolate=FALSE)


# ---- Font
font_add_google(name = 'Big Shoulders Display',family = 'bigshoulders')
font_add_google(name = 'News Cycle',family = 'newscycle')
font_add_google(name = 'Montserrat',family = 'montserrat')
showtext_auto()


# ---- Plot
top_obstacle_per_order %>%
  ggplot(
    aes(
      y = reorder(obstacle_name, obstacle_order),
      x = obstacle_order
    )
  ) +
  geom_step(
    aes(
      group = 1
    ),
    color = "#7D4D55",
    # color = "#f0f0f0",
    size = 2
  ) +
  geom_point(
    size = 3,
    col = "#7D4D55"
  ) +
  geom_richtext(
    aes(
      label = glue::glue(" **{obstacle_order}**. {obstacle_name} (**{perc}%**)"),
      hjust = if_else(obstacle_name == 'Quintuple Steps',.5,1.06),
      vjust = if_else(obstacle_name == 'Quintuple Steps',1.5,-.5)
    ),
    color = "#f0f0f0",
    fill = NA,
    label.color = NA,
    family = 'newscycle',
    size = 4.5
  ) +
  annotation_custom(
    trophy_raster,
    xmin = 9.5,
    xmax = 10.4,
    ymin = 10
  ) +
  annotation_custom(
    jump_raster,
    xmin = 3.1,
    xmax = 4.0,
    ymax = 6.5
  ) +
  annotation_custom(
    ladder_raster,
    xmin = 6.18,
    xmax = 7.08,
    ymax = 12.65
  ) +
  geom_text(
    aes(
      x = 7,
      y = 'Bridge of Blades',
      label = "Ninja Warrior"
    ),
    family = 'bigshoulders',
    color = '#f0f0f0',
    size = 28,
    vjust = -.53
  ) +
  geom_richtext(
    aes(
      x = 4.5,
      y = 'Flying Bar',
      label = "What does a typical path looks like?"
    ),
    color = "#f0f0f0",
    fill = NA,
    label.color = NA,
    family = 'newscycle',
    size = 7.8
  ) +
  geom_richtext(
    aes(
      x = 4.5,
      y = 'Flying Bar',
      label = "For each obstacle number the plot shows the most frequent challenge contestants face"
    ),
    color = "#f0f0f0",
    fill = NA,
    label.color = NA,
    family = 'newscycle',
    size = 4.3,
    label.padding = unit(c(3, 0.25, 0.25, 7.8), "lines")
  ) +
  coord_flip() +
  scale_x_continuous(
    limits = c(1,10),
    breaks = seq(1,10,1),position = 'bottom'
  ) +
  labs(
    caption = "#TidyTuesday 51, 2020 | @pedro_drocha | Data: sasukepedia | Icons: The Noun Project"
  ) +
  theme_void() +
  theme(
    plot.margin = margin(30,20,20,20),
    plot.background = element_rect(
      fill = "#444444", color = "#444444"
    ),
    plot.caption = element_text(
      color = '#f0f0f095',
      family = 'montserrat',
      size = 6.6
    )
  ) -> plot



ggsave(
  plot = plot,
  filename = here::here('2020-12-15_NinjaWarrior/plot/plot1.pdf'),
  device = cairo_pdf,
  width = 16,
  height = 9,
  dpi = 400
)

pdf_convert(pdf = here::here('2020-12-15_NinjaWarrior/plot/plot1.pdf'),
            format = "png", dpi = 400)


