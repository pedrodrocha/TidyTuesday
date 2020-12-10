# Packages
library(tidyverse)
library(showtext)
library(ggrepel)
library(pdftools)
library(jsonlite)
# Loading
tuesdata <- tidytuesdayR::tt_load('2020-11-24')
tuesdata <- tidytuesdayR::tt_load(2020, week = 48)
hike_data <- tuesdata[[1]]


# Cleaning
hike_data %>%
  mutate(
    region =
      case_when(
        str_detect(location, '--') ~ str_extract(location,"^(.+?) --") %>%
                                      str_remove(.,' --') %>%
                                      str_trim(),
        TRUE ~ location
    ),
    length = as.numeric(str_remove(length,' miles(.*)')),
    rating = as.numeric(ifelse(rating == '0', NA,rating)),
    gain = as.numeric(gain),
    highpoint = as.numeric(highpoint),
    length_km = round(length * 1.609,2),
    gain_m = round(gain * 3.281,2),
    highpoint_m = round(highpoint * 3.281,2)
  ) %>%
  relocate(length_km, .after = length) %>%
  relocate(gain_m, .after = gain) %>%
  relocate(highpoint_m, .after = highpoint) %>%
  relocate(region, .before = location)  %>%
  select(-location) -> clean_hike




# Grouping and summarizing
clean_hike %>%
  group_by(region) %>%
  summarise(
    mean_lenght_km = mean(length_km),
    mean_gain_m = mean(gain_m),
    mean_highpoint_m = mean(highpoint_m),
    mean_rating = mean(rating)
  ) -> summarised_hike

clean_hike %>%
  select(region, features) %>%
  unnest_longer(col = features) %>%
  group_by(region) %>%
  count(features) %>%
  arrange(region,-n) %>%
  top_n(5) %>%
  select(-n) %>%
  mutate(features = paste0(features, collapse = ',')) %>%
  distinct() -> features_summarised

left_join(summarised_hike, features_summarised) %>%
  mutate(id = row_number()) %>%
  write_csv(
    path = here::here('24-11-2020_Trails/hike_trails.csv')
  )

# Load font
font_add_google(name = 'Montserrat',family = 'montserrat')
showtext_auto()



# Plot
summarised_hike %>%
  ggplot(
    aes(x = reorder(region,mean_highpoint_m), y = mean_highpoint_m)
  ) +
  geom_line(
    group = 1,
    color = '#de997e',
    size = 26
  ) +
  geom_point(
    size = 2.5,
    color = '#444444',
    shape = 18
    ) +
  geom_text_repel(
    aes(x = reorder(region,mean_highpoint_m),
        y = mean_highpoint_m,
        label = reorder(region,mean_highpoint_m)),
    color = '#444444',
    family = 'montserrat',
    seed = 222,
    size = 5
  ) +
  scale_y_continuous(
    limits = c(1500,18000),
    expand = c(0,0),
    breaks = seq(1500,18000,2000),
    position = "right"
      ) +
  labs(
    title = '\nWashington Trails',
    subtitle = 'The plot shows the average Highest point (m) above sea level of trails per Washington region',
    caption = 'Tidy Tuesday Week 48 | Viz: @pedro_drocha | Source: TidyX Crew (Ellis Hughes & Patrick Ward)'
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = '#ffffff', color = '#f0f0f0' ),
    panel.grid.major.y = element_line(color = '#44444420'),
    plot.margin = margin(5,20,5,20),
    axis.text.y = element_text(
      margin = margin(0,0,0,10),
      size = 10,
      color = '#44444480',
      family = 'montserrat'
      ),
    plot.caption = element_text(
      size = 9,
      color = '#44444490',
      family = 'montserrat',
      hjust = 0,
      margin = margin(25,0,0,0)
      ),
    plot.title = element_text(
      size = 32,
      color = '#444444',
      family = 'montserrat',
      face = 'bold'
    ),
    plot.subtitle = element_text(
      size = 14.7,
      color = '#444444',
      family = 'montserrat',
      margin = margin(10,0,20,0)
    )
  ) -> trails


# Saving
ggsave(
  plot = trails,
  path = here::here('24-11-2020_Trails'),
  filename = 'trails.pdf',
  width = 10,
  height = 12,
  device= cairo_pdf,
  dpi = 300
)

pdf_convert(pdf = here::here('24-11-2020_Trails/trails.pdf'),
            format = "png", dpi = 400,)

