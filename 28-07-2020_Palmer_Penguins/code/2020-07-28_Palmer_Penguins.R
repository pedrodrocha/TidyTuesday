################################################################################################
# Title: Penguin species from the **Palmer Archipelago** and their different size measurements
# Author: @pedro_drocha
# Purpose: TidyTuesday Visualization - Week 31 - Palmer Penguins
# Date: 21-07-2020
################################################################################################

################# PACKAGES #####################################################################
library(tidyverse) 
library(tidytuesdayR) 
library(patchwork)
library(ggtext)
library(glue)
library(pdftools)

################### IMPORTING DATA #############################################################

tuesdata <- tidytuesdayR::tt_load(2020, week = 31)
penguins <- tuesdata$penguins

################### SETUP #####################################################################

penguins %>% 
  filter(species == "Adelie") %>% 
  drop_na() -> penguins_adelie



penguins %>% 
  filter(species == "Chinstrap") %>% 
  drop_na() -> penguins_chinstrap


penguins %>% 
  filter(species == "Gentoo") %>% 
  drop_na() -> penguins_gentoo


################### PLOT  ######################################################################

## Body Mass plot
penguins %>%
  drop_na() %>% 
  ggplot() +
  geom_histogram(aes(x = body_mass_g, fill = species, color = species), 
                 position = "identity", 
                 alpha = .4) +
  geom_vline(data = penguins_adelie,
             aes(xintercept = mean(body_mass_g)), 
             col = "#ff4c00", 
             linetype = "dashed",
             size = 1) + 
  geom_vline(data = penguins_chinstrap,
             aes(xintercept = mean(body_mass_g)), 
             col = "#7a28a3", 
             linetype = "dashed",
             size = 1) + 
  geom_vline(data = penguins_gentoo,
             aes(xintercept = mean(body_mass_g)), 
             col = "#008B8B", 
             linetype = "dashed",
             size = 1) +
  geom_text(data = penguins_adelie,
          aes(x = mean(penguins_adelie$body_mass_g) - 350,
          y = 17),
          label = glue("Mean: {round(mean(penguins_adelie$body_mass_g))}g"),
          col = "#ff4c00",
          size = 3) + 
  geom_text(data = penguins_chinstrap,
          aes(x = mean(penguins_chinstrap$body_mass_g) + 350,
          y = 12),
          label = glue("Mean: {round(mean(penguins_chinstrap$body_mass_g))}g"),
          col = "#7a28a3",
          size = 3) + 
  geom_text(data = penguins_gentoo,
          aes(x = mean(penguins_gentoo$body_mass_g) - 350,
          y = 17),
          label = glue("Mean: {round(mean(penguins_gentoo$body_mass_g))}g"),
          col = "#008B8B",
          size = 3) +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4")) +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  ylab("Count") +
  xlab("Body mass (g)") +
  ggtitle("**Body Mass**") +
  theme_minimal() +
  theme(legend.position = "None",
        text = element_text(family = "Garamond"),
        strip.text = element_text(size = 12),
        plot.title = element_markdown(family = "Garamond", hjust = 0.5)) -> p1



## Flipper Length plot
penguins %>%
  drop_na() %>% 
  ggplot() +
  geom_histogram(aes(x = flipper_length_mm, fill = species, color = species), 
                 position = "identity", 
                 alpha = .4) +
  geom_vline(data = penguins_adelie,
             aes(xintercept = mean(flipper_length_mm)), 
             col = "#ff4c00", 
             linetype = "dashed",
             size = 1) + 
  geom_vline(data = penguins_chinstrap,
             aes(xintercept = mean(flipper_length_mm)), 
             col = "#7a28a3", 
             linetype = "dashed",
             size = 1) + 
  geom_vline(data = penguins_gentoo,
             aes(xintercept = mean(flipper_length_mm)), 
             col = "#008B8B", 
             linetype = "dashed",
             size = 1) + 
  geom_text(data = penguins_adelie,
          aes(x = mean(penguins_adelie$flipper_length_mm) - 6,
          y = 21.5),
          label = glue("Mean: {round(mean(penguins_adelie$flipper_length_mm))}mm"),
          col = "#ff4c00",
          size = 3) + 
  geom_text(data = penguins_chinstrap,
          aes(x = mean(penguins_chinstrap$flipper_length_mm) + 6,
          y = 11),
          label = glue("Mean: {round(mean(penguins_chinstrap$flipper_length_mm))}mm"),
          col = "#7a28a3",
          size = 3) + 
  geom_text(data = penguins_gentoo,
          aes(x = mean(penguins_gentoo$flipper_length_mm) - 6,
          y = 19),
          label = glue("Mean: {round(mean(penguins_gentoo$flipper_length_mm))}mm"),
          col = "#008B8B",
          size = 3) + 
  xlab("Flipper length (mm)") +
  ylab("Countr") +
  ggtitle("**Flipper Length**") +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4")) +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  theme_minimal() +
  theme(legend.position = "None",
        strip.text = element_text(size = 12),
        text = element_text(family = "Garamond"),
        plot.title = element_markdown(family = "Garamond", hjust = 0.5)) -> p2


## Bill Depth plot
penguins %>%
  drop_na() %>% 
  ggplot() +
  geom_histogram(aes(x = bill_depth_mm, fill = species, color = species), 
                 position = "identity", 
                 alpha = .4) +
  geom_vline(data = penguins_adelie,
             aes(xintercept = mean(bill_depth_mm)), 
             col = "#ff4c00", 
             linetype = "dashed",
             size = 1) + 
  geom_vline(data = penguins_chinstrap,
             aes(xintercept = mean(bill_depth_mm)), 
             col = "#7a28a3", 
             linetype = "dashed",
             size = 1) + 
  geom_vline(data = penguins_gentoo,
             aes(xintercept = mean(bill_depth_mm)), 
             col = "#008B8B", 
             linetype = "dashed",
             size = 1) + 
  geom_text(data = penguins_adelie,
          aes(x = mean(penguins_adelie$bill_depth_mm) - 0.8,
          y = 20),
          label = glue("Mean: {round(mean(penguins_adelie$bill_depth_mm))}mm"),
          col = "#ff4c00",
          size = 3) + 
  geom_text(data = penguins_chinstrap,
          aes(x = mean(penguins_chinstrap$bill_depth_mm) + 0.8,
          y = 8),
          label = glue("Mean: {round(mean(penguins_chinstrap$bill_depth_mm))}mm"),
          col = "#7a28a3",
          size = 3) + 
  geom_text(data = penguins_gentoo,
          aes(x = mean(penguins_gentoo$bill_depth_mm) - 0.8,
          y = 18),
          label = glue("Mean: {round(mean(penguins_gentoo$bill_depth_mm))}mm"),
          col = "#008B8B",
          size = 3) + 
  xlab("Bill depth (mm)") +
  ylab("Count") +
  ggtitle("**Bill Depth**") +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4")) +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  theme_minimal() +
  theme(legend.position = "None",
        strip.text = element_text(size = 12),
        text = element_text(family = "Garamond"),
        plot.title = element_markdown(family = "Garamond", hjust = 0.5)) -> p3

## Bill Length plot
penguins %>%
  drop_na() %>% 
  ggplot() +
  geom_histogram(aes(x = bill_length_mm, fill = species, color = species), 
                 position = "identity", 
                 alpha = .4) +
  geom_vline(data = penguins_adelie,
             aes(xintercept = mean(bill_length_mm)), 
             col = "#ff4c00", 
             linetype = "dashed",
             size = 1) + 
  geom_vline(data = penguins_chinstrap,
             aes(xintercept = mean(bill_length_mm)), 
             col = "#7a28a3", 
             linetype = "dashed",
             size = 1) + 
  geom_vline(data = penguins_gentoo,
             aes(xintercept = mean(bill_length_mm)), 
             col = "#008B8B", 
             linetype = "dashed",
             size = 1) + 
  geom_text(data = penguins_adelie,
          aes(x = mean(penguins_adelie$bill_length_mm) - 2.5,
          y = 17),
          label = glue("Mean: {round(mean(penguins_adelie$bill_length_mm))}mm"),
          col = "#ff4c00",
          size = 3) + 
  geom_text(data = penguins_chinstrap,
          aes(x = mean(penguins_chinstrap$bill_length_mm) + 2.5,
          y = 11),
          label = glue("Mean: {round(mean(penguins_chinstrap$bill_length_mm))}mm"),
          col = "#7a28a3",
          size = 3) + 
  geom_text(data = penguins_gentoo,
          aes(x = mean(penguins_gentoo$bill_length_mm) - 2.5,
          y = 18),
          label = glue("Mean: {round(mean(penguins_gentoo$bill_length_mm))}mm"),
          col = "#008B8B",
          size = 3) +
  xlab("Bill length (mm)") +
  ylab("Count") +
  ggtitle("**Bill Length**") +
  scale_y_continuous(labels = scales::comma, expand = c(0,0)) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4")) +
  scale_fill_manual(values = c("darkorange","darkorchid","cyan4")) +
  theme_minimal() +
  theme(legend.position = "None",
        strip.text = element_text(size = 12),
        text = element_text(family = "Garamond"),
        plot.title = element_markdown(family = "Garamond", hjust = 0.5)) -> p4

## Patchwork
plot <- (p1 + p2) / (p3 + p4)  + 
  plot_annotation(title = "**Penguin** species from the **Palmer Archipelago** are all adorable, but their size measurements are quite different",  
                  subtitle = "<b style='color:#008B8B;'>Gentoos</b> have on average almost twice the _body mass_ of <b style='color:#7a28a3;'>Chinstraps</b> and <b style='color:#ff4c00;'> Adelies</b>.The _flippers_ of <b style='color:#008B8B;'>Gentoos</b> are also usually larger than<br> the ones from <b style='color:#ff4c00;'> Adelies</b>  or <b style='color:#7a28a3;'>Chinstraps</b>.<b style='color:#ff4c00;'> Adelies</b>\' _bill depth_  are the same size as those from <b style='color:#7a28a3;'>Chinstraps</b> and 3mm larger than the<br> ones from <b style='color:#008B8B;'>Gentoos</b>. On the other hand, their _bill length_ are usually smaller if compared to the ones from both <b style='color:#7a28a3;'>Chinstraps</b><br> and <b style='color:#008B8B;'>Gentoos</b>, whose _bill length_ are on average almost the same.",
                  caption = "TidyTuesday #31 | @pedro_drocha | Data: @kbgorman_ecoevo & @PalmerLTER team",
                  theme = theme(plot.title = element_markdown(family = "Garamond", size = 16, hjust = 0.5),
                                plot.subtitle = element_markdown(family = "Garamond",size = 13.5, hjust = 0.5),
                                plot.background = element_rect(fill = "#F7F8F8"),
                                plot.caption  = element_text(size = 11,hjust = 0.5, family = "Garamond", face = "italic"))) 


################### SAVING  #####################################################################
ggsave("2020-02-28_Palmer_Penguins.pdf",
       plot = plot,
       width = 10.5,
       height = 7.67,
       device = cairo_pdf)

pdf_convert(pdf = "2020-02-28_Palmer_Penguins.pdf",
            format = "png", dpi = 400)

