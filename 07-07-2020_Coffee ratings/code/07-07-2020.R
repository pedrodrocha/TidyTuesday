# Packages
library(tidyverse)
library(stringr)
library(ggrepel)
library(hrbrthemes)

# Importe o dataset
tuesdata <- tidytuesdayR::tt_load(2020, week = 28)
coffe_ratings <- tuesdata$coffee_ratings


# Crie uma função para Iteração
plot <- function(year){ # Recebe ano de argumento
  
   # Transformação dos dados
  transform <- coffe_ratings %>% 
    mutate(country_of_origin = ifelse(country_of_origin == "Tanzania, United Republic Of","Tanzania",country_of_origin)) %>%  # Limpando o nome da Tanzania
    mutate(grading_year = as.numeric((str_extract(grading_date, "[0-9]{4}")))) %>%  # Usando regex para criar uma coluna de anos
    filter(species == "Arabica") %>% # Filtrando para o Arabica
    filter(grading_year == year) %>% # O argumento do ano vem aqui 
    select(country_of_origin, grading_year, total_cup_points) %>%
    group_by(country_of_origin,grading_year)  %>% # agrupando por país e ano
    summarise(median = median(total_cup_points)) %>% 
    ungroup() %>%  
    group_by(grading_year) %>%  # agrupando por ano
    top_n(5) # Pegando top 5 por ano
  
  # plot base
  plt <- transform %>% 
    ggplot() +
    geom_point(aes(x = reorder(as.factor(country_of_origin), median), # Reordenando o eixo X
                   y = median), color = "brown") +
    geom_label_repel(aes(x = reorder(as.factor(country_of_origin), median), # Reordenando o eixo X
                         y = median,
                         label = as.factor(country_of_origin)),
                     seed = 123) +
    labs(title = "Which country has the best Arabica Coffee in the World?",
         subtitle = paste0("According to Coffee Quality Institute's trained reviewers • ", transform$grading_year), # usando paste0() para mudar o subtítulo conforme o ano 
         y = "Median grade", x = "",
         caption = "Data: James LeDoux (@jmzledoux)") +
    theme_ipsum(axis_title_just = "center") +
    theme(legend.position = "none",
          axis.text.x = element_blank(),
          axis.title.y = element_text(face = "bold", size = 12),
          axis.ticks.x = element_blank(),
          plot.title = element_text(size=22,face = "bold.italic"),
          plot.subtitle = element_text(size =15),
          plot.background = element_rect(fill = "#f9e8d8"))
  return(plt)
}


# Dataframe de anos para iteração
years <- tibble(years = as.character(2010:2018))

# Iteração para criar um plot pra cada ano
for (year in years$years){
  plot(year)
  ggsave(filename = paste0(year,".png"),
         plot = last_plot())
}

# Criando o gif
gifski::gifski(png_files = c("2010.png","2011.png","2012.png","2013.png","2014.png","2015.png","2015.png","2016.png","2017.png","2018.png"),
               width = 802, height = 323) # PERIGO - o tamanho aqui tem que ser em pixels. Para conversão dos valores: http://auctionrepair.com/pixels.html

