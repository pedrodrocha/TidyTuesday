# PACKAGES
library(claremontrun)
library(tidyverse)
library(lubridate)
library(cowplot)
library(stringr)
library(skimr)
library(ggthemes)
library(ggdark)
library(RColorBrewer)
library(knitr)
library(ggrepel)
library(textcat)
library(ggthemr)
library(skimr)
library(stringr)
library(hrbrthemes)
library(corrr)
# ============================================================================================================================ #
library(tidyverse)
library(claremontrun)
library(hrbrthemes)

characters %>%  
  select(character,visible_tears_number_of_intances) %>%
  group_by(character) %>% 
  summarise(n = sum(visible_tears_number_of_intances, na.rm = TRUE)) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  mutate(character = str_remove(character,"=(.*)")) -> dat

characters %>%  
  select(character,visible_tears_number_of_intances) %>%
  group_by(character) %>% 
  summarise(n = sum(visible_tears_number_of_intances, na.rm = TRUE)) %>% 
  arrange(desc(n)) %>% 
  slice(1:10)

dat %>% 
  ggplot(aes(x = reorder(character,
                         n),
             y = n)) +
  geom_col(fill = "#008080", alpha = .4) +
  geom_text(aes(label = n),
            hjust=0, nudge_y=0.4) +
  coord_flip() +
  theme_ipsum(axis_title_size = 12,
              axis_title_face = 'italic',
              caption_size = 8) +
  labs(x = "Character", y = "Visible number of tear instances",
       title = "Ranking of crying characters",
       subtitle = "Chris Claremont’s iconic run on Uncanny X-Men (1975-1991)",
       caption = "Source: Claremont Run project")



