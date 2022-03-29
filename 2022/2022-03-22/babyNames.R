library(tidyverse)
library(ggwaffle)
library(ggbeeswarm)
library(sysfonts)
library(showtext)
library(cowplot)
library(colorspace)
library(patchwork)

data <- tidytuesdayR::tt_load(2022, week = 12)

# Add font
font_add_google("Source Sans Pro", family = "sansPro")
showtext_auto()
showtext_opts(dpi = 300)

# Ideas: 
# 1.How do top-ranked baby names for each year fare over time? 
# Which baby names are consistently popular vs. transiently popular? 

rankedNames <- data$babynames %>%
  group_by(year, sex) %>%
  mutate(rank = rank(-prop))

topNames <- rankedNames %>%
  filter(rank == 1) %>%
  ungroup() %>%
  distinct(name, sex) 

numberOfOneRanks <- rankedNames %>% 
  right_join(topNames, 
             by = c("sex", "name")) %>%
  group_by(name,sex) %>%
  filter(rank == 1) %>%
  summarize(NumOnes = n()) %>%
  arrange(sex, desc(NumOnes))

ggplot(rankedNames %>% 
         right_join(topNames, 
                  by = c("sex", "name")) %>%
         mutate(name = factor(name, levels = numberOfOneRanks$name)), 
       aes(x = year, y = rank, group = name, color = sex)) + 
  geom_line()+
  scale_y_reverse()+
  theme_minimal_grid()+
  facet_wrap(~name)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(rankedNames %>% 
         right_join(topNames, 
                    by = c("sex", "name")) %>%
         mutate(name = factor(name, levels = numberOfOneRanks$name)), 
       aes(x = name, y = rank,color = sex)) + 
  geom_quasirandom(alpha = 0.6,  stroke = 0, size =2)+
  scale_y_reverse(breaks = c(1, 100, 200, 300, 400, 500), limits = c(500,1))+
  scale_x_discrete(position = "top") +
  theme_minimal_grid()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(),
        legend.position = "bottom")
# rectangular heatmap showing which name was #1 for M and F

# 2.Number of unique baby names vs. time
data$babynames %>% 
  group_by(year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n))+
  geom_line(size = 1.5)+
  ylim(0,36000)+
  ylab("unique baby names")+
  theme_minimal_grid()+
  theme(axis.title.x = element_blank())

# 3.Find 5 most common names by generation - 
# Waffle plots of proportion of people with a certain name 
# belonging to a each generation
generations <- tibble(
  year = seq(1880,2015),
  generation = c(rep("Traditionalists", 66),
                 rep("Baby Boomers", 19),
                 rep("Generation X", 12),
                 rep("Millenials", 19),
                 rep("Gen Z", 20)),
  age = 2022 - year # age in 2022
) 

topNames <- data$babynames %>%
  group_by(name, sex) %>%
  summarize(sum = sum(n, na.rm = TRUE), .groups = "drop") %>%
  group_by(sex) %>%
  mutate(rank = rank(-sum)) %>%
  filter(rank <=5) %>%
  arrange(sex,rank) %>%
  ungroup()

topNamesSummary <- data$babynames %>%
  right_join(topNames %>% select(name,sex), by = c("name", "sex")) %>%
  left_join(generations, by = c("year")) %>%
  left_join(data$lifetables %>% 
              filter(year == 2010), # Used 2010 lifetable, assuming 2022 is too different
            by = c("age" = "x", "sex" = "sex")) %>%
  drop_na(lx) %>%
  mutate(n_surviving = round(n*lx/100000)) %>%
  group_by(name, sex, generation) %>%
  summarize(n_in_2022 = sum(n_surviving)) %>%
  mutate(percent = 100*n_in_2022/sum(n_in_2022)) %>%
  nest() %>%
  mutate(data = map(data, sumTo100)) %>%
  unnest(cols = data)

topNamesPerGen <- data$babynames %>%
  left_join(generations, by = c("year")) %>%
  drop_na(generation) %>%
  group_by(generation, name, sex) %>%
  summarize(sum = sum(n, na.rm = TRUE), .groups = "drop") %>%
  group_by(generation, sex) %>%
  mutate(rank = rank(-sum)) %>%
  filter(rank <=5) %>%
  arrange(generation, sex,rank) %>%
  ungroup()

topNamesPerGenSummary <- data$babynames %>%
  right_join(topNamesPerGen %>% select(name,sex), by = c("name", "sex")) %>%
  left_join(generations, by = c("year")) %>%
  left_join(data$lifetables %>% 
              filter(year == 2010), # Used 2010 lifetable, assuming 2022 is too different
            by = c("age" = "x", "sex" = "sex")) %>%
  drop_na(lx) %>%
  mutate(n_surviving = round(n*lx/100000)) %>%
  group_by(name, sex, generation) %>%
  summarize(n_in_2022 = sum(n_surviving)) %>%
  mutate(percent = 100*n_in_2022/sum(n_in_2022)) %>%
  nest() %>%
  mutate(data = map(data, sumTo100)) %>%
  unnest(cols = data)

topMaleNames <- topNames %>%
  filter(sex == "M") %>%
  mutate(plot = map(name, makeWaffle, nameSummary = topNamesSummary)) 

topMaleNamesPerGen <- topNamesPerGen %>%
  filter(sex == "M") %>%
  mutate(plot = map(name, makeWaffle, nameSummary = topNamesPerGenSummary)) %>%
  select(generation, plot) %>%
  group_by(generation) %>%
  nest() %>%
  mutate(row = map(data, ~wrap_plots(.x$plot, nrow = 1))) %>%
  mutate(generation = factor(generation,levels = c("Traditionalists", "Baby Boomers", "Generation X", "Millenials", "Gen Z"))) %>%
  arrange(generation)

topFemaleNames <- topNames %>%
  filter(sex == "F") %>%
  mutate(plot = map(name, makeWaffle, nameSummary = topNamesSummary)) 

topFemaleNamesPerGen <- topNamesPerGen %>%
  filter(sex == "F") %>%
  mutate(plot = map(name, makeWaffle, nameSummary = topNamesPerGenSummary)) %>%
  select(generation, plot) %>%
  group_by(generation) %>%
  nest() %>%
  mutate(row = map(data, ~wrap_plots(.x$plot, nrow = 1))) %>%
  mutate(generation = factor(generation,levels = c("Traditionalists", "Baby Boomers", "Generation X", "Millenials", "Gen Z"))) %>%
  arrange(generation)

legend <-  makeWaffleLegend("Elizabeth", topNamesSummary)

wrap_plots(topMaleNamesPerGen$row, nrow = 5)/wrap_elements(legend) +
  plot_layout(heights = c(11,1))
wrap_plots(topFemaleNamesPerGen$row, nrow = 5) 
wrap_plots(topMaleNames$plot, nrow = 1)/
wrap_plots(topFemaleNames$plot, nrow = 1) +
  legend + 
  plot_layout(heights = c(3,3,1))


