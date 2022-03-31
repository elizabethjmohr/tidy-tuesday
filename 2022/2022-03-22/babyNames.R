library(tidyverse)
library(ggwaffle)
library(ggbeeswarm)
library(sysfonts)
library(showtext)
library(cowplot)
library(colorspace)
library(patchwork)
library(shadowtext)

# Read in data
data <- tidytuesdayR::tt_load(2022, week = 12)

# Add fonts
font_add_google("Source Sans Pro", family = "sansPro")
font_add_google("Quicksand", family = "quicksand")
showtext_auto()
showtext_opts(dpi = 300)

# Define helper functions
source(here::here("2022", "2022-03-22", "functions.R"))

# Plot number of unique baby names vs. time
uniqueBabyNamesVTime <- data$babynames %>% 
  group_by(year) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = year, y = n))+
  #geom_line(size = 1.5, color = "#567572FF")+
  geom_line(size = 1.5, color = "#030E4F")+
  ggtitle("Number of unique baby names through the years")+
  theme_minimal_grid()+
  scale_x_continuous(breaks = seq(1880, 2020, by = 10), expand = c(0,0))+
  scale_y_continuous(breaks = c(0,10000,20000,30000), 
                     labels = c("0", "10,000", "20,000", "30,000"),
                     limits = c(0,36000),
                     expand = c(0,0))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family = "sansPro", size = 12),
        axis.text.y = element_text(family = "sansPro", size = 12),
        plot.margin = margin(0,0,0,0),
        plot.title = element_text(family = "sansPro",face = "bold", size = 13),
        plot.title.position = "plot")

# Specify generations
generations <- tibble(
  year = seq(1928,2012),
  generation = c(rep("Silent", 18), #1928-1945
                 rep("Baby Boomers", 19), #1946-1964
                 rep("Generation X", 16),#1965-1980
                 rep("Millenials", 16),#1981-1996
                 rep("Generation Z", 16)),#1997-2012
  age = 2022 - year # age in 2022
) 

# Plot percent of each generation that has a name in the top 50 
topFiftyNamesByGen <- data$babynames %>%
  left_join(generations, by = c("year")) %>%
  drop_na(generation) %>%
  group_by(generation, sex, name) %>%
  summarize(sum = sum(n, na.rm = TRUE)) %>%
  mutate(rank = rank(-sum)) %>%
  mutate(top = rank <= 50) %>%
  ungroup() %>%
  group_by(generation, top) %>%
  summarise(n = sum(sum, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = top, values_from = n, names_prefix = "top_") %>%
  mutate(percentTop = 100*top_TRUE/(top_TRUE+top_FALSE),
         nTop = round(percentTop),
         nNotTop = 100 - nTop) %>%
  select(generation, nTop, nNotTop) %>%
  pivot_longer(2:3, names_to = "top", values_to = "n")
  
nameInTopWafflePlot<- wrap_elements(
  wrap_plots(map(unique(generations$generation), 
                 makeWaffle, 
                 nameSummary = topFiftyNamesByGen),
             nrow = 1),
  clip = FALSE)+
  ggtitle("Proportion of babies given one of the 50 most popular names of their generation") +
  theme(plot.title = element_text(family = "sansPro",size = 13, face = "bold", lineheight = 0.5),
        plot.title.position = "plot")


legend <- makeWaffleLegend("Generation Z", topFiftyNamesByGen)

mainTitle <- uniqueNamePlotTitle <- 
  grid::textGrob(label = "BABIES BORN IN THE UNITED STATES ARE \nINCREASINGLY LIKELY TO BE GIVEN UNIQUE NAMES",
                 x = unit(0, "npc"),
                 gp = grid::gpar(fontfamily = "quicksand", 
                                 fontsize = 15, 
                                 fontface = "bold",
                                 lineheight = 0.8),
                 hjust = 0)


# Patch plots together
combinedPlot <- (wrap_elements(mainTitle)/
                   wrap_elements(uniqueBabyNamesVTime)/ 
                   plot_spacer()/
                   nameInTopWafflePlot/
                   legend) + 
  plot_layout(heights = c(3,10,0.5,8,1))

ggsave(filename = "./2022/2022-03-22/babyNames.pdf", 
       plot = combinedPlot,
       device= "pdf",
       width = 6.7, 
       height= 5.5,
       units = "in")

# Plot top-ranked baby names vs. time
topNamesByYear <- data$babynames %>%
  filter(year >=1928) %>%
  group_by(sex, year) %>%
  mutate(rank = rank(-n)) %>%
  filter(rank == 1) %>%
  arrange(year, sex, rank) %>%
  ungroup()

rankedNames <- topNamesByYear %>%
  group_by(name, sex) %>%
  summarize(firstYear = min(year), 
            nTop = n(), 
            .groups = "drop") %>%
  arrange(sex, desc(firstYear))

topNamesByYear <- topNamesByYear %>%
  mutate(name = factor(name, levels = rankedNames$name))

ggplot(topNamesByYear %>% filter(sex == "F"), aes(x = year, y = name)) + 
  geom_tile()+
  scale_x_continuous(breaks = seq(1925, 2015, by = 5))+
  theme_minimal_vgrid()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

