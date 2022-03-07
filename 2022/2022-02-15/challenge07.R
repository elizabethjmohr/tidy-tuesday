library(tidyverse)
library(showtext)
library(patchwork)
library(here)
library(ggpattern)

# Read in and melt data
data <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge07/data.csv")%>%
  pivot_longer(cols = Widowed:Single,
               names_to = "congigalCondition",
               values_to = "percent") %>%
  mutate(congigalCondition = factor(congigalCondition, levels = c("Widowed", "Married", "Single"))) %>%
  mutate(Group = if_else(Group == "Over 65", "AGES\n OVER 65", Group)) 

# Specify color scheme and fonts
colors <- c("#385697","#a31938", "#54845b")

font_add_google("Public Sans", family = "public")
font_add_google("Bitter", family = "scope")
showtext_auto()
showtext_opts(dpi = 300)

# Define theme for plots
duBoisTheme <- theme(panel.background = element_blank(),
                     legend.position = "none",
                     axis.ticks = element_blank(),
                     axis.title = element_blank(),
                     panel.ontop = TRUE,
                     plot.margin = margin(0,0,0,0),
                     panel.grid.major.x = element_line(color = "#2f2f2f", size = 0.18),
                     panel.grid.minor.x = element_line(color = "#2f2f2f", size = 0.08),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     plot.title = element_text(hjust = 0.5, margin = margin(), size = 7, face = "bold", family = "public"),
                     axis.text.x = element_text(size = 5, face = "bold", family = "public"),
                     axis.text.y = element_text(size = 7, face = "bold", family = "public", vjust = 0))

# Define theme for title text
textTheme <- theme(panel.background = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(),
                   axis.text = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   plot.margin = margin(-0,-0,0,0))

# Define function to make text grobs
makeText <- function(text, fontsize, fontfamily = "scope", fontface = "plain"){
  ggplot() + 
    geom_text(aes(x = 1, y = 1, 
                  label = text),
              size = fontsize,
              family = fontfamily, 
              fontface = fontface)+
    textTheme+
    ylim(0.995,1.005)
}

# Define aesthetics that get re-used
labelTextSize <- 3.1
patternScale <- 0.04

#### Normal Version ####
# Plot male data
malePlot <- ggplot(data = data %>%
                     filter(Gender == "Male"), 
                   aes(x = percent,
                       y = Group,
                       fill = congigalCondition)) +
  geom_col(width = 1.0)+
  scale_x_reverse(expand = c(0,0), 
                  breaks = seq(100, 0, by = -10),
                  minor_breaks = seq(100, 0, by = -2))+
  scale_y_discrete(expand = c(0,0))+
  scale_fill_manual(values = rev(colors))+
  ggtitle("MALES.")+
  geom_hline(yintercept = seq(from = 0.5, to = 8.5, by = 1), 
             color = "#2f2f2f", size = 0.18)+
  annotate("text", x = 35, y = 1.8, label= "SINGLE", angle = 45, size =labelTextSize , fontface = "bold", family = "public")+
  annotate("text", x = 55, y = 5.4, label= "MARRIED", angle = 45, size = labelTextSize, fontface = "bold", family = "public")+
  annotate("text", x = 92, y = 8.5, label= "WIDOWED", angle = 70, size = 2.5, fontface = "bold", family = "public")+
  duBoisTheme

# Plot female data
femalePlot <- ggplot(data = data%>%
                       filter(Gender == "Female"),
                     aes(x = percent, 
                         y = Group, 
                         fill = congigalCondition)) + 
  geom_col(width = 1.0)+
  scale_fill_manual(values = rev(colors))+
  scale_y_discrete(position = "right", 
                   expand = c(0,0))+
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(0, 100, by = 10),
                     minor_breaks = seq(0, 100, by = 2))+
  ggtitle("FEMALES.")+
  geom_hline(yintercept = seq(from = 0.5, to = 8.5, by = 1), 
             color = "#2f2f2f", size = 0.18)+
  annotate("text", x = 35, y = 1.8, label= "SINGLE", angle = 315, size = labelTextSize, fontface = "bold", family = "public")+
  annotate("text", x = 55, y = 5.4, label= "MARRIED", angle = 315, size = labelTextSize, fontface = "bold", family = "public")+
  annotate("text", x = 92, y = 7.2, label= "WIDOWED", angle = 290, size = 2.5, fontface = "bold", family = "public")+
  duBoisTheme

# Make title plots
title1 <- makeText("Conjugal condition of American Negros according to age periods", 2.6)
title2 <- makeText("Condition conjugale des Nègres Americains au point du vue de l' age.", 2)
title3 <- makeText("Done by Atlanta University.", 1.5)
xAxis <- makeText("PER CENTS.", 2, "public","bold")

citations <- ggplot() + 
  geom_text(aes(x = 1, y = 1, 
                label = "Inspired by W.E.B. DuBois | Data from #DuBoisChallenge2022 | Recreated by @libbymohr"),
            size = 2,
            family = "public")+
  textTheme+
  ylim(0.99,1.01)

# Make line that separates title text
line <- ggplot()+
  geom_segment(aes(x = 2, xend = 3, y = 1, yend = 1), size = 0.2)+
  xlim(c(0,5))+
  textTheme

# Arrange plots
layout <- "
AA
HH
BB
II
CC
##
DE
FF
GG
"
fullPlot <- wrap_plots(A = title1, B = title2, C = title3, D = malePlot, E = femalePlot, F = xAxis,G = citations,H= line,I = line,
           design = layout, heights = c(2, 1, 1, 1, 1, 1, 45, 1.5, 1)) & 
  theme(plot.background = element_rect(fill = "#e8d9c4", color = "#e8d9c4"))

ggsave(file = "./2022/DuBoisChallenge/challenge07.png", plot = fullPlot, width = 5.5, height = 6.3)

#### Alternative Version ####
polygonData <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge07/data.csv") %>%
  mutate(ymax = as.numeric(factor(Group))) %>%
  mutate(ymin = ymax - 1) %>%
  pivot_longer(c(ymax, ymin), names_to = "yType", values_to = "y") %>%
  mutate(xmin_Single = 0, xmax_Single = Single,
         xmin_Married = Single, xmax_Married= Single+Married,
         xmin_Widowed = xmax_Married, xmax_Widowed = 100) %>%
  pivot_longer(cols = xmin_Single:xmax_Widowed, 
               names_to = "xType", 
               values_to = "x") %>%
  mutate(congigalCondition = sapply(strsplit(xType, '_'), `[`, 2)) %>% 
  mutate(xType = sapply(strsplit(xType, '_'), `[`, 1)) %>% 
  mutate(upper_lower = case_when(
    xType == "xmax" ~ 1,
    TRUE ~2
  )) %>%
  mutate(index = case_when(
    yType == "ymin" & xType == "xmax" ~1,
    yType == "ymax" & xType == "xmax" ~2,
    yType == "ymax" & xType == "xmin" ~3 ,
    TRUE ~ 4
  )) %>%
  mutate(Group = if_else(Group == "Over 65", "AGES\n OVER 65", Group)) 

upperPoints <- polygonData %>%
  filter(upper_lower == 1) %>%
  arrange(Group, index)

lowerPoints <-  polygonData %>%
  filter(upper_lower == 2) %>%
  arrange(desc(Group), index)

green <- here("2022", "DuBoisChallenge", "green.png")
red <- here("2022", "DuBoisChallenge", "red.png")
blue <- here("2022", "DuBoisChallenge", "blue.png")
femalePlot <- ggplot(data = (upperPoints %>% bind_rows(lowerPoints)) %>% filter(Gender == "Female", congigalCondition == "Single"))+
  geom_polygon_pattern(aes(x = x, y = y), 
                       pattern = 'image', 
                       pattern_filename = blue,
                       pattern_type = 'tile', 
                       pattern_scale =patternScale) + 
  geom_polygon_pattern(data = (upperPoints %>% bind_rows(lowerPoints)) %>% filter(Gender == "Female", congigalCondition == "Married"), 
               aes(x = x, y = y), 
               pattern = 'image', 
               pattern_filename = red,
               pattern_type = 'tile', 
               pattern_scale = patternScale) + 
  geom_polygon_pattern(data = (upperPoints %>% bind_rows(lowerPoints)) %>% filter(Gender == "Female", congigalCondition == "Widowed"), 
               aes(x = x, y = y),
               pattern = 'image', 
               pattern_filename = green,
               pattern_type = 'tile', 
               pattern_scale = patternScale)+
  scale_y_continuous(breaks = seq(0.6,8.6 , by = 1),
                     labels = polygonData$Group %>% unique(),
                     limits = c(0, 9.0),
                     expand = c(0,0),
                     position = "right")+
  scale_x_continuous(expand = c(0,0),
                     breaks = seq(10, 100, by = 10),
                     minor_breaks = seq(0, 100, by = 2))+
  ggtitle("FEMALES.")+
  geom_hline(yintercept = seq(from = 0, to = 8, by = 1), 
             color = "#2f2f2f", size = 0.18)+
  annotate("text", x = 35, y = 1.4, label= "SINGLE", angle = 315, size = labelTextSize, fontface = "bold", family = "public")+
  annotate("text", x = 55, y = 5, label= "MARRIED", angle = 315, size = labelTextSize, fontface = "bold", family = "public")+
  annotate("text", x = 92, y = 7.2, label= "WIDOWED", angle = 290, size = 2.5, fontface = "bold", family = "public")+
  duBoisTheme

malePlot <- ggplot(data = (upperPoints %>% bind_rows(lowerPoints)) %>% filter(Gender == "Male", congigalCondition == "Single"))+
  geom_polygon_pattern(aes(x = x, y = y), 
                       pattern = 'image', 
                       pattern_filename = blue,
                       pattern_type = 'tile', 
                       pattern_scale = patternScale) + 
  geom_polygon_pattern(data = (upperPoints %>% bind_rows(lowerPoints)) %>% filter(Gender == "Male", congigalCondition == "Married"), 
                       aes(x = x, y = y), 
                       pattern = 'image', 
                       pattern_filename = red,
                       pattern_type = 'tile', 
                       pattern_scale = patternScale) + 
  geom_polygon_pattern(data = (upperPoints %>% bind_rows(lowerPoints)) %>% filter(Gender == "Male", congigalCondition == "Widowed"), 
                       aes(x = x, y = y),
                       pattern = 'image', 
                       pattern_filename = green,
                       pattern_type = 'tile', 
                       pattern_scale = patternScale)+
  scale_y_continuous(breaks = seq(0.6,8.6 , by = 1),
                     labels = polygonData$Group %>% unique(),
                     limits = c(0, 9.0),
                     expand = c(0,0))+
  scale_x_reverse(expand = c(0,0),
                     breaks = seq(100,10, by = -10),
                     minor_breaks = seq(100, 0, by = -2))+
  ggtitle("MALES.")+
  geom_hline(yintercept = seq(from = 0, to = 8, by = 1), 
             color = "#2f2f2f", size = 0.18)+
  annotate("text", x = 35, y = 1.3, label= "SINGLE", angle = 45, size =labelTextSize , fontface = "bold", family = "public")+
  annotate("text", x = 55, y = 4.9, label= "MARRIED", angle = 45, size = labelTextSize, fontface = "bold", family = "public")+
  annotate("text", x = 92, y = 8.0, label= "WIDOWED", angle = 70, size = 2.5, fontface = "bold", family = "public")+
  duBoisTheme

fullPlot <- wrap_plots(A = title1, B = title2, C = title3, D = malePlot, E = femalePlot, F = xAxis,G = citations,H= line,I = line,
                       design = layout, heights = c(2, 1, 1, 1, 1, 1, 45, 1.5, 1)) & 
  theme(plot.background = element_rect(fill = "#e8d9c4", color = "#e8d9c4"))

ggsave(file = "./2022/DuBoisChallenge/challenge07_pencilFill.png", plot = fullPlot, width = 5.5, height = 6.3)
