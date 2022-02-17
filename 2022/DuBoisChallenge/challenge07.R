library(tidyverse)
library(showtext)
library(patchwork)

# Read in data
data <- read.csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge07/data.csv")

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
                   plot.margin = margin(-0.1,-0.1,0,0))

maleData <- data %>%
  filter(Gender == "Male") %>%
  pivot_longer(cols = Widowed:Single,
               names_to = "congigalCondition",
               values_to = "percent") %>%
  mutate(congigalCondition = factor(congigalCondition, levels = c("Widowed", "Married", "Single"))) %>%
  mutate(Group = if_else(Group == "Over 65", "AGES\n OVER 65", Group)) 

malePlot <- ggplot(data = maleData, 
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
  annotate("text", x = 35, y = 2, label= "SINGLE", angle = 45, size = 2.5, fontface = "bold", family = "public")+
  annotate("text", x = 55, y = 5.4, label= "MARRIED", angle = 45, size = 2.5, fontface = "bold", family = "public")+
  annotate("text", x = 92, y = 8.5, label= "WIDOWED", angle = 70, size = 2.5, fontface = "bold", family = "public")+
  duBoisTheme

femaleData <- data %>%
  filter(Gender == "Female") %>%
  pivot_longer(cols = Widowed:Single,
               names_to = "congigalCondition",
               values_to = "percent") %>%
  mutate(congigalCondition = factor(congigalCondition, levels = c("Widowed", "Married", "Single"))) %>%
  mutate(Group = if_else(Group == "Over 65", "AGES\n OVER 65", Group)) 

femalePlot <- ggplot(data = femaleData,
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
  annotate("text", x = 35, y = 2, label= "SINGLE", angle = 315, size = 2.5, fontface = "bold", family = "public")+
  annotate("text", x = 55, y = 5.4, label= "MARRIED", angle = 315, size = 2.5, fontface = "bold", family = "public")+
  annotate("text", x = 92, y = 7.2, label= "WIDOWED", angle = 290, size = 2.5, fontface = "bold", family = "public")+
  duBoisTheme

title1 <- ggplot() + 
  geom_text(aes(x = 1, y = 1, 
                label = "Conjugal condition of American Negroes according to age periods."),
            size = 2.7,
            family = "scope")+
  textTheme+
  ylim(0.995,1.005)

title2 <- ggplot() + 
  geom_text(aes(x = 1, y = 1, 
                label = "Condition conjugale des Nègres Americaires au point du vue de l'age."),
            size = 2,
            family = "scope")+
  textTheme+
  ylim(0.995,1.005)


title3 <- ggplot() + 
  geom_text(aes(x = 1, y = 1, 
                label = "Done by Atlanta University."),
            size = 1.5,
            family = "scope")+
  textTheme+
  ylim(0.995,1.005)

xAxis <- ggplot() + 
  geom_text(aes(x = 1, y = 1, 
                label = "PER CENTS."),
            size = 2,
            fontface = "bold", 
            family = "public")+
  textTheme+
  ylim(0.99,1.01)

layout <- "
AA
BB
CC
##
DE
FF
"
fullPlot <- wrap_plots(A = title1, B = title2, C = title3, D = malePlot, E = femalePlot, F = xAxis,
           design = layout, heights = c(2, 2, 2, 1, 45, 1.5)) & 
  theme(plot.background = element_rect(fill = "#e8d9c4", color = "#e8d9c4"))

ggsave(file = "./2022/DuBoisChallenge/challenge07.png", plot = fullPlot, width = 5.5, height = 6)
  