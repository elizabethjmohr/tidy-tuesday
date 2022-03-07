library(tidyverse)
library(tigris)
library(sf)
library(tidytuesdayR)
library(biscale)
library(cowplot)
library(ggtext)
options(tigris_use_cache = TRUE)

# get station data
stations <- tt_load(2022, week = 9)$stations

# get continental US polygon and interstates from 
# US census Tiger database
bb <- c("xmin" = -124.7844079, 
        "ymin" = 22, 
        "xmax" = -66.9513812,
        "ymax" = 49.3457868)

continentalUS <- nation() %>%
  st_crop(y = bb) %>%
  st_transform(crs = 5070) # Albers equal area conic

roads <- primary_roads() %>%
  st_transform(crs = 5070) %>%
  st_crop(continentalUS)%>%
  filter(RTTYP == "I")

# get electric vehicle charging stations only, 
# add variables to indicate:
# 1) if station is in Tesla network,
# 2) if energy source is renewable
electricStations <- stations %>%
  filter(FUEL_TYPE_CODE == "ELEC",
         STATUS_CODE == "E") %>%
  mutate(tesla = if_else(EV_NETWORK %in% c("Tesla", "Tesla Destination"), TRUE, FALSE, FALSE),
         solar = if_else(EV_ON_SITE_RENEWABLE_SOURCE == "SOLAR", TRUE, FALSE, FALSE),
         renewable = if_else(is.na(EV_ON_SITE_RENEWABLE_SOURCE) | EV_ON_SITE_RENEWABLE_SOURCE == "NONE", FALSE, TRUE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(crs = 5070) %>%
  st_crop(continentalUS)

# Plot of all stations colored by renewable energy status
ggplot(data = continentalUS) +
  geom_sf(color = "black", fill = "black")+
  geom_sf(data = roads, color = "grey66", 
          size = 0.4, alpha = 0.4)+
  geom_sf(data = electricStations,
          size = 0.15,
          alpha = 0.6,
          color = "#0C7BDC",
          fill = "#0C7BDC")+
  geom_sf(data = electricStations %>% filter(renewable),
          size = 0.4,
          color = "#FFC20A",
          fill = "#FFC20A")+
  coord_sf(crs = st_crs(4326))+
  theme(plot.background = element_rect("transparent", colour = NA),
        legend.background = element_rect("transparent", colour = NA),
        panel.background = element_rect("transparent", colour = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
  
# make hexagon grid covering US
hexGrid <- continentalUS  %>%
  st_make_grid(n = c(100,100),
               what = 'polygons', 
               square = FALSE,
               flat_topped = TRUE) %>%
  st_as_sf() %>%
  mutate(row = as.numeric(row.names(.)))

teslaCounts <- electricStations%>%
  filter(tesla) %>%
  st_join(hexGrid,
          left = TRUE) %>%
  group_by(row) %>%
  summarize(count = n())

nonTeslaCounts <- electricStations %>%
  filter(!tesla) %>%
  st_join(hexGrid,
          left = TRUE) %>%
  group_by(row) %>%
  summarize(count = n())

hexGrid <- hexGrid %>%
  st_join(teslaCounts) %>%
  rename(teslaStations = count) %>%
  st_join(nonTeslaCounts) %>%
  rename(nonTeslaStations = count) %>%
  mutate("# of stations" = teslaStations+nonTeslaStations,
         "% Tesla" = teslaStations/(`# of stations`)) %>%
  mutate(teslaClass = if_else(`% Tesla` >=0.5, 2, 1)) %>%
  mutate(stationClass = if_else(`# of stations` >= 10, 2, 1)) %>%
  mutate(bi_class = paste(teslaClass, stationClass, sep = "-")) %>%
  filter(!(bi_class == "NA-NA"))

palette = bi_pal_manual(
  val_1_1 = "#ffcc80", #light orange
  val_1_2 = "#b30000", # dark orange
  val_2_1 = "#c3b3d8", # light purple
  val_2_2 = "#240d5e" # dark purple
)
plotTitle <- "<span style = 'font-size:20pt; font-family:Helvetica;'>**<span style='color:#c3b3d8;'>Tesla </span><span style='color:#240d5e;'>stations</span>**
              tend to dominate only in areas with **<span style='color:#c3b3d8;'>fewer (<10)</span>** stations. <br>
              Where electric vehicle charging stations are 
              **<span style='color:#b30000;'>abun</span><span style='color:#240d5e;'>dant(>10)</span>,**<br>
              **<span style='color:#b30000;'>Non-Tesla stations</span>** dominate.</span>"
map <- ggplot(continentalUS)+
  geom_sf(lwd = 0) + 
  geom_sf(data = roads, color = "black", 
          size = 0.4, alpha = 0.2)+
  geom_sf(data = hexGrid,
          aes(fill = bi_class, color = bi_class), 
          show.legend = FALSE)+
  bi_scale_fill(pal = palette, dim = 2) +
  bi_scale_color(pal = palette, dim = 2)+
  labs(title = plotTitle)+
  theme(plot.background = element_rect("transparent", colour = NA),
        legend.background = element_rect("transparent", colour = NA),
        panel.background = element_rect("transparent", colour = NA),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_markdown(lineheight = 1.2))

legend <- bi_legend(pal = palette,
                    dim = 2,
                    xlab = "Higher % Tesla",
                    ylab = "More Stations",
                    size = 8) + 
  theme(axis.title = element_blank(),
        plot.background = element_rect("transparent", colour = NA),
        panel.background = element_rect("transparent", colour = NA)) 

legendTextSize <- 4
finalPlot <- ggdraw() +
  draw_plot(map, x = 0, y = 0.05, height = 0.95, width = 1.0)+
  draw_plot(legend, 0.18, 0.1, 0.2, 0.2) +
  annotate("text", x = 0.1, y = 0.05, size = legendTextSize, label ="Less than 50% of stations \n in Tesla network")+
  annotate("text", x = 0.45, y = 0.05,  size = legendTextSize,label ="Majority of stations \n in Tesla network" )+
  annotate("text", x = 0.1, y = 0.15,  size = legendTextSize,label ="Less than \n 10 stations")+
  annotate("text", x = 0.1, y = 0.25,  size = legendTextSize,label ="More than \n 10 stations")+
  geom_curve(aes(x = 0.17, y = 0.04, xend = 0.26, yend = 0.12),
             arrow = arrow(length = unit(0.03, "npc")))  +
  geom_curve(aes(x = 0.38, y = 0.04, xend = 0.31, yend = 0.12),
             arrow = arrow(length = unit(0.03, "npc")),
             curvature = -0.5) +
  geom_curve(aes(x = 0.14, y = 0.15, xend = 0.23, yend = 0.16),
             arrow = arrow(length = unit(0.03, "npc")),
             curvature = -0.1) +
  geom_curve(aes(x = 0.14, y = 0.25, xend = 0.23, yend = 0.23),
             arrow = arrow(length = unit(0.03, "npc")), 
             curvature = 0.1) 

ggsave(filename = "./2022/2022-03-01/EVStations.png", finalPlot, width = 11, height = 8.5 )
