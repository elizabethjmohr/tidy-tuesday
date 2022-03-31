# Define theme and color schemes
myWaffleTheme <-  theme(panel.background = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title = element_blank(),
                        plot.margin = margin(1,1,1,1),
                        axis.text = element_blank(),
                        plot.title = element_text(hjust = 0.5,vjust = 0, family = "sansPro", size = 12))

generationColorScheme <-  scale_fill_manual(values = c('#BBCC33', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB'), drop = FALSE)
binaryColorScheme <- scale_fill_manual(values = c("#964F4CFF", "#D3D3D3"), drop = FALSE)

# Force rounded percents to sum to 100
sumTo100 <- function(tbl){
  if(sum(round(tbl$percent)) == 99){
    generationToRoundUp <- (tbl %>% 
                              mutate(modulus = percent %% 1) %>% 
                              filter(modulus <0.5) %>% 
                              arrange(desc(modulus)) %>% 
                              pull(generation))[1] 
    tbl <- tbl %>%
      mutate(n = if_else(generation == generationToRoundUp, ceiling(percent), round(percent)))
  } else if(sum(round(tbl$percent)) == 101){
    generationToRoundDown <- (tbl %>% 
                                mutate(modulus = percent %% 1) %>% 
                                filter(modulus >=0.5) %>% 
                                arrange(modulus) %>% 
                                pull(generation))[1] 
    tbl <- tbl %>%
      mutate(n = if_else(generation == generationToRoundDown, floor(percent), round(percent)))
  } else {
    tbl <- tbl %>%
      mutate(n = round(percent))
  }
  return(tbl)
}

# Format data for waffle plot showing generational breakdown for a given name
myWaffleIron <- function(nameSummary){
  tibble(x = 1:10, y = 1:10) %>% 
    expand(x,y) %>%
    mutate(is_even = x %%2 == 0,
           sort_col = if_else(is_even, 11-y, as.numeric(y))) %>%
    arrange(x,sort_col) %>%
    mutate(generation = map2(nameSummary$generation, nameSummary$n, ~rep(.x, times = .y)) %>% 
             unlist() %>%
             factor(levels = c("Silent", "Baby Boomers", "Generation X", "Millenials", "Generation Z"))%>%
             sort()) %>%
    select(x,y, generation) 
}

# Format data for waffle plot showing breakdown of how many people in a 
# generation have a name in the top fifty names of that generation
myOtherWaffleIron <- function(nameSummary){
  tibble(x = 1:10, y = 1:10) %>% 
    expand(x,y) %>%
    arrange(x,y) %>%
    mutate(top = map2(nameSummary$top, nameSummary$n, ~rep(.x, times = .y)) %>% 
             unlist() %>%
             factor(levels = c("ntop", "nNottop"), labels = c("Yes", "No"))%>%
             sort()) %>%
    select(x,y, top) 
}

# Make waffle plot showing generational breakdown for a given name
makeWaffle <- function(NAME, nameSummary){
  myWaffleIron(nameSummary%>% filter(name == NAME)) %>% 
    ggplot(aes(x = x, y = y, fill = generation)) +
    geom_waffle(size = 0.3) + 
    coord_equal()+
    generationColorScheme +
    scale_x_continuous(limits = c(0.5,10.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.5,10.5), expand = c(0, 0))+
    ggtitle(NAME)+
    myWaffleTheme+
    theme(legend.position = "none")
}

# Make waffle showing breakdown of how many people in a generation
# have a name in the top fifty names of that generation
makeAnotherWaffle <- function(gen, nameSummary){
  myOtherWaffleIron(nameSummary %>% filter(generation == gen)) %>% 
    ggplot(aes(x = y, y = x, fill = top)) +
    geom_waffle(size = 0.3) + 
    coord_equal()+
    binaryColorScheme+
    scale_x_continuous(limits = c(0.5,10.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.5,10.5), expand = c(0, 0))+
    ggtitle(gen)+
    myWaffleTheme+
    theme(legend.position = "none")
}

makeWaffleLegend <- function(NAME, nameSummary){
  my_ggp <- makeWaffle(NAME, nameSummary) +
    theme(legend.position = "bottom",
          legend.title = element_text(family = "sansPro", size = 10),
          legend.text = element_text(family = "sansPro", size = 10))
  
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}

makeAnotherWaffleLegend <- function(gen, nameSummary){
  my_ggp <- makeAnotherWaffle(gen, nameSummary)+
    theme(legend.position = "bottom",
          legend.title = element_text(family = "sansPro",  size = 12),
          legend.text = element_text(family = "sansPro", size = 12))+
    guides(fill=guide_legend(title="Name in top fifty?"))
  
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
