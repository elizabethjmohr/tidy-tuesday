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

myWaffleIron <- function(nameSummary){
  tibble(x = 1:10, y = 1:10) %>% 
    expand(x,y) %>%
    mutate(is_even = x %%2 == 0,
           sort_col = if_else(is_even, 11-y, as.numeric(y))) %>%
    arrange(x,sort_col) %>%
    mutate(generation = map2(nameSummary$generation, nameSummary$n, ~rep(.x, times = .y)) %>% 
             unlist() %>%
             factor(levels = c("Traditionalists", "Baby Boomers", "Generation X", "Millenials", "Gen Z"))%>%
             sort()) %>%
    select(x,y, generation) 
}

makeWaffle <- function(NAME, nameSummary){
  myWaffleIron(nameSummary %>% filter(name == NAME)) %>% 
    ggplot(aes(x = x, y = y, fill = generation)) +
    geom_waffle(size = 0.3) + 
    coord_equal()+
    scale_fill_manual(values = c('#BBCC33', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB'), drop = FALSE)+
    #scale_fill_manual(values = sequential_hcl(7, "Mako")[2:6], drop = FALSE)+
    scale_x_continuous(limits = c(0.5,10.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.5,10.5), expand = c(0, 0))+
    ggtitle(NAME)+
    theme(panel.background = element_blank(),
          legend.position = "none",
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.margin = margin(1,1,1,1),
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5,vjust = 0, family = "sansPro", face = "bold", size = 10))
}

makeWaffleLegend <- function(NAME, nameSummary){
  my_ggp <- myWaffleIron(nameSummary %>% filter(name == NAME)) %>% 
    ggplot(aes(x = x, y = y, fill = generation)) +
    geom_waffle(size = 0.3) + 
    coord_equal()+
    scale_fill_manual(values = c('#BBCC33', '#77AADD', '#EE8866', '#EEDD88', '#FFAABB'), drop = FALSE)+
    #scale_fill_manual(values = sequential_hcl(7, "Mako")[2:6], drop = FALSE)+
    scale_x_continuous(limits = c(0.5,10.5), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0.5,10.5), expand = c(0, 0))+
    ggtitle(NAME)+
    theme(panel.background = element_blank(),
          legend.position = "bottom",
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.margin = margin(1,1,1,1),
          axis.text = element_blank(),
          legend.text = element_text(family = "sansPro", size = 10),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5,vjust = 0, family = "sansPro", size = 10))
  
  step1 <- ggplot_gtable(ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
