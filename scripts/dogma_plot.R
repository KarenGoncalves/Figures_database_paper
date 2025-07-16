library(tidyverse)
source("scripts/FUNCTIONS.R")

DOGMA_summary =
    read_delim("summary_dogma.txt",
               col_names = c("Species", "Score")) %>%
    mutate(
        Species = gsub("_", " ", Species),
        Formatted_species = formatted_species(Species),
        Origin = get_Origin(Species))

DOGMA_summary %>%
    ggplot(aes(y = Formatted_species %>% fct_rev, 
               x = Score)) +
    geom_col(fill = "black") + 
    geom_vline(xintercept = 75, color = "red") +
    theme_classic() + 
    facet_grid(rows = vars(Origin), scales = "free_y",  
               axes = "all_y", space = "free",
               labeller = label_parsed) +
    labs(x = "DOGMA completeness %", y = "", fill = "") +
    scale_x_continuous(limits = c(0,100)) +
    scale_y_discrete(labels = ggplot2:::parse_safe) +
    theme(axis.text = element_text(color = "black", size = 11),
          legend.text = element_text(size = 11),
          strip.text.y = element_text(color = "black",
                                      angle = 0, 
                                      size = 11), 
          strip.background = element_blank())

dir.create("plots", showWarnings = F)
ggsave(paste0("plots/DOGMA_", Sys.Date(), ".svg"), 
       width = 8, height = 8, units="in")
ggsave(paste0("plots/DOGMA_", Sys.Date(), ".png"), 
       width = 8, height = 8, units="in")