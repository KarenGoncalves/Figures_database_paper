# busco plot
source("scripts/FUNCTIONS.R")

busco_data = read_delim("Busco_summary.txt") %>% 
    mutate(Species = gsub(".fasta", "", Input_file) %>% 
               gsub("_", " ",  .),
           Species_formatted = Species %>% formatted_species) %>% 
    select(Species, Species_formatted, Single, Duplicated, Fragmented, Missing) %>% 
    pivot_longer(cols = -c(Species, Species_formatted),
                 names_to = "BUSCO", values_to = "Pct") %>% 
    mutate(BUSCO = factor(BUSCO, 
                          levels = c("Single", "Duplicated", 
                                     "Fragmented", "Missing")),
           Origin = get_Origin(Species)
    ) 

busco_data %>%
    filter(BUSCO %in% c("Single", "Duplicated")) %>%
    group_by(Species) %>%
    summarise(Complete = sum(Pct)) %>% 
    arrange(desc(Complete))

busco_data %>%
    filter(BUSCO == "Missing") %>%
    arrange(desc(Pct))

busco_data %>% 
    ggplot(aes(Pct, Species_formatted %>% fct_rev, fill = BUSCO %>% fct_rev)) + 
    geom_col() + 
    theme_classic() + 
    facet_grid(rows = vars(Origin), scales = "free_y",  
               axes = "all_y", space = "free",
               labeller = label_parsed) +
    scale_fill_viridis_d(option="plasma", direction = -1,
                         breaks = c("Single", "Duplicated", "Fragmented", "Missing")) + 
    labs(x = "BUSCO %", y = "", fill = "") +
    scale_y_discrete(labels = ggplot2:::parse_safe) +
    theme(axis.text = element_text(color = "black", size = 11),
          legend.text = element_text(size = 11),
          legend.position = "bottom",
          strip.text.y = element_text(color = "black",hjust=0,
                                      angle = 0, 
                                      size = 11), 
          strip.background = element_blank())

dir.create("plots", showWarnings = F)
ggsave(paste0("plots/Figure3_BUSCO_", Sys.Date(), ".svg"), 
       width = 8, height = 8.5, units="in")
ggsave(paste0("plots/Figure3_BUSCO_", Sys.Date(), ".png"), 
       width = 8, height = 8.5, units="in")
