# busco plot
source("scripts/FUNCTIONS.R")

read_delim("Busco_summary.txt") %>% 
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
    ) %>% 
    ggplot(aes(Pct, Species_formatted %>% fct_rev, fill = BUSCO %>% fct_rev)) + 
    geom_col() + 
    theme_classic() + 
    facet_grid(rows = vars(Origin), scales = "free_y",  
               axes = "all_y", space = "free",
               labeller = label_parsed) +
    scale_fill_viridis_d(option="plasma", direction = -1) + 
    labs(x = "BUSCO %", y = "", fill = "") +
    scale_y_discrete(labels = ggplot2:::parse_safe) +
    theme(axis.text = element_text(color = "black", size = 11),
          legend.text = element_text(size = 11),
          strip.text.y = element_text(color = "black",
                                      angle = 0, 
                                      size = 11), 
          strip.background = element_blank())

dir.create("plots", showWarnings = F)
ggsave(paste0("plots/BUSCO_", Sys.Date(), ".svg"), 
width = 8, height = 8, units="in")
