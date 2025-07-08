source("scripts/FUNCTIONS.R")

mapping.summary = 
    read_delim("kallisto_stats.txt") %>%
    mutate(Species = gsub("_", " ", Species)) %>%
    group_by(Species) %>%
    summarize(Mean_Total = mean(p_pseudoaligned),
              SD_Total = sd(p_pseudoaligned),
              Mean_Unique = mean(p_unique),
              SD_Unique = sd(p_unique))

SD_mapping_summary = 
    mapping.summary %>%
    select(-starts_with("Mean")) %>%
    pivot_longer(cols = -Species,
                 values_to = "Standard_deviation",
                 names_to = "Type",
                 names_prefix = "SD_"
    )
mean_mapping_summary =
    mapping.summary %>%
    select(-starts_with("SD_")) %>%
    pivot_longer(cols = -Species,
                 values_to = "Mean",
                 names_to = "Type",
                 names_prefix = "Mean_"
    )

full_summary = left_join(
    SD_mapping_summary,
    mean_mapping_summary,
    by = c("Species", "Type")
) %>%
    replace_na(replace=list(Standard_deviation = 0)) %>%
    mutate(Species_formatted = formatted_species(Species),
           Origin = get_Origin(Species))


for (i in c("Total", "Unique")) {
    plot_name = paste0("plots/Mapping_", i, c(".svg", ".png"))
    
    full_summary %>%
    filter(Type == i) %>%
    ggplot(aes(y = Species_formatted %>% fct_rev)) +
    geom_col(aes(x = Mean),
             position = position_identity(),
             fill = "black") +
    geom_errorbar(aes(xmin = Mean - Standard_deviation,
                      xmax = Mean + Standard_deviation),
                  color = "grey50", linewidth = 1) +
    theme_classic() + 
    facet_grid(rows = vars(Origin), scales = "free_y",
               axes = "all_y", space = "free",
               labeller = label_parsed) +
    labs(x = "Reads (%)", y = "", fill = "") +
    xlim(c(0, 100)) +
    scale_y_discrete(labels = ggplot2:::parse_safe) +
    theme(axis.text = element_text(color = "black", size = 11),
          legend.text = element_text(size = 11),
          strip.text.y = element_text(color = "black",
                                      angle = 0, 
                                      size = 11), 
          legend.position = "bottom",
          strip.background = element_blank())
    
    
    for (fileName in plot_name) {
        ggsave(fileName, width = 8, height = 8, units="in", dpi = 600)
    }
}
