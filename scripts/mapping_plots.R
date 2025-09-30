source("scripts/FUNCTIONS.R")

mapping_stats = 
    read_delim("kallisto_stats.txt") %>%
    mutate(Species = gsub("_", " ", Species)) 
    
    
#### Summarized data and plots ####

mapping.summary = 
    mapping_stats %>%
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

plot_names=c("Total" = "plots/Figure4_mappingTotal",
             "Unique" = "plots/Supplementary_MappingUnique")

for (i in c("Total", "Unique")) {
    plot_name = paste0(plot_names[i], c(".svg", ".png"))
    
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
        theme(axis.text = element_text(color = "black", size=11),
              strip.text.y = element_text(hjust = 0, angle = 360, size=11),
              strip.background = element_blank(),
              legend.text = element_text(size = 11),
              legend.position = "bottom"
        )
    
    for (fileName in plot_name) {
        ggsave(fileName, width = 8, height = 8, units="in", dpi = 600)
    }
}

#### Plots by species ####

mapping_stats_formatted = 
    mapping_stats %>%
    mutate(Species_formatted = formatted_species(Species),
           Origin = get_Origin(Species)) %>%
    rename("Total_pseudoaligned_pct" = p_pseudoaligned,
           "Unique_pseudoaligned_pct" = p_unique)

plots_by_species = unique(mapping_stats_formatted$Species) %>%
    sapply(simplify = F, \(speciesName) {
        
    title = formatted_species(speciesName)
    
    mapping_stats_formatted %>%
            filter(Species == speciesName) %>%
            select(Run, Species_formatted, Origin,
                   ends_with("pct")) %>%
            pivot_longer(cols = ends_with("pct"),
                         names_to = "Type",
                         values_to = "Pct") %>%
            mutate(Type = gsub("_pseudoaligned_pct", "", Type)) %>%
            ggplot(aes(y = Run, x = Pct, fill = Type)) +
            geom_col(position = position_identity()) +
            theme_classic() + 
            labs(x = "Reads (%)", y = "", fill = "", 
                 title = ggplot2:::parse_safe(title)) +
            xlim(c(0, 100)) +
            scale_y_discrete(labels = ggplot2:::parse_safe) +
            scale_fill_manual(values = c("black", "red"),
                              labels = c("Total", "Unique")) +
            theme(axis.text = element_text(color = "black", size = 11),
                  legend.text = element_text(size = 11),
                  strip.text.y = element_text(color = "black",
                                              angle = 0, 
                                              size = 11), 
                  legend.position = "bottom",
                  strip.background = element_blank())

    })


##### Mean mapping by number of samples #####

table(mapping_stats$Species) %>% 
    as.data.frame %>% 
    rename(Species = Var1, Samples = Freq) %>% 
    left_join(full_summary, by = "Species") %>%
    ggplot(aes(x = Samples, y = Mean, color = Type)) +
    geom_point() +
    theme_classic()
    
