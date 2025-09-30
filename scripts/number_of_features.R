# Figure 2

source("scripts/FUNCTIONS.R")

features = read_delim("stats_assemblies.txt", delim="\t",
                      col_names = c("Species", "Genes", 
                                    "Transcripts", "Genes_wORFs", 
                                    "Proteins")) %>% 
    mutate(Species = Species %>% gsub("_", " ", x = .),
           Species_formatted = Species %>% formatted_species) %>%
    pivot_longer(cols = -c(Species, Species_formatted),
                 values_to = "Number of features",
                 names_to = "Feature") %>%
    mutate(
        Feature = factor(Feature, levels = c("Genes", "Transcripts", "Proteins", "Genes_wORFs")),
        Origin = get_Origin(Species)
    ) 

features$Species_formatted %>% unique %>% parse_character(x = .) %>% 
    gsub("(italic|\\(|\\)|'|~)", "", x = .) %>%
    paste(collapse = ", ")

features %>% 
    group_by(Feature) %>% 
    slice_max(`Number of features`) %>% 
    select(Species, Feature, `Number of features`)

color_feature = c("Genes" = "black", 
                  "Transcripts" = "grey50",
                  "Proteins" = "red")
desired_breaks = 4
breaks = list("Genes" = seq(0, 15, by = 5),
           "Transcripts" = seq(0, 20, by = 5),
           "Proteins" = seq(0, 2, by = .6)
)

plots = lapply(levels(features$Feature)[1:3], \(x) {
    
    dataset = features %>% filter(Feature == x)
    max_x = max(breaks[[x]], dataset$`Number of features`/100000)
    dataset %>% 
        ggplot(aes(`Number of features` / 100000, 
                   Species_formatted %>% fct_rev, fill = Feature %>% fct_rev)) + 
        geom_col(show.legend = F) + 
        theme_classic() + 
        facet_grid(rows = vars(Origin), drop = T,
                   cols = vars(Feature),  
                   labeller = label_parsed,
                   scales = "free_y", shrink = T,
                   space = "free_y", ) + 
        scale_fill_manual(values = color_feature[[x]]) + 
        labs(x = "Number of features (x 100 000)", y = "", fill = "") +
        scale_y_discrete(labels = ggplot2:::parse_safe) +
        scale_x_continuous(breaks = breaks[[x]], limits = c(0, max_x)) +
        theme(axis.text = element_text(color = "black", size = 11),
              strip.text.y = element_text(color = "black", hjust = 0,
                                          angle = 0, 
                                          size = 11), 
              strip.text.x = element_text(size = 12, face = "bold"),
              strip.background = element_blank())
})


# Number of plots
n <- length(plots)

# Customize plots:
plots_modified <- lapply(seq_along(plots), function(i) {
    p <- plots[[i]]
    
    # Remove x-axis title from all except middle plot
    if (i != ceiling(n / 2)) {
        p <- p + labs(x = NULL)
    }
    
    # Remove right strip text from all except last plot
    if (i != n) {
        p <- p + theme(strip.text.y.right = element_blank())
    }
    
    # Remove y-axis from all except the first (left-most) plot
    if (i != 1) {
        p <- p + theme(
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank()
        )
    }
    p
})

# Combine side-by-side
final_plot <- plots_modified %>%
    wrap_plots(plots_modified, nrow = 1)

# Print
final_plot

dir.create("plots", showWarnings = F)
ggsave(paste0("plots/Figure2_numberFeatures", Sys.Date(), ".svg"),
       width = 9, height = 8, units="in")
ggsave(paste0("plots/Figure2_numberFeatures", Sys.Date(), ".png"),
       width = 9, height = 8, units="in")

##### Proportion of protein coding genes ######

prop_prot_encoding <- read_delim("stats_assemblies.txt", delim="\t",
                                 col_names = c("Species", "Genes", 
                                               "Transcripts", "Genes_wORFs", 
                                               "Proteins")) %>% 
    mutate(Species = Species %>% gsub("_", " ", x = .)) %>%
    group_by(Species) %>% 
    summarize(Proportion = Genes_wORFs/Genes) %>% 
    mutate(Origin = get_Origin(Species),
           Species_formatted = Species %>% formatted_species,
           Percentages = Proportion * 100)

prop_prot_encoding %>% 
    ggplot(aes(Percentages, 
               Species_formatted %>% fct_rev)) + 
    geom_col(show.legend = F, fill="black") + 
    theme_classic() + 
    facet_grid(rows = vars(Origin), drop = T,  
               labeller = label_parsed,
               scales = "free_y", shrink = T,
               space = "free_y") + 
    labs(x = "Protein-encoding genes in transcriptome (%)", y = "", fill = "") +
    scale_y_discrete(labels = ggplot2:::parse_safe) +
    theme(axis.text = element_text(color = "black", size = 11),
          strip.text.y = element_text(color = "black",hjust = 0,
                                      angle = 0, 
                                      size = 11),
          strip.background = element_blank())

ggsave("plots/FigureS2_proportionProtEncodingGenes.png",
       width = 8, height = 8, units="in")
ggsave("plots/FigureS2_proportionProtEncodingGenes.svg",
       width = 8, height = 8, units="in")
prop_prot_encoding %>% 
    group_by(Origin) %>% 
    summarize(Mean_pct = mean(Percentages))

prop_prot_encoding %>% 
    filter(Species != "Lycoris aurea PB") %>% 
    group_by(Origin) %>% 
    summarize(Mean_pct = mean(Percentages))

prop_prot_encoding %>% slice_max(order_by = Percentages)
prop_prot_encoding %>% slice_min(order_by = Percentages)
