# busco plot

library(tidyverse)

formatted_species =
    function(character){
        character %>%
            gsub("_", " ", x=.) %>%
            gsub("(powelli|hybridum)", "x \\1", x=.) %>% 
            gsub("(Narcissus) sp", "italic('\\1')~' Tête-à-Tête'", x=.) %>%
            gsub("(\\w+) sp$", "italic('\\1')~' sp.'", x=.) %>%
            gsub("(\\w+)( x | aff )(\\w+)",
                 "italic('\\1')~'\\2'~italic('\\3')", x=.) %>%
            gsub("^(\\w+ \\w+)$", "italic('\\1')", x=.) %>% 
            gsub("^(\\w+ \\w+) (PB|TH)$", "italic('\\1')~' (\\2)'", x=.)
    }

get_Origin = function(assembly) {
    case_when(assembly %in% c("Amaryllis belladonna",
                              "Narcissus viridiflorus",
                              "Phycella aff cyrtanthoides",
                              "Rhodophiala pratensis",
                              "Traubia modesta",
                              "Zephyranthes treatiae") ~
                  "1Kp",
              assembly %in% c("Zephyranthes carinata",
                              "Crinum asiaticum",
                              "Hippeastrum striatum",
                              "Scadoxus multiflorus") ~
                  "Wang et al, 2024",
              assembly == "Narcissus sp" ~ "Mehta et al 2024",
              assembly %in% c("Narcissus papyraceus", "Leucojum aestivum",
                              "Crinum powellii") ~
                  "Desgagne-Penix team",
              assembly %in% c("Narcissus aff pseudonarcissus",
                              "Galanthus sp",
                              "Galanthus elwesii") ~
                  "Kilgore et al, 2014,2016",
              .default = "Assembled de novo")
}

features = read_delim("Features_per_assembly.txt") %>% 
    mutate(Species = Species %>% gsub("_", " ", x = .),
           Species_formatted = Species %>% formatted_species) %>%
    pivot_longer(cols = -c(Species, Species_formatted),
                 values_to = "Number of features",
                 names_to = "Feature") %>%
    mutate(
        Feature = factor(Feature, levels = c("Genes", "Transcripts", "Proteins")),
        Origin = get_Origin(Species)
    ) 

color_feature = c("Genes" = "black", 
                  "Transcripts" = "grey50",
                  "Proteins" = "red")
desired_breaks = 4
breaks = list("Genes" = seq(0, 10, by = 3),
           "Transcripts" = seq(0, 15, by = 5),
           "Proteins" = seq(0, 2, by = .6)
)

plots = lapply(levels(features$Feature), \(x) {
    
    dataset = features %>% filter(Feature == x)
    max_x = max(breaks[[x]], dataset$`Number of features`/100000)
    dataset %>% 
        ggplot(aes(`Number of features` / 100000, 
                   Species_formatted %>% fct_rev, fill = Feature %>% fct_rev)) + 
        geom_col(show.legend = F) + 
        theme_classic() + 
        facet_grid(rows = vars(Origin), drop = T,
                   cols = vars(Feature),  
                   scales = "free_y", shrink = T,
                   space = "free_y", ) + 
        scale_fill_manual(values = color_feature[[x]]) + 
        labs(x = "Number of features (x 100 000)", y = "", fill = "") +
        scale_y_discrete(labels = ggplot2:::parse_safe) +
        scale_x_continuous(breaks = breaks[[x]], limits = c(0, max_x)) +
        theme(axis.text = element_text(color = "black", size = 11),
              strip.text.y = element_text(color = "black",
                                          angle = 0, 
                                          size = 11), 
              strip.text.x = element_text(size = 12, face = "bold"),
              strip.background = element_blank())
})

library(patchwork)

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
ggsave(paste0("plots/Number_of_features", Sys.Date(), ".svg"),
       width = 8, height = 8, units="in")
