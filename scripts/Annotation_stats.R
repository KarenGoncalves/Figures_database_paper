source("scripts/FUNCTIONS.R")

theme_set(theme_classic() +
              theme(
                  axis.text = element_text(color = "black"),
                  strip.text = element_text(color = "black"),
                  title = element_text(color = "black"),
                  axis.title = element_text(color = "black")
              )
)
stats_annotation = 
    read_delim("stats_annotation.txt",
               col_names = c("Species", "Database", 
                             "Genes", "Transcripts", "Proteins")) %>%
    mutate(Species = gsub("_", " ", Species), 
           Species_formatted = formatted_species(Species),
           Origin = get_Origin(Species)) %>%
    pivot_longer(cols = c(Genes, Transcripts, Proteins),
                 names_to = "Feature",
                 values_to = "Total") %>% 
    mutate(Database = gsub("Any_protein_DB", 
                           "'EggNOG, Pfam or UniProt'",
                           Database))

stats_assembly = 
    read_delim("stats_assemblies.txt",
               col_names = c("Species", "Genes", 
                             "Transcripts", "Genes_wORFs", 
                             "Proteins")) %>%
    mutate(Species = gsub("_", " ", Species))
stats_motifs = 
    read_delim("stats_motifs.txt",
               col_names = c("Species", "Database", 
                             "Genes", "Proteins")) %>%
    mutate(Species = gsub("_", " ", Species))

stats_annotation %>%
    filter(Database %in% c("Infernal", "'EggNOG, Pfam or UniProt'"),
           Feature == "Genes") %>% 
    ggplot(aes(x = Total, y = Species_formatted %>% fct_rev())) +
    geom_col() +
    labs(x = "", y = "") +
    scale_y_discrete(labels = ggplot2:::parse_safe) +
    facet_grid(cols = vars(Database), scales = "free",
               rows = vars(Origin), space = "free_y",
               labeller = label_parsed) +
    theme(strip.text.y = element_text(angle = 360),
          strip.background = element_blank())


proportion_annotation =
    read_delim("stats_annotation.txt") %>%
    mutate(Species = gsub("_", " ", Species), 
           Species_formatted = formatted_species(Species),
           Origin = get_Origin(Species)) %>%
    left_join(stats_assembly, 
              by = c("Species")) %>%
    mutate(PCT_Genes = Genes.x*100 / Genes.y,
           PCT_Transcripts = Transcripts.x*100 / Transcripts.y,
           PCT_ProteinCodingGenes = Genes.x*100 / Genes_wORFs,
           PCT_Proteins = Proteins.x*100 / Proteins.y,
           Database = gsub("Any_protein_DB", 
                           "'EggNOG, Pfam or UniProt'",
                           Database)) %>%
    pivot_longer(cols = starts_with("PCT"),
                 names_to = "Feature",
                 values_to = "Percentage", 
                 names_prefix = "PCT_") %>% 
    filter(Database %in% c("Infernal", "'EggNOG, Pfam or UniProt'")) 

proportion_annotation %>% 
    filter(Feature == "Genes") %>% 
    ggplot(aes(x = Percentage, y = Species_formatted %>% fct_rev())) +
    geom_col() +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_discrete(labels = ggplot2:::parse_safe) +
    facet_grid(cols = vars(Database), scales = "free",
               rows = vars(Origin), space = "free_y",
               labeller = label_parsed) +
    theme(strip.text.y = element_text(angle = 360),
          strip.background = element_blank())
ggsave("plots/Genes_annotated_protein_DBs.svg",
       width = 6.9, height = 7.54)

ggsave("plots/Genes_annotated_protein_DBs.png",
       width = 6.9, height = 7.54)

proportion_annotation %>% 
    filter(Feature == "ProteinCodingGenes",
           Database == "'EggNOG, Pfam or UniProt'") %>% 
    ggplot(aes(x = Percentage, y = Species_formatted %>% fct_rev())) +
    geom_col() +
    labs(x = "", y = "") +
    theme_classic() +
    scale_y_discrete(labels = ggplot2:::parse_safe) +
    facet_grid(cols = vars(Database), scales = "free",
               rows = vars(Origin), space = "free_y", 
               labeller = label_parsed) +
    theme(strip.text.y = element_text(angle = 360),
          strip.background = element_blank())
ggsave("plots/ProteinCodingGenes_annotated_protein_DBs.png",
       width = 6.9, height = 7.54)


proportion_annotation %>% 
    select(Genes.y, Feature, Species_formatted, 
           Database, Percentage) %>% 
    filter(Database == "'EggNOG, Pfam or UniProt'",
           Feature %in% c("Genes", "ProteinCodingGenes")) %>% 
    mutate(Feature = gsub("ProteinCodingGenes",
                          "Protein coding genes",
                          Feature)) %>% 
    ggplot(aes(x = Genes.y/10000))+
    geom_point(aes(y = Percentage, color = Feature)) +
    geom_smooth(aes(y = Percentage, fill = Feature, color = Feature)) +
    scale_x_continuous() +
    scale_color_manual(values = c(
        "Genes" = "black",
        "Protein coding genes" = "red"
    ), name = "") +
    scale_fill_manual(values = c(
        "Genes" = "black",
        "Protein coding genes" = "red"
    ), name = "") +
    labs(x = "Genes in transcriptome (*10 000)",
         y = "Genes annotated with protein databases (%)") +
    theme_classic() +
    theme(legend.position = "bottom")
ggsave("plots/TranscriptomeSize_AnnotationPCT.svg",
       width = 6.12, height = 4.14)

ggsave("plots/TranscriptomeSize_AnnotationPCT..png",
       width = 6.12, height = 4.14)
