source("scripts/FUNCTIONS.R")

trinity_assembled_1kp = read_delim("BUSCO_1kp_summary.txt") %>%
    mutate(Input_file = gsub("cyrthantoides", "cyrtanthoides", Input_file),
           Assembly = "Trinity")
original_1kp_assembly = read_delim("Busco_summary.txt") %>%
    filter(Input_file %in% trinity_assembled_1kp$Input_file) %>%
    mutate(Assembly = "SOAP (1kP)")

combined_data = 
    rbind(trinity_assembled_1kp, 
          original_1kp_assembly) %>% as_tibble() %>%
    mutate(Species = gsub(".fasta", "", Input_file) %>% 
               gsub("_", " ",  .),
           Species_formatted = Species %>% formatted_species) %>% 
    select(Assembly, Species, Species_formatted, 
           Single, Duplicated, Fragmented, Missing) %>% 
    pivot_longer(cols = -c(Species, Species_formatted, Assembly),
                 names_to = "BUSCO", values_to = "Pct") %>% 
    mutate(BUSCO = factor(BUSCO, 
                          levels = c("Single", "Duplicated", 
                                     "Fragmented", "Missing"))
    )   

combined_data %>%
    ggplot(aes(Pct, Assembly, 
               fill = BUSCO %>% fct_rev)) + 
    geom_col() + 
    theme_classic() + 
    facet_grid(rows = vars(Species_formatted) ,
               labeller = label_parsed,
               axes = "all_y") + 
    scale_fill_viridis_d(option="plasma", direction = -1,
                         breaks = c("Single", "Duplicated", 
                                    "Fragmented", "Missing")) + 
    labs(x = "BUSCO %", y = "", fill = "") +
    # scale_y_discrete(
    theme(axis.text = element_text(color = "black", size = 11),
          legend.text = element_text(size = 11),
          strip.text.y = element_text(color = "black",hjust=0,
                                      angle = 0, 
                                      size = 11), 
          strip.background = element_blank())

ggsave("plots/Supplementary_compare_1KP_busco.svg",
       width=7.68, height = 2.93)
ggsave("plots/Supplementary_compare_1KP_busco.png",
       width=7.68, height = 2.93)


combined_data %>%
    filter(BUSCO == "Missing") %>%
    ggplot(aes(Pct, Assembly, 
               fill = BUSCO %>% fct_rev)) + 
    geom_col(show.legend = F) + 
    theme_classic() + 
    facet_grid(rows = vars(Species_formatted) ,
               labeller = label_parsed,
               axes = "all_y") + 
    labs(x = "Missing BUSCO %", y = "", fill = "") +
    scale_fill_manual(values = "black") + 
    theme(axis.text = element_text(color = "black", size = 11),
          legend.text = element_text(size = 11),
          strip.text.y = element_text(color = "black", hjust = 0,
                                      angle = 0, 
                                      size = 11), 
          strip.background = element_blank())
ggsave("plots/Supplementary_compare_1KP_busco_missing.svg",
       width=8, height = 4.6)
ggsave("plots/Supplementary_compare_1KP_busco_missing.png",
       width=8, height = 4.6)
