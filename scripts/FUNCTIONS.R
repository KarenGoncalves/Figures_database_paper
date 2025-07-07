library(tidyverse)
library(patchwork)

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
                  "'1Kp'",
              assembly %in% c("Zephyranthes carinata",
                              "Crinum asiaticum",
                              "Hippeastrum striatum",
                              "Scadoxus multiflorus") ~
                  paste0("'Wang '", "~italic('et al.')~", "' 2024'"),
              assembly == "Narcissus sp" ~ 
                  paste0("'Mehta '", "~italic('et al.')~", "' 2024'"),
              assembly %in% c("Narcissus papyraceus", "Leucojum aestivum",
                              "Crinum powellii") ~
                  "'Desgagne-Penix team'",
              assembly %in% c("Narcissus aff pseudonarcissus",
                              "Galanthus sp",
                              "Galanthus elwesii") ~
                 paste0("'Kilgore '", "~italic('et al.')~", "', 2014, 2016'"),
              .default = paste0("'Assembled '", "~italic('de novo')")
    )
}
