args=commandArgs(trailingOnly=T)

DIR=args[1] # input and output are in the same folder
in_suffix=args[2] # inputs differ only by the species names
out_suffix=args[3] # outputs differ only by the species names
species_list=args[4:length(args)] # all other arguments are species

library(tidyverse, quietly=T)

export_expressed_iso_list =
	function(species, what=c("head", "all")) {
		input=paste0(DIR, "/", species, in_suffix)
		output=paste0(DIR, "/", species, out_suffix)
		
		counts = read_delim(input) 
		names(counts)[1] = "isoform"
		
		counts$Total = rowSums(counts[, -1])
		
		filtered = counts %>% filter(Total > 0)

		write.table(filtered$isoform, file=output, quote=F, row.names=F, col.names=F)
		if (what == "head") {
			return(head(filtered))
		} else { return(filtered) }
}

filtered_counts = 
 sapply(species_list, simplify = F, \(species) {
	export_expressed_iso_list(species, what = "head")
})
