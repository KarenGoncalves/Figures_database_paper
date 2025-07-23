#!/bin/sh
source $SCRATCH/plots_database_paper/scripts/functions_stats.sh
DIR=/nearline/def-desgagne/amaryllidaceae_annotations
SPECIES=$1
outDIR=$2

get_stats_assembly 		>> $outDIR/stats_assemblies.txt

get_emapper_stats  		>> $outDIR/stats_annotation.txt
get_pfam_stats	 		>> $outDIR/stats_annotation.txt
get_uniprot_stats		>> $outDIR/stats_annotation.txt
get_infernal_stats		>> $outDIR/stats_annotation.txt
get_general_annotation_stats	>> $outDIR/stats_annotation.txt

detect_motifs_stats	 	>> $outDIR/stats_motifs.txt


rm_tmp_files
