#!/bin/sh

function get_stats_assembly () {

	# After cd-EST how many trinity genes, how many trinity transcripts
	Filtered_nGenes=$(cut -f1 $DIR/gtm/${SPECIES}.tab | uniq | wc -l) 
	Filtered_nTranscripts=$(cut -f2 $DIR/gtm/${SPECIES}.tab | wc -l) 

	# How many genes with ORFS
	grep  ">" $DIR/proteome/${SPECIES}.fasta | \
	cut -f 1 -d " " | \
	sed -E 's/^>(.+)(.p[0-9]+)/\1\t\1\t\1\2/' | \
	sed -E 's/(_i|_seq)*([0-9]+)*\t/\t/' \
	> list_orf

	ORF_nGenes=$(cut -f1 list_orf | uniq | wc -l) 
	ORF_nProtein=$(cut -f2 list_orf | wc -l)

	echo "${SPECIES} $Filtered_nGenes $Filtered_nTranscripts $ORF_nGenes $ORF_nProtein" |\
	 tr ' ' '\t' 
}
	
function get_emapper_stats () {

	grep -v "#" $DIR/emapper/${SPECIES}.out | \
	awk -F ' ' '{print $1}'| \
	sed -E 's/^(.+)(.p[0-9]+)/\1\t\1\t\1\2/' | \
	sed -E 's/(_i|_seq)*([0-9]+)*\t/\t/'  \
	> eggNog.list

	nGenes=$(cut -f1 eggNog.list | uniq | wc -l) 
	nTranscripts=$(cut -f2 eggNog.list | uniq| wc -l)
	nProtein=$(cut -f3 eggNog.list | wc -l)

	echo "${SPECIES} EggNOG	$nGenes $nTranscripts $nProtein" |\
	 tr ' ' '\t' 
}

function get_pfam_stats () {

	grep -v "#" $DIR/hmm/${SPECIES}.out | \
	awk -F ' ' '{print $4}'| \
	sed -E 's/^(.+)(.p[0-9]+)/\1\t\1\t\1\2/' | \
	sed -E 's/(_i|_seq)*([0-9]+)*\t/\t/' \
	> Pfam.list

        nGenes=$(cut -f1 Pfam.list | uniq | wc -l)
        nTranscripts=$(cut -f2 Pfam.list | uniq| wc -l)
        nProtein=$(cut -f3 Pfam.list | wc -l)

        echo "${SPECIES} Pfam $nGenes $nTranscripts $nProtein" |\
         tr ' ' '\t'
}

function get_uniprot_stats () {

        grep -v "#" $DIR/blastp_uniprot/${SPECIES}.out | \
        awk -F ' ' '{print $1}'| \
	sed -E 's/^(.+)(.p[0-9]+)/\1\t\1\t\1\2/' | \
	sed -E 's/(_i|_seq)*([0-9]+)*\t/\t/'  \
	> Uniprot.list

        nGenes=$(cut -f1 Uniprot.list | uniq | wc -l)
        nTranscripts=$(cut -f2 Uniprot.list | uniq| wc -l)
        nProtein=$(cut -f3 Pfam.list | wc -l)
	
        echo "${SPECIES} Uniprot $nGenes $nTranscripts $nProtein" |\
         tr ' ' '\t'
}

function get_infernal_stats () {

        grep -v "#" $DIR/infernal/${SPECIES}.out | \
        sed -E 's/ +/\t/g' | \
	awk -F ' ' '{print $4}'| \
        sed -E 's/^(.+)/\1\t\1/' | \
	sed -E 's/(_i|_seq)*([0-9]+)*\t/\t/'  \
	> Infernal.list

        nGenes=$(cut -f1 Infernal.list | uniq | wc -l)
        nTranscripts=$(cut -f2 Infernal.list | uniq| wc -l)
	
        echo "${SPECIES} Infernal $nGenes $nTranscripts NA" |\
         tr ' ' '\t'


}

function get_general_annotation_stats () {
	cat Uniprot.list Pfam.list eggNog.list | sort | uniq > general.list
	nGenes=$(cut -f1 general.list | uniq | wc -l)
	nTranscripts=$(cut -f2 general.list | uniq| wc -l)
        nProtein=$(cut -f3 general.list | wc -l)

	echo "${SPECIES} Any_protein_DB $nGenes $nTranscripts $nProtein" |\
         tr ' ' '\t'
}

function detect_motifs_stats () {
        
	# - TmHMM
	grep -v "#" $DIR/tmhmm/${SPECIES}.out | \
	awk -F ' ' '{print $1}'| \
        sed -E 's/^(.+)(.p[0-9]+)/\1\t\1/' | \
	sed -E 's/(_i|_seq)*([0-9]+)*\t/\t/' \
	> Transmembrane_domain.list

	TmHmm_nGenes=$(cut -f1 Transmembrane_domain.list | uniq | wc -l) 
	TmHmm_nProtein=$(cut -f2 Transmembrane_domain.list | wc -l)

	# - signalp 

	grep -v "#" $DIR/signalp/${SPECIES}_output.gff3 | \
	awk -F ' ' '{print $1}'| \
        sed -E 's/^(.+)(.p[0-9]+)/\1\t\1/' | \
	sed -E 's/(_i|_seq)*([0-9]+)*\t/\t/'  \
	> Signal_peptide.list

	signalp_nGenes=$(cut -f1 Signal_peptide.list | uniq | wc -l) 
	signalp_nProtein=$(cut -f3 Signal_peptide.list | wc -l)

	echo "${SPECIES} Transmembrane_domain $TmHmm_nGenes $TmHmm_nProtein"
	echo "${SPECIES} Signal_peptide $signalp_nGenes $signalp_nProtein"

}

function rm_tmp_files () {
	rm -v list_orf eggNog.list Pfam.list Uniprot.list Infernal.list general.list Transmembrane_domain.list Signal_peptide.list
}
