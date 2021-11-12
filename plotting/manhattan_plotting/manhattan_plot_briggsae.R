## MANHATTAN PLOTS ##

library("tidyverse")
library("plotly")
library("DT")
library("ggbeeswarm")
library("knitr")
library("ggrepel")
library("genetics")
library("ggnewscale")
library("cowplot")

# in nextflow use sed to edit this field and make a copy of the .rmd for each trait
trait_name <- ""

#load gene list file for labelling
genes <- read.delim("/Users/shriram/Desktop/worms/plotting/genes/brig_genes.tsv")

# load independent tests result
total_independent_tests <- read.table("/Users/shriram/Desktop/worms/results/nemascan_results/pre_fine_mapping/briggsae/mean_values_run/Analysis_Results-20211109/Genotype_Matrix/total_independent_tests.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)

independent_test_cutoff <- -log10(0.05/total_independent_tests[[1]])

# load processed mapping data. 
#processed_mapping <- read.delim("/projects/b1059/projects/Shriram/worm/results/result_date/10_12/Analysis_Results-20211012/Mapping/Processed/processed_telomere_length_AGGREGATE_mapping.tsv", stringsAsFactors=FALSE) %>%
processed_mapping <- read.delim("/Users/shriram/Desktop/worms/results/nemascan_results/pre_fine_mapping/briggsae/mean_values_run/Analysis_Results-20211109/Mapping/Processed/processed_len_AGGREGATE_mapping.tsv", stringsAsFactors=FALSE) %>%
  dplyr::mutate(CHROM = factor(CHROM, levels = c("I","II","III","IV","V","X","MtDNA"))) %>%
  dplyr::select(-marker) %>%
  tidyr::unite("marker", CHROM, POS, sep = ":", remove = F)


for.plot <- processed_mapping %>%
  dplyr::mutate(CHROM = as.factor(CHROM)) %>%
  dplyr::filter(CHROM != "MtDNA") %>%
  dplyr::mutate(algorithm = as.factor(algorithm))

BF <- processed_mapping %>% 
  dplyr::group_by(trait) %>% 
  dplyr::filter(log10p != 0) %>% 
  dplyr::mutate(BF = -log10(0.05/sum(log10p > 0, na.rm = T))) %>%
  dplyr::ungroup() %>%
  dplyr::select(BF) %>%
  unique(.) %>%
  as.numeric()


# ntests <- data.table::fread(tests) %>%
#  as.numeric()
# EIGEN <- -log10(0.05/ntests)
BF.frame <- processed_mapping %>%
  dplyr::select(trait) %>%
  dplyr::filter(!duplicated(trait)) %>%
  dplyr::mutate(BF = BF, EIGEN  = independent_test_cutoff)

for.plot.ann <- for.plot %>%
  dplyr::mutate(sig = case_when(log10p > BF.frame$BF ~ "BF",
                                log10p > BF.frame$EIGEN ~ "EIGEN",
                                TRUE ~ "NONSIG"))

sig.colors <- c("red","#EE4266")
names(sig.colors) <- c("BF","EIGEN")

man.plot <-  ggplot2::ggplot() + 
  ggplot2::theme_bw() + 
  ggplot2::geom_point(data = for.plot.ann[which(for.plot.ann$sig != "NONSIG"),], 
                      mapping = aes(x = POS/1000000, 
                                    y = log10p,
                                    colour = sig),
                                    size=0.25) +
  ggplot2::scale_colour_manual(values = sig.colors) + 
  ggplot2::geom_point(data = for.plot[which(for.plot.ann$sig == "NONSIG"),], 
                      mapping = aes(x = POS/1000000, 
                                   y = log10p), 
                                   alpha = 0.5,
                                   size=0.25) +
  ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0,BF + 6.7)) +
  scale_x_continuous(expand = c(0, 0), breaks = c(5, 10, 15, 20)) +
  ggplot2::geom_point(data = genes, 
                      mapping=aes(x = POS/1000000, 
                                  y = log10p
                                  ),
                                  shape=25,
                                  size=2,
                                  fill="blue") +
  ggplot2::geom_hline(data = BF.frame, aes(yintercept = BF), linetype = 2) + 
  ggplot2::geom_hline(data = BF.frame, aes(yintercept = EIGEN), linetype = 3) + 
  ggplot2::labs(x = "",
                #y = expression(-log[10](italic(p)))) +
                y="") +
  ggplot2::theme(legend.position = "none", 
                 panel.grid = element_blank()) + 
  ggplot2::facet_grid(. ~ CHROM, scales = "free_x", space = "free") +
  ggplot2::theme(plot.title = element_text(face = "italic"))

man.plot

ggsave("brig_plot.png",width=7.5,height=2.5,units = "in",dpi=300)


# save the plot to file. keep this code for user to create plots with their own styles
#ggplot2::ggsave(man.plot, filename = "manplot.png", width = 8, height = 4)

