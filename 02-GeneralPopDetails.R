# Introduction ------------------------------------------------------------
#
# This script was originally written by A. Pannullo in the summer of 2017
# It has been modified by Z.N. Kamvar in the fall/winter of 2017 
# 
# The purpose of this script is to generate the first table for the manuscript.
# ZNK has added comments throughout the file to guide the reader through the
# analyses.

 
# Setup -------------------------------------------------------------------
#
#
library(poppr)
library(tidyverse)
if (!interactive()) options(width = 200)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- readRDS(here::here("data", "full-genclone-object.rds"))


# MCG Tabulation ----------------------------------------------------------


# We can tabulate the MCGs by using them as custom multilocus genotypes.
mll.custom(CD) <- strata(CD) %>% 
  mutate(MCG = case_when(is.na(MCG) ~ "?", 
                         TRUE     ~ as.character(MCG))) %>% 
  pull(MCG)
mlg.table(CD, color = TRUE)

# The plot produced needs to be cleaned up a bit:
# 
#  1. The populations need to be arranged in the correct levels
#  2. The colors need to be replaced with our custom colors
#  3. We want to provide an indicator for the uncategorized MCGs
#  4. The text size and family needs to be set. 
#  
p          <- last_plot()
pal        <- other(CD)$palette
mcg_counts <- colSums(table(strata(CD, ~MCG/Population, combine = FALSE)))
names(pal) <- paste0(names(pal), "(", mcg_counts[names(pal)], ")")
pdf(here::here("figs/MCG-bar.pdf"), width = 7.20472 * 0.6, height = 7.20472 * 0.4, pointsize = 5, colormodel = "cmyk")
dev.control("enable") # allows me to copy to tiff device
p %+%
  mutate(p$data, Population = fct_relevel(Population, names(other(CD)$palette))) +
  scale_fill_manual(values = other(CD)$palette, labels = names(pal)) + 
  aes(alpha = ifelse(MLG == "?", "unknown", "known"), color = I("black")) +
  scale_alpha_manual(values = c(unknown = 0.5, known = 1), guide = "none") +
  guides(fill = guide_legend(nrow = 3)) +
  theme_bw(base_size = 10, base_family = "Helvetica") +
  theme(legend.position = "top") +
  theme(panel.grid.major.x = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.y = element_line(color = "grey20")) +
  theme(panel.grid.minor.y = element_line(color = "grey40", linetype = 2)) +
  theme(axis.text = element_text(color = "black")) +
  theme(axis.ticks = element_line(color = "black")) +
  theme(panel.border = element_rect(color = "black", size = 1)) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  labs(list(
    alpha = "MCG status",
    x     = "MCG",
    title = NULL
  ))
dev.copy(device = tiff, here::here("figs/MCG-bar.tiff"), width = 7.20472 * 0.6, height = 7.20472 * 0.4, pointsize = 5, units = "in", res = 1200)
dev.off()
dev.off()
dev.off()

# Checking loci and missing data ------------------------------------------

# Does the genotype accumulation curve plateau?
genotype_curve(CD, sample = 1000, thresh = 0.9)

# Are there patterns of missing data/population?
info_table(CD, type = "missing", percent = TRUE)

# Are there any samples with fewer than 9 typed loci?
table(nLoc(CD) * propTyped(CD))

# Genotypic and Allelic Diversity -----------------------------------------
#
# Here we are calculating the basic statistics for genotypic diversity.
# We are counting up Shannon-Weiner Index (H), Stoddardt and Taylor's Index (G)
# and the ratio of the two (E.5).
mll(CD) <- "original"
genotype_table <- poppr(CD, quiet = TRUE, lambda = FALSE, total = FALSE) %>%
  select(Pop, N, MLG, H, G, E.5)

# The indices of allelic diversity come from the locus_table() function, but
# they are presented over the entire data set. Here, we are splitting the 
# populations and getting one locus table per population so that we can extract
# information from them later
lts <- purrr::map(seppop(CD), locus_table, information = FALSE)

# One "easy" way to extract information is to create helper functions like this
# that will return a single number.
# 
# Calcuation for effective number of allelels
Ae <- function(loctab){
  mean(1 / (1 - loctab[-nrow(loctab), "Hexp", drop = TRUE]))
}
# Calculation for Nei's gene diversity
Hexp <- function(loctab){
  loctab[nrow(loctab), "Hexp", drop = TRUE]
}
# Calculation for the Average number of alleles/locus
nall <- function(loctab){
  loctab[nrow(loctab), "allele", drop = TRUE]
}

# Private Alleles (out of n alleles/locus) --------------------------------

print(pal <- private_alleles(CD, locus ~ Population, count.alleles = FALSE))
print(rowSums(pal)) # number of private alleles per Country

# Fraction of alleles in data that are private 
print(priv_fraction <- sweep(pal, 2, nAll(CD)[colnames(pal)], FUN = "/"))

# Function to correct encoding
correct_encoding <- function(path){
  readLines(path) %>% 
    iconv(from = "UTF-8", to = "ISO-8859-1") %>%
    writeLines(con = path)
}

# creating table 1 --------------------------------------------------------
# First, we can create a table of populations
table1_path <- here::here("tables", "country-population-year-n.csv")
poptable <- strata(CD) %>% 
  select(Continent, Country, Population, Year) %>% 
  group_by(Country, Population) %>%
  summarize(`Year(s) Collected` = Year %>% sort() %>% unique() %>% paste(collapse = ", ") , n = n()) %>%
  arrange(desc(Country), n) %>%
  ungroup() %>% 
  readr::write_csv(table1_path) %>%
  print()
correct_encoding(table1_path)

# Now we can take all of the data we gathered above and combine it
purrr::map_df(lts, ~{tibble::data_frame(Alleles = nall(.), 
                                        Ae      = Ae(.), 
                                        Hexp    = Hexp(.))}) %>% 
  tibble::add_column(N = tabulate(pop(CD)), .before = 1) %>% 
  dplyr::bind_cols(poptable, .) %>%
  dplyr::inner_join(genotype_table, by = c("Population" = "Pop", "N")) %>%
  tibble::add_column(private = rowSums(pal), .before = "Hexp") %>%
  dplyr::mutate_if(is.numeric, signif, 3) %>%
  dplyr::arrange(-N) %>%
  dplyr::mutate(N = glue::glue_data(., "{N} ({MLG})")) %>%
  select(-MLG) %>%
  readr::write_csv("tables/table1.csv") %>%
  print()

# Fixing silly encoding issue >:(
readLines(here::here("tables/table1.csv")) %>% 
  iconv(from = "UTF-8", to = "ISO-8859-1") %>%
  writeLines(con = here::here("tables/table1.csv"))

# Private Alleles (out of n alleles/locus) --------------------------------

(pal <- private_alleles(CD, locus ~ Population, count.alleles = FALSE))
rowSums(pal) # number of private alleles per Country

# Fraction of alleles in data that are private 
(priv_fraction <- sweep(pal, 2, nAll(CD)[colnames(pal)], FUN = "/"))

options(encoding = enc)








