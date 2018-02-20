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


# Function to correct encoding
correct_encoding <- function(path){
  readLines(path) %>% 
    iconv(from = "UTF-8", to = "ISO-8859-1") %>%
    writeLines(con = path)
}

table1_path <- here::here("tables", "country-population-year-n.csv")
table2_path <- here::here("tables", "diversity-statistics.csv")


# creating table 1 --------------------------------------------------------

poptable <- strata(CD) %>% 
  select(Continent, Country, Population, Year) %>% 
  group_by(Continent, Country, Population) %>%
  summarize(`Year(s) Collected` = Year %>% sort() %>% unique() %>% paste(collapse = ", ") , n = n()) %>%
  arrange(desc(Country), -n) %>%
  ungroup() %>% 
  readr::write_csv(table1_path) %>%
  print()
correct_encoding(table1_path)

# creating table 2 --------------------------------------------------------
# This table is a bit more complicated because we want to show summary
# statistics over all heirarchical levels. We can first calculate private
# alleles over all these levels.

# Private Alleles (out of n alleles/locus) --------------------------------

(pal_pop  <- private_alleles(CD, locus ~ Population, count.alleles = FALSE))
(pal_ctry <- private_alleles(CD, locus ~ Country,    count.alleles = FALSE))
(pal_ctnt <- private_alleles(CD, locus ~ Continent,  count.alleles = FALSE))
private_allele_table <- map_df(list(pal_pop, pal_ctry, pal_ctnt), 
       ~enframe(rowSums(.x), name = "Population", value = "private"))

# Fraction of alleles in data that are private 
print(priv_fraction <- sweep(pal_pop, 2, nAll(CD)[colnames(pal_pop)], FUN = "/"))
print(priv_fraction <- sweep(pal_ctry, 2, nAll(CD)[colnames(pal_ctry)], FUN = "/"))
print(priv_fraction <- sweep(pal_ctnt, 2, nAll(CD)[colnames(pal_ctnt)], FUN = "/"))

# To calculate allele and genotype diversity, the easiest way is to create a 
# list containing all of our heirarchical levels and iterate over those using
# purrr::map()

mll(CD) <- "original"
poplist <- c(seppop(CD),
             seppop(CD, ~Country),
             seppop(CD, ~Continent),
             list(Pooled = CD)
             )

# Removing population factor from the pooled populations
pop(poplist[["Pooled"]]) <- NULL

# Allelic Diversity -------------------------------------------------------
# We only want the average value across loci for these stats

locus_tables <- purrr::map(poplist, locus_table, information = FALSE)

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

# Now we can take all of the data we gathered above and combine it
main_locus_table <- purrr::map_df(locus_tables, 
                                  ~{tibble::data_frame(Alleles = nall(.), 
                                                       Ae      = Ae(.), 
                                                       Hexp    = Hexp(.)
                                                       )
                                    }, 
                                  .id = "Population")


# Genotypic Diversity -----------------------------------------------------


# Here we are calculating the basic statistics for genotypic diversity.
# We are counting up Shannon-Weiner Index (H), Stoddardt and Taylor's Index (G)
# and the ratio of the two (E.5).
genotype_table <- purrr::map_df(poplist, 
                                poppr, 
                                  quiet = TRUE, lambda = FALSE, total = FALSE, 
                                .id = "Population") %>%
  select(Population, N, MLG, H, G, E.5) %>%
  mutate(H = exp(H)) %>%
  rename(eH = H)

# Combining Private Alleles, Genotypic, and Allelic Diversity -------------
# Here we can join all the tables together and polish them.

dplyr::left_join(main_locus_table, genotype_table, by = "Population") %>%
  dplyr::left_join(private_allele_table, by = "Population") %>%
  dplyr::distinct() %>%
  dplyr::mutate(Country = case_when(
    Population == "Midwest" ~ "United States",
    Population == "United States" ~ "United States", 
    Population == "Argentina"     ~ "Argentina",
    Population == "Brazil"        ~ "Brazil",
    Population == "North America" ~ "-", # These labels don't apply to Country
    Population == "South America" ~ "-",
    Population == "Pooled"        ~ "-",
    TRUE                          ~ "Brazil"
    )) %>%
  dplyr::mutate(Continent = case_when(
    Country == "United States" ~ "North America",
    Country == "-"             ~ Population,
    TRUE                       ~ "South America"
  )) %>%
  dplyr::mutate(Population = case_when(
    Population %in% Continent ~ "-", # These labels don't apply to populations
    Population %in% Country   ~ "-",
    TRUE                      ~ Population
  )) %>% 
  dplyr::mutate(Continent = gsub("o[ur]th", ".", Continent)) %>% # North -> N./South -> S.
  dplyr::mutate(Country = gsub("United States", "U.S.", Country)) %>%
  dplyr::select(Continent, Country, Population, N, MLG, Alleles, Ap = private, everything()) %>%
  dplyr::arrange(Continent == "Pooled",     # This sequence arranges first the
                 Continent == "N. America", # pooled data, then continent, 
                 Country   == "-",          # country, and finally, N.
                 desc(Country), 
                 Population == "-", 
                 -N) %>%
  dplyr::filter(Continent != "N. America" | Population != "-") %>% # removing duplicates
  dplyr::mutate_if(is.numeric, signif, 3) %>%
  dplyr::mutate(N = glue::glue_data(., "{sprintf('%2d', N)} ({sprintf('%2d', MLG)})")) %>%
  select(-MLG) %>%
  readr::write_csv(table2_path) %>%
  print()

correct_encoding(table2_path)
  
options(encoding = enc)








