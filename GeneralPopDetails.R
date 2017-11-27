# setwd("~/Thesis Project/Data Analysis")
library(poppr)
library(tidyverse)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv")
splitStrata(CD) <- ~Continent/Country/Population
setPop(CD) <- ~Population

print(poppr(CD))


# Instead of number of alleles, diversity, and evenness at each locus, I’d
# prefer to see these statistics calculated for each population, with column
# names of (population, n, alleles per locus, effective alleles per locus,
# private alleles, and genetic diversity)
lts <- purrr::map(seppop(CD), locus_table, information = FALSE)

# Calcuation for effective number of allelels
Ae <- function(loctab){
  mean(1 / (1 - loctab[-nrow(loctab), "Hexp", drop = TRUE]))
}
Hexp <- function(loctab){
  loctab[nrow(loctab), "Hexp", drop = TRUE]
}
nall <- function(loctab){
  loctab[nrow(loctab), "allele", drop = TRUE]
}


# Private Alleles (out of n alleles/locus) --------------------------------

print(pal <- private_alleles(CD, locus ~ Population, count.alleles = FALSE))
print(rowSums(pal)) # number of private alleles per Country

# Fraction of alleles in data that are private 
print(priv_fraction <- sweep(pal, 2, nAll(CD)[colnames(pal)], FUN = "/"))


purrr::map_df(lts, ~{tibble::data_frame(Alleles = nall(.), 
                                        Ae = Ae(.), 
                                        Hexp = Hexp(.))}) %>%
  tibble::add_column(N = tabulate(pop(CD)), .before = 1) %>%
  dplyr::bind_cols(strata(CD) %>% dplyr::distinct() %>% select(Country, Population), .) %>%
  tibble::add_column(private = rowSums(pal), .before = "Hexp") %>%
  dplyr::mutate_if(is.numeric, signif, 3) %>%
  dplyr::arrange(-N) %>%
  readr::write_csv("tables/table1.csv") %>%
  print()


# Private Alleles (out of n alleles/locus) --------------------------------

(pal <- private_alleles(CD, locus ~ Population, count.alleles = FALSE))
rowSums(pal) # number of private alleles per Country

# Fraction of alleles in data that are private 
(priv_fraction <- sweep(pal, 2, nAll(CD)[colnames(pal)], FUN = "/"))

options(encoding = enc)








