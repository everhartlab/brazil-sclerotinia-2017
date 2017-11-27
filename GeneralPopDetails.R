# setwd("~/Thesis Project/Data Analysis")
library(poppr)

enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv")
splitStrata(CD) <- ~Continent/Country/Population
setPop(CD) <- ~Population

poppr(CD)


print(locus_table(CD), digits = 3)


# Private Alleles (out of n alleles/locus) --------------------------------

(pal <- private_alleles(CD, locus ~ Country, count.alleles = FALSE))
rowSums(pal) # number of private alleles per Country

# Fraction of alleles in data that are private 
(priv_fraction <- sweep(pal, 2, nAll(CD)[colnames(pal)], FUN = "/"))

options(encoding = enc)








