# setwd("~/Thesis Project/Data Analysis")
# 
# Make amova table from ade4 output
# 
#' Title
#'
#' @param am      The amova table from poppr.amova
#' @param amt     The results of randtest.amova on `am`
#' @param samples A character vector specifying how the "Samples" field should
#'   be renamed.
#'   
#' @return a data frame
#' @note Source:
#'   https://github.com/everhartlab/sclerotinia-366/blob/v1.4/doc/RMD/by-year.Rmd
make_amova_table <- function(am, amt, samples = "Region"){
  tot <- nrow(am$results)
  res <- data.frame(list(am$results[-tot, c("Df", "Sum Sq")], 
                         Percent = am$componentsofcovariance[-tot, 2],
                         Pval    = rev(amt$pvalue), 
                         Sigma   = am$componentsofcovariance[-tot, 1],
                         Phi     = rev(am$statphi$Phi[-tot])))
  res <- as.matrix(res)
  colnames(res) <- c("d.f.", "Sum of Squares", "Percent variation", "P", 
                     "Sigma", "Phi statistic")
  names(dimnames(res)) <- c("levels", "statistic")
  rownames(res) <- gsub("samples", samples, rownames(res))
  return(res)
}



library("poppr")
library("tidyverse")
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv")
splitStrata(CD) <- ~Continent/Country/Population
CDrepet <- c(2,6,2,2,2,2,4,4,4,4,3)
CDdist <- bruvo.dist(CD, replen=CDrepet)
CDamova <- poppr.amova(CD, ~Continent/Country/Population, cutoff=0.1, dist=CDdist)
# CDamovacc <- poppr.amova(CDSA, ~Country/Continent, clonecorrect = T, # ZNK: RETURNS AN ERROR -- what is CDSA? Where did it come from?
#                          cutoff=0.10, within=F)
set.seed(2017-11-29)
CDsignif <- randtest(xtest=CDamova, nrepet = 999)
# CDccsignif <- randtest(CDamovacc, nrepet = 999)
make_amova_table(CDamova, CDsignif) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Levels") %>%
  dplyr::mutate(Levels = trimws(Levels)) %>%
  dplyr::select(Levels, dplyr::everything()) %T>%
  print() %>%
  dplyr::mutate_if(is.numeric, signif, 3) %>%
  readr::write_csv("tables/table2.csv") 
options(encoding = enc)