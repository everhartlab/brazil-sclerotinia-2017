# setwd("~/Thesis Project/Data Analysis")
#
# Make amova table from pegas output
#
#' Title
#'
#' @param am      The amova table from pegas
#' @param within  A logical whether or not the lowest level is within individuals
#'
#' @return a data frame
make_amova_table <- function(am, within = FALSE){
  siggies <-
    if (is.data.frame(am$varcomp))
      setNames(am$varcomp, c("sigma", "P"))
    else
      data.frame( sigma = am$varcomp )
  vars <- rownames(am$tab[-(nrow(am$tab) - 0:1), ])
  sig  <- siggies$sigma
  siggies$perc <- sig/sum(sig)

  # Calculating phi statistics
  nsig <- length(sig)
  vnames <- paste(vars, "within", c("Total", vars[-length(vars)]))
  phis <- matrix(0.0, nrow = nsig - 1, ncol = nsig - 1,
                 dimnames = list(c("Total", vars[-length(vars)]),
                                 vars))
  for (i in 1:(nsig - 1)) {
    for (j in i:(nsig - 1)) {
      phis[i, j] <- sum(sig[i:j])/sum(sig[i:nsig])
    }
  }
  wn <- c(diag(phis), if (within) phis[1, ncol(phis) - 1] else phis[1, ncol(phis)])

  # creating the resulting data frame
  res <- data.frame(am$tab[-nrow(am$tab), c(3, 1)],
                    siggies,
                    Phi = wn)
  rownames(res) <- c(vnames, "Error")
  res
}

library("poppr")
library("tidyverse")
if (!interactive()) options(width = 200)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- readRDS(here::here("data", "full-genclone-object.rds"))
CDdist <- bruvo.dist(CD, replen = other(CD)$REPLEN)
set.seed(2017 - 11 - 29)
CDamova <- pegas::amova(CDdist ~ Continent/Country/Population, data = strata(CD), nperm = 1000)
make_amova_table(CDamova) %>%
  tibble::rownames_to_column(var = "Levels") %>%
  dplyr::mutate(Levels = trimws(Levels)) %>%
  dplyr::select(Levels, dplyr::everything()) %T>%
  print() %>%
  dplyr::mutate_if(is.numeric, signif, 3) %>%
  readr::write_csv("tables/table2.csv")
options(encoding = enc)
