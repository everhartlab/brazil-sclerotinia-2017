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

# Function to correct encoding
correct_encoding <- function(path){
  readLines(path) %>% 
    iconv(from = "UTF-8", to = "ISO-8859-1") %>%
    writeLines(con = path)
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

correct_encoding("tables/table2.csv")

# Pairwise AMOVA

CDpops <- seppop(CD, ~Population)
out <- matrix(0, nrow = nPop(CD), ncol = nPop(CD),
              dimnames = list(popNames(CD), popNames(CD)))
single_pair_amova <- function(pair = NULL, popdf = NULL, column = "Population", dist, ...){
  if (is.null(pair)) stop("pair must be a vector specifying the populations to include")
  if (is.null(popdf)) stop("popdf must be a data frame specifying the population of each individual")
  to_subset <- popdf[[column]] %in% pair
  popdf <- popdf[to_subset, , drop = FALSE]
  dist <- as.dist(as.matrix(dist)[to_subset, to_subset, drop = FALSE])
  form <- as.formula("dist ~ Population", env = environment())
  pegas::amova(form, data = popdf, ...)
}
to_test <- combn(names(CDpops), 2)

set.seed(2017 - 11 - 29)
res <- apply(to_test, 
             MARGIN = 2, 
             FUN = single_pair_amova, 
               popdf = strata(CD), dist = CDdist, nperm = 1000)
for (pair in seq(res)) {
   i <- to_test[2, pair]
   j <- to_test[1, pair]
  varcomp   <- res[[pair]]$varcomp
  p         <- varcomp$P.value[1]
  stars     <- ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))
  stars     <- paste0("%.03f", stars)
  out[i, j] <- sprintf(stars, round(varcomp$sigma2[1]/sum(varcomp$sigma2), 3))
  out[j, i] <- sprintf(stars, round(p, 3))
}
out %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = " ") %>%
  readr::write_csv(here::here("tables/pair_amova.csv"))

correct_encoding("tables/pair_amova.csv")
# Visualizing the Brazilian Populations

options(encoding = enc)
