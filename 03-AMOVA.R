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

res <- apply(to_test, 
             MARGIN = 2, 
             FUN = single_pair_amova, 
               popdf = strata(CD), dist = CDdist, nperm = 999)
for (i in seq(res)){
  phi_location <- lower.tri(out)
  p_location   <- upper.tri(out)
  varcomp <- res[[i]]$varcomp
  out[phi_location][i] <- varcomp$sigma2[1]/sum(varcomp$sigma2)
  out[p_location][i]   <- varcomp$P.value[1]
}
out %>% 
  signif(3) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = " ") %>%
  readr::write_csv(here::here("tables/pair_amova.csv"))
# Visualizing the Brazilian Populations
outtype <- out
outtype[lower.tri(outtype)] <- "Phi"
outtype[upper.tri(outtype)] <- "P-value"
diag(outtype) <- NA
(outtype <- reshape2::melt(outtype, value.name = "type") %>% as_tibble())
reshape2::melt(out) %>% 
  full_join(outtype) %>%
  filter(!Var1 %in% c("Argentina", "Midwest"),
         !Var2 %in% c("Argentina", "Midwest")) %>%
  ggplot(aes(x = Var1, y = fct_rev(Var2), fill = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 3), color = type)) +
  theme_bw() +
  theme(axis.title = element_blank()) +
  theme(axis.text.x = element_text(
                                   angle = 90,
                                   hjust = 1,
                                   vjust = 0.5
                                 )) + 
  viridis::scale_fill_viridis() + 
  scale_color_manual(values = c(`P-value` = "firebrick",
                                Phi = "black")) +
  coord_fixed()
  
options(encoding = enc)
