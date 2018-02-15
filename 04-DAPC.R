library(poppr)
library(ggcompoplot)
library(ggplot2)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- readRDS(here::here("data", "full-genclone-object.rds"))

if (interactive()){
  set.seed(2017-12-14)
  CDXVAL <- xvalDapc(
    tab(CD, NA.method = "mean"),
    grp = pop(CD),
    n.pca.max = 30,
    n.rep = 500,
    n.pc = seq.int(15)
  )
}

CD_DAPC <- dapc(CD, n.pca = 7L, n.da = 4L)
CD_DAPC
pdf(here::here("figs/DAPC-scatterplot.pdf"), width = 3.464565, height = 3.464565 * (1/1.2), pointsize = 5, colormodel = "cmyk")
dev.control("enable")
scatter.dapc(
  CD_DAPC,
  pch = 19,
  cex = 1.0,
  clabel = 0,
  cstar = 0,
  cellipse = 1,
  legend = TRUE,
  inset.da = 0,
  posi.da = "bottomleft",
  posi.leg = "topleft",
  col = other(CD)$palette[popNames(CD)])
dev.copy(device = tiff, here::here("figs/DAPC-scatterplot.tiff"), width = 3.464565, height = 3.464565 * (1/1.2), pointsize = 5, units = "in", res = 1200)
dev.off()
dev.off()

# ANTHONY

pdf(here::here("figs/DAPC-barplot.pdf"), width = 7.20472, height = 3.464565, pointsize = 5, colormodel = "cmyk")
dev.control("enable")
gg <- ggcompoplot(CD_DAPC, CD, cols = 4, pal = other(CD)$palette)
gg$data$population <- factor(gg$data$population, names(other(CD)$palette))
gg$data$oldPopulation <- factor(gg$data$oldPopulation, names(other(CD)$palette))
gg + 
  theme_bw(base_size = 10, base_family = "Helvetica") +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "top") 
dev.copy(device = tiff, here::here("figs/DAPC-barplot.tiff"), width = 7.20472, height = 3.464565, pointsize = 5, units = "in", res = 1200)
dev.off()
dev.off()
options(encoding = enc)
getOption("encoding")