# setwd("~/Thesis Project/Data Analysis")
library(poppr)
library(ggcompoplot)
library(ggplot2)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv")
splitStrata(CD) <- ~Continent/Country/Population
setPop(CD) <- ~Population
my_palette <- c("Nebraska" = "#000000",
                "Argentina" = "#F0E442", # "#E69F00",
                "Bahia" = "#56B4E9",
                "Góias" = "#009E73",
                "Mato Grosso do Sul" = "#E69F00",
                "Minas Gerias" = "#0072B2",
                "Paraná" = "#D55E00",
                "Rio Grande do Sul" = "#CC79A7")

# ANTHONY
# 
# Anthony, please modify this so that:
#  - you can show why you chose 20 PCs ( use xvalDapc )
#  - show the scree plot for the discriminant axes ( this is an important diagnostic )
#  - the colors on the plot are correct ( hint: use the output of popNames(CD) to subset my_pallette )
#  - All the points show up in the plot.
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
  posi.leg = "bottomright",
  col = my_palette[popNames(CD)])                         
dev.copy(device = tiff, here::here("figs/DAPC-scatterplot.tiff"), width = 3.464565, height = 3.464565 * (1/1.2), pointsize = 5, units = "in", res = 1200)
dev.off()
dev.off()

# ANTHONY

pdf(here::here("figs/DAPC-barplot.pdf"), width = 7.20472, height = 3.464565, pointsize = 5, colormodel = "cmyk")
dev.control("enable")
gg <- ggcompoplot(CD_DAPC, CD, cols = 4, pal = my_palette)
gg$data$population <- factor(gg$data$population, names(my_palette))
gg$data$oldPopulation <- factor(gg$data$oldPopulation, names(my_palette))
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
