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
# 
CD_DAPC <- dapc(CD, n.pca=20, n.da=7) 
pdf(here::here("figs/DAPC-scatterplot.pdf"), width = 3.464565, height = 3.464565 * (1/1.6), pointsize = 5, colormodel = "cmyk")
scatter.dapc(CD_DAPC, cex=1.0, scree.da=F, clabel=0, cstar=0, cellipse = 1, legend=T, 
        col=my_palette)                         
dev.off()
# ANTHONY


pdf(here::here("figs/DAPC-barplot.pdf"), width = 7.20472, height = 3.464565, pointsize = 5, colormodel = "cmyk")
gg <- ggcompoplot(CD_DAPC, CD, cols = 4, pal = my_palette)
gg$data$population <- factor(gg$data$population, names(my_palette))
gg$data$oldPopulation <- factor(gg$data$oldPopulation, names(my_palette))
gg + 
  theme_bw(base_size = 10, base_family = "Helvetica") +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.position = "top") 
dev.off()
options(encoding = enc)
getOption("encoding")
