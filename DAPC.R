# setwd("~/Thesis Project/Data Analysis")
library(poppr)
library(ggcompoplot)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv")
splitStrata(CD) <- ~Continent/Country/Population
setPop(CD) <- ~Population
my_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                "#D55E00", "#CC79A7", "#FFFFFF")


CD_DAPC <- dapc(CD, n.pca=20, n.da=7) # ZNK: Why did you choose 20 PCs? I suggest you use xvalDapc() with the parameter n.pca. 
pdf(here::here("figs/DAPC-scatter.pdf"), width = 3.464565, height = 3.464565 * (1/1.6), pointsize = 5, colormodel = "cmyk")
scatter.dapc(CD_DAPC, cex=1.0, scree.da=F, clabel=0, cstar=0, cellipse = 1, legend=T, 
        col=my_palette)                         
dev.off()

pdf(here::here("figs/DAPC-barplot.pdf"), width = 3.464565, height = 3.464565 * 1.6, pointsize = 5, colormodel = "cmyk")
ggcompoplot(CD_DAPC, CD, cols = 2)
dev.off()
options(encoding = enc)
getOption("encoding")
