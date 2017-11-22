library(poppr)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv") 
my_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                  "#D55E00", "#CC79A7", "#FFFFFF")
splitStrata(CD) <- ~Continent/Country/Population
setPop(CD) <- ~Population
CDrepet <- c(2,6,2,2,2,2,4,4,4,4,3)
bruvo.dist(CD, replen = CDrepet)


CD_sub <- popsub(CD, blacklist = character(0))
# ZNK: add and loss are parameters specifically for polyploid data with missing 
#      data and are both TRUE by default. You do not need to specify them here.
min_span_net <- bruvo.msn(CD_sub, replen = c(CDrepet), add = TRUE, loss = TRUE, showplot = FALSE, include.ties = FALSE) 
set.seed(69)
pdf(here::here("figs/MSN.pdf"), width = 3.464565, height = 3.464565, colormodel = "cmyk")
plot_poppr_msn(CD,
               min_span_net,
               inds = "NONE",
               mlg = FALSE,
               gadj = 3,
               palette = my_palette,
               cutoff = NULL,
               quantiles = FALSE,
               beforecut = TRUE,
               layfun = igraph::layout_nicely)
dev.off()
options(encoding = enc)