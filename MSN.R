library(poppr)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv") 
my_palette <- c("Nebraska" = "#000000",
                "Argentina" = "#F0E442", # "#E69F00",
                "Bahia" = "#56B4E9",
                "Góias" = "#009E73",
                "Mato Grosso do Sul" = "#E69F00",
                "Minas Gerias" = "#0072B2",
                "Paraná" = "#D55E00",
                "Rio Grande do Sul" = "#CC79A7")
splitStrata(CD) <- ~Continent/Country/Population
setPop(CD) <- ~Population
CDrepet <- c(2,6,2,2,2,2,4,4,4,4,3)
bruvo.dist(CD, replen = CDrepet)


CD_sub <- popsub(CD, blacklist = character(0))
# ZNK: add and loss are parameters specifically for polyploid data with missing 
#      data and are both TRUE by default. You do not need to specify them here.
min_span_net <- bruvo.msn(CD_sub, replen = c(CDrepet), add = TRUE, loss = TRUE, showplot = FALSE, include.ties = FALSE) 
set.seed(69)
pdf(here::here("figs/MSN.pdf"), width = 7.20472, height = 7.20472 * (1/1.6), pointsize = 5, colormodel = "cmyk")
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