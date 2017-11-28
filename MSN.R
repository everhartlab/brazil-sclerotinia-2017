library(poppr)
library("igraph")
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

CD_sub <- popsub(CD, blacklist = character(0))
# ZNK: add and loss are parameters specifically for polyploid data with missing 
#      data and are both TRUE by default. You do not need to specify them here.
min_span_net <- bruvo.msn(CD_sub, replen = c(CDrepet), add = TRUE, loss = TRUE, showplot = FALSE, include.ties = FALSE) 
set.seed(69)
min_span_net <- plot_poppr_msn(CD,
               min_span_net,
               inds = "NONE",
               mlg = FALSE,
               gadj = 3,
               palette = my_palette,
               cutoff = NULL,
               quantiles = FALSE,
               beforecut = TRUE,
               layfun = igraph::layout_nicely)
opar <- par(no.readonly = TRUE)
pdf(here::here("figs/MSN.pdf"), width = 3.464565 * 1,  height = 3.464565 * 1, pointsize = 5, colormodel = "cmyk")

par(mar = c(0.1, 0.1, 0.1, 0.1))
# code from plot_poppr_msn.
make_scale_bar <- function(msn, glim = c(0, 0.8), gadj = 3){
  w    <- edge_attr(msn$graph, "weight")
  wmin <- min(w)
  wmax <- max(w)
  scales <- seq(wmin, wmax, l = 1000)
  greyscales <- grDevices::gray(poppr:::adjustcurve(scales, show = FALSE, glim = glim, correction = gadj))
  legend_image <- grDevices::as.raster(matrix(greyscales, nrow = 1))
  graphics::par(mar = c(0, 1, 0, 1) + 0.5)
  graphics::plot.new()
  graphics::rasterImage(legend_image, 0, 0.5, 1, 1)
  graphics::polygon(c(0, 1, 1), c(0.5, 0.5, 0.8), col = "white", 
                    border = "white", lwd = 2)
  graphics::axis(3, at = c(0, 0.25, 0.5, 0.75, 1), 
                 labels = round(quantile(scales), 3))
  graphics::text(0.5, 0, labels = "Bruvo's distance", font = 2, 
                 cex = 1.5, adj = c(0.5, 0))
}
graphics::layout(matrix(c(1,2), nrow = 2), heights = c(4.5, 0.5))
set.seed(69)
# Graph is plotted so the area is scaled by number of samples
vsizes <- sqrt(vertex_attr(min_span_net$graph, "size")) * 5
plot.igraph(min_span_net$graph, 
            margin = -0.025,
            vertex.size = vsizes,
            vertex.label = NA)

sortpop <- names(my_palette)
a <- legend("topleft", 
       legend = sortpop, 
       fill = min_span_net$colors[sortpop],
       title = "Population")

uvsizes <- sort(unique(vsizes))
leg     <- paste("", (uvsizes/5)^2, "")
legend("topright", 
       ncol = 2, 
       legend = leg,
       pch = 21, 
       pt.cex = uvsizes/3, 
       title = "Samples per MLG", 
       # x.intersp = 1.5,
       horiz = TRUE)
# circlx <- a$text$x[1:2] + diff(c(a$text$x[1], a$rect$left))
# circly <- rep(a$rect$top - a$rect$h - min(abs(diff(a$text$y))), 2)
# rads <- uvsizes[c(1, length(uvsizes))]/200
# symbols(circlx, (rads/pi) + circly, circles = rads, add = TRUE)
make_scale_bar(min_span_net)
graphics::layout(matrix(1, ncol = 1, byrow = TRUE))

par(opar)
dev.off()
options(encoding = enc)
