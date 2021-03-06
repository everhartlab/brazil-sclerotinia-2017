library("poppr")
library("igraph")
enc <- getOption("encoding")
in_script <- !interactive()
options(encoding = "iso-8859-1")
CD <- readRDS(here::here("data", "full-genclone-object.rds"))

min_span_net <- bruvo.msn(CD, replen = other(CD)$REPLEN,  showplot = FALSE, include.ties = TRUE) 
set.seed(69)
min_span_net <- plot_poppr_msn(CD,
               min_span_net,
               inds = "NONE",
               mlg = FALSE,
               gadj = 3,
               palette = other(CD)$palette,
               cutoff = NULL,
               quantiles = FALSE,
               beforecut = TRUE,
               pop.leg = FALSE,
               layfun = igraph::layout_nicely)
opar <- par(no.readonly = TRUE)
if (in_script){
  pdf(here::here("figs/MSN.pdf"), width = 3.464565 * 1,  height = 3.464565 * 1, pointsize = 5, colormodel = "cmyk")
  dev.control("enable")  
}

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
set.seed(124)
# Graph is plotted so the area is scaled by number of samples
vsizes <- vertex_attr(min_span_net$graph, "size")
vsizes <- if (packageVersion("poppr") < package_version("2.5.0.99")) sqrt(vsizes) * 5 else vsizes * 5

lay <- igraph::layout_with_gem(min_span_net$graph)#[, 2:1]

plot.igraph(min_span_net$graph, 
            margin = -0.025,
            vertex.size = vsizes,
            vertex.label = NA,
            layout = lay)

# Create population legend and save it into variable "a"
sortpop <- names(other(CD)$palette)
a <- legend(x = -1.2, 
            y = 0.2, 
            legend = sortpop, 
            fill   = min_span_net$colors[sortpop])

# Create example circles for comparison
rads   <- (sqrt(seq(5, 1))*5)/200

# Get the bottom of the pop legend
ybot   <- a$rect$top - a$rect$h
# Get the space between legend elements
yspace <- min(abs(diff(a$text$y)))
# Create positions of circles vertically
circly <- rep(ybot - (2.5 * yspace), length(rads))

# Find the distance between two circles.
# https://stackoverflow.com/a/14830596/2752888
cdist <- function(c1, c2){
  a <- (c1 + c2)^2
  b <- (c1 - c2)^2
  sqrt(a - b)
}

# spread the circles out 
make_adjacent_circles <- function(radii){
  res <- vapply(seq(radii), function(i){
    if (i == 1) 
        0.0
    else 
      cdist(radii[i], radii[i - 1])
    }, numeric(1))
  cumsum(res)
}

# shift the x position of the circles
circlx <- a$rect$left + a$rect$w/4
circlx <- make_adjacent_circles(rads) + circlx

# Create the circle legend
text(x = a$rect$left + a$rect$w/2, y = ybot - (yspace), label = "Samples per MLG")
symbols(x = circlx, y = circly, circles = rads, add = TRUE, inches = FALSE, asp = 1)
text(x = circlx, y = circly, labels = seq(5, 1), font = 2)

# Create the scale bar legend
make_scale_bar(min_span_net)

# reset the graphics
graphics::layout(matrix(1, ncol = 1, byrow = TRUE))
if (in_script) dev.copy(device = tiff, here::here("figs/MSN.tiff"), width = 3.464565 * 1,  height = 3.464565 * 1, pointsize = 5, units = "in", res = 1200)
# reset par
par(opar)
if (in_script) {
  dev.off()
  dev.off()
}
options(encoding = enc)
