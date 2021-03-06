# setwd("~/Thesis Project/Data Analysis")
library("poppr")
library("ape")
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- readRDS(here::here("data", "full-genclone-object.rds"))
set.seed(2017 - 12 - 19)
CDTree <- bruvo.boot(CD,
                     replen = other(CD)$REPLEN,
                     sample = 1000,
                     tree = njs, # nj* because there may be missing data.
                     showtree = FALSE,
                     cutoff = 75)
# Removing base node label, which is always 100 and meaningless
CDTree$node.labels[1] <- NA
CDTree$node.labels <- ifelse(is.na(CDTree$node.labels), NA, paste0("<- ", CDTree$node.labels))
CDTree$node.labels[40] <- "<-75"

# Tree suggestions --------------------------------------------------------
#
# ZNK: Trees can be slightly irritating to deal with. I've added some options
#      that you might want to consider for the publication. Choose either one
#      you want, customize it to what you want to show (either by changing
#      the tip labels or anything like that), and then add code to save it to
#      a pdf with the correct width for phytopathology.
## setup ------------------------------------------------------------------
pops <- strata(CD)$Population
pops <- factor(pops, levels = c("Midwest", sort(levels(pops)[levels(pops) != "Midwest"])))
# sorting the legend so Nebraska is first
popleg  <- levels(pops)
popcols <- other(CD)$palette[popleg]
#' Get the parent edges to nodes
#'
#' @param tree an object of class "phylo" (see [ape::phylo])
#' @param nodes an integer vector specifying the nodes to find parent edges for
#' @param internal.only an indicator specifying if the nodes are indexed from
#'   the first internal node `internal.only = TRUE` (default) or includes tips
#'   `internal.only = FALSE`.
#'
#' @return a logical vector for all edges indicating if the edge is a parent of
#'   a given node.
parent_edge <- function(tree, nodes, internal.only = TRUE){
  emat <- tree$edge
  nodes <- if (internal.only) nodes + Ntip(tree) else nodes
  emat[, 2] %in% nodes
}
# Getting all node labels greater than 75
edges_to_highlight <- parent_edge(CDTree, which(grepl("[-]", CDTree$node.labels)))


# Unrooted Tree -----------------------------------------------------------
# The unrooted tree is often ideal because it doesn't imply that any one
# population is more derived than the other.
{
pdf(here::here("figs/tree.pdf"), width = 3.464565, height = 3.464565, pointsize = 5, colormodel = "cmyk")
dev.control("enable")
plot.phylo(
  CDTree,
  show.node.label = TRUE,
  font = 2,
  no.margin = TRUE,
  type = "unrooted",
  lab4ut = "axial",    # lab4ut makes it so the tip labels are axial instead of horizontal
  label.offset = 0.01,
  rotate.tree = 45,     # adjust this to manually rotate the tree
  show.tip.label = FALSE, # removing the tip labels to use points instead
  edge.width = 2,
  edge.color = c("black", "tomato")[edges_to_highlight + 1]
)
# Normally for tip labels, we would use the "tiplabels" function, but since we
# have overlapping samples (clones), one way to represent them would be to
# jitter these. We can get the coordinates of the points from the
# "last_plot.phylo" object:
# http://grokbase.com/t/r/r-sig-phylo/137syf0c3c/coordinates-for-phylo-tips
lastPP <- get("last_plot.phylo", envir = .PlotPhyloEnv)
tip    <- 1:lastPP$Ntip
XX     <- lastPP$xx[tip]
YY     <- lastPP$yy[tip]
# We can find out which points need jittering by finding out which ones have a
# zero-valued distance
jits <- colSums(as.matrix(dist(data.frame(XX, YY))) == 0) > 1
# I'm only going to jitter along the x axis here.
# set.seed(2017-11-26)
set.seed(2018-05-26)
XX[jits] <- jitter(XX[jits], amount = diff(range(XX[-jits]))/100)
# Again, normally I would use "tiplabels" for this, but because I want to use
# the jitter, I must use "points".
points(x   = XX,
       y   = YY,
       pch = 21,
       cex = 2,
       bg  = transp(popcols[as.character(pops)], 0.75))
legend(x = 0, y = 0.2, legend = popleg, fill = popcols)
add.scale.bar(x = 0, y = 0.225, lwd = 2)
dev.copy(device = tiff, here::here("figs/tree.tiff"), width = 3.464565, height = 3.464565, pointsize = 5, units = "in", res = 1200)
dev.off()
dev.off()
}
# # Radial Tree -------------------------------------------------------------
#
# # The radial tree was what you were playing with earlier. This is nice
# # because it tends to be easier to fit on a single page and you can align
# # the tip labels. The downside is that the relationships are a bit harder to
# # parse visually and it gives the impression of rooting.
# plot.phylo(
#   CDTree,
#   font = 2,
#   no.margin = TRUE,
#   type = "fan",
#   lab4ut = "axial",
#   label.offset = 0.004,
#   tip.color = popcols[as.character(pops)],
#   align.tip.label = TRUE,
#   edge.width = 2,
#   x.lim = c(-0.75, 0.75), # Setting the limits here to accomidate labels
#   y.lim = c(-0.75, 0.75),
#   open.angle = 60,        # The angle allows us to set the legend and scale bar
#   edge.color = c("black", "red")[edges_to_highlight + 1]
# )
# add.scale.bar(x = 0.25, y = -0.075, lwd = 2)
# legend(x = 0.25, y = -0.125, legend = popleg, fill = popcols)
# # nodelabels(text = ifelse(CDTree$node.labels > 75, CDTree$node.labels, NA),
# #            frame = "circle",
# #            bg = "white",
# #            cex = 0.5)
options(encoding = enc)
