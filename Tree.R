# setwd("~/Thesis Project/Data Analysis")
library("poppr")
library("ape")
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv")
splitStrata(CD) <- ~Continent/Country/Population
setPop(CD) <- ~Country
CDrepet <- c(2,6,2,2,2,2,4,4,4,4,3)

CDdist <- bruvo.dist(CD[CD$pop], replen=CDrepet) # ZNK: RETURNS A WARNING -- why are you subsetting this
CDTree <- bruvo.boot(CD, replen = CDrepet, add=T, loss=T,           # ZNK: You should add a seed before this line
                     sample=1000, tree="nj", showtree=T, cutoff=50) # ZNK: In the paper you mention 1000 replicates
cols <- c("#000000", "#E69F00", "#009E73","#56B4E9" , "#F0E442", "#0072B2",
          "#D55E00", "#CC79A7", "#FFFFFF")
plot.phylo(CDTree, cex=0.5, font=2, tip.color=cols[CD$pop], 
           show.node.label = T, adj=0, no.margin=T, x.lim=c(0.02,0.665), y.lim=c(3,92), 
           label.offset = 0.004, type = "fan") # ZNK: This plots strange in my Rstudio. You may want to change the x and y parameters


# Tree suggestions --------------------------------------------------------
#
# ZNK: Trees can be slightly irritating to deal with. I've added some options
#      that you might want to consider for the publication. Choose either one
#      you want, customize it to what you want to show (either by changing
#      the tip labels or anything like that), and then add code to save it to
#      a pdf with the correct width for phytopathology. 
## setup ------------------------------------------------------------------
pops <- strata(CD)$Population
# sorting the legend so Nebraska is first
popleg  <- c("Nebraska", sort(levels(pops)[levels(pops) != "Nebraska"]))
popcols <- setNames(cols, popleg)
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
edges_to_highlight <- parent_edge(CDTree, which(CDTree$node.labels > 75))


# Unrooted Tree -----------------------------------------------------------

# The unrooted tree is often ideal because it doesn't imply that any one
# population is more derived than the other. 
plot.phylo(
  CDTree,
  font = 2,
  no.margin = TRUE,
  type = "unrooted",
  lab4ut = "axial",    # lab4ut makes it so the tip labels are axial instead of horizontal
  label.offset = 0.004,
  rotate.tree = 5,     # adjust this to manually rotate the tree
  tip.color = popcols[as.character(pops)],
  edge.width = 2,
  edge.color = c("black", "red")[edges_to_highlight + 1]
)
legend(x = 0, y = 0.2, legend = popleg, fill = popcols)
add.scale.bar(x = 0, y = 0.225, lwd = 2)

# Radial Tree -------------------------------------------------------------

# The radial tree was what you were playing with earlier. This is nice 
# because it tends to be easier to fit on a single page and you can align
# the tip labels. The downside is that the relationships are a bit harder to
# parse visually and it gives the impression of rooting.
plot.phylo(
  CDTree,
  font = 2,
  no.margin = TRUE,
  type = "fan",
  lab4ut = "axial",
  label.offset = 0.004,
  tip.color = popcols[as.character(pops)],
  align.tip.label = TRUE,
  edge.width = 2,
  x.lim = c(-0.75, 0.75), # Setting the limits here to accomidate labels
  y.lim = c(-0.75, 0.75),
  open.angle = 60,        # The angle allows us to set the legend and scale bar
  edge.color = c("black", "red")[edges_to_highlight + 1]
)
add.scale.bar(x = 0.25, y = -0.075, lwd = 2)
legend(x = 0.25, y = -0.125, legend = popleg, fill = popcols)
# nodelabels(text = ifelse(CDTree$node.labels > 75, CDTree$node.labels, NA),
#            frame = "circle",
#            bg = "white",
#            cex = 0.5)
options(encoding = enc)


