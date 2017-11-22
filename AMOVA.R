# setwd("~/Thesis Project/Data Analysis")
library(poppr)
enc <- getOption("encoding")
options(encoding = "iso-8859-1")
CD <- read.genalex(here::here("data", "data.csv")) #"~/Thesis Project/Data Analysis/Raw Data/Compiled Data AN 2.csv")
splitStrata(CD) <- ~Continent/Country/Population
CDrepet <- c(2,6,2,2,2,2,4,4,4,4,3)
CDdist <- bruvo.dist(CD, replen=CDrepet)
CDamova <- poppr.amova(CD, ~Continent/Country/Population,cutoff=0.1, dist=CDdist)
CDamovacc <- poppr.amova(CDSA, ~Country/Continent, clonecorrect = T, # ZNK: RETURNS AN ERROR -- what is CDSA? Where did it come from?
                         cutoff=0.10, within=F)
CDsignif <- randtest(xtest=CDamova, nrepet = 999)
CDccsignif <- randtest(CDamovacc, nrepet = 999)
options(encoding = enc)

