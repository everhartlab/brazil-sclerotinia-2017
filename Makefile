FIGURES := tree.pdf DAPC-scatterplot.pdf DAPC-barplot.pdf MSN.pdf
TABLES  := table1.csv
FIGURES := $(addprefix figs/,$(FIGURES))
TABLES := $(addprefix tables/,$(TABLES))

.PHONY: all

all: bootstrap.txt $(FIGURES) $(TABLES) box

bootstrap.txt : packages.R
	Rscript $< &> $@

results/%.Rout : %.R results figs bootstrap.txt
	Rscript $< &> $@

results figs tables :
	mkdir -p $@
	
figs/tree.pdf             : results/Tree.Rout
figs/DAPC-scatterplot.pdf : results/DAPC.Rout
figs/DAPC-barplot.pdf     : figs/DAPC-scatterplot.pdf
figs/MSN.pdf              : results/MSN.Rout
tables/table1.csv         : results/GeneralPopDetails.Rout

.PHONY : box

box : $(FIGURES) $(TABLES)
	rsync -avz --update \
	--exclude '.git' \
	--exclude '.Rproj.user' \
	. \
	~/Box\ Sync/Brazil\ Paper/Scripts/