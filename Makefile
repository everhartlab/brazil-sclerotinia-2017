TARGETS := tree.pdf DAPC-scatterplot.pdf DAPC-barplot.pdf MSN.pdf
TARGETS := $(addprefix figs/,$(TARGETS))

.PHONY: all

all: bootstrap.txt $(TARGETS) box

bootstrap.txt : packages.R
	Rscript $< &> $@

results/%.Rout : %.R results figs bootstrap.txt
	Rscript $< &> $@

results figs :
	mkdir -p $@
	
figs/tree.pdf             : results/Tree.Rout
figs/DAPC-scatterplot.pdf : results/DAPC.Rout
figs/DAPC-barplot.pdf     : figs/DAPC-scatterplot.pdf
figs/MSN.pdf              : results/MSN.Rout

.PHONY : box

box : $(TARGETS)
	rsync -avz --update \
	--exclude '.git' \
	--exclude '.Rproj.user' \
	. \
	~/Box\ Sync/Brazil\ Paper/Scripts/