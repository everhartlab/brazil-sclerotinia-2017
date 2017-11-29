FIGURES := tree.pdf DAPC-scatterplot.pdf DAPC-barplot.pdf MSN.pdf MCG-bar.pdf
TABLES  := table1.csv
FIGURES := $(addprefix figs/,$(FIGURES))
TABLES  := $(addprefix tables/,$(TABLES))
FOLDERS := results \
           figs \
           tables 
           

.PHONY: all

all: $(FOLDERS) bootstrap.txt $(FIGURES) $(TABLES) box

figs/tree.pdf             : results/Tree.Rout
figs/DAPC-scatterplot.pdf : results/DAPC.Rout
figs/DAPC-barplot.pdf     : results/DAPC.Rout
figs/MSN.pdf              : results/MSN.Rout
tables/table1.csv         : results/GeneralPopDetails.Rout
figs/MCG-bar.pdf          : results/GeneralPopDetails.Rout

bootstrap.txt : packages.R data/data.csv
	Rscript $< &> $@

results/%.Rout : %.R bootstrap.txt
	Rscript $< &> $@

$(FOLDERS) :
	mkdir -p $@

.PHONY : box

box : $(FIGURES) $(TABLES)
	rsync -avz --update \
	--exclude '.git' \
	--exclude '.Rproj.user' \
	. \
	~/Box\ Sync/Brazil\ Paper/Scripts/
	
.PHONY : clean

clean :
	$(RM) -r $(FOLDERS)