# These are all of our targets
FIGURES := figs/tree.pdf \
           figs/DAPC-scatterplot.pdf \
           figs/DAPC-barplot.pdf \
           figs/MSN.pdf \
           figs/MCG-bar.pdf
TABLES  := tables/table1.csv \
           tables/table2.csv
FOLDERS := results \
           figs \
           tables 
           
# Master command. Type make and this will run. 
.PHONY: all

all: $(FOLDERS) bootstrap.txt $(FIGURES) $(TABLES) box

# All of the targets should depend on successful execution of the R scripts
figs/tree.pdf             : results/Tree.Rout
figs/DAPC-scatterplot.pdf : results/DAPC.Rout
figs/DAPC-barplot.pdf     : results/DAPC.Rout
figs/MSN.pdf              : results/MSN.Rout
figs/MCG-bar.pdf          : results/GeneralPopDetails.Rout
tables/table1.csv         : results/GeneralPopDetails.Rout
tables/table2.csv         : results/AMOVA.Rout

# bootstrap.txt keeps a record of the installed packages
# Note that &> means to redirect the stdout and stderr to the specified file.
bootstrap.txt : install.R data/data.csv
	Rscript $< &> $@

# R scripts executed here
results/%.Rout : %.R bootstrap.txt
	Rscript $< &> $@

# Output folders created here
$(FOLDERS) :
	mkdir -p $@

# Rule for copying everything to box
.PHONY : box

box : $(FIGURES) $(TABLES)
	rsync -avz --update \
	--exclude bootstrap.txt \
	--exclude '.git' \
	--exclude '.Rproj.user' \
	. \
	~/Box\ Sync/Brazil\ Paper/Scripts/

# Rule for removing everything that was generated
.PHONY : clean

clean :
	$(RM) -r $(FOLDERS) bootstrap.txt