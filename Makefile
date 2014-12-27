XLSXS = $(wildcard upstream/*.xlsx)
CSVS = $(patsubst %.xlsx,%.csv,$(subst upstream/,csv/,$(XLSXS)))
RDAS = R/data/hesa2014.rda R/data/ref2014.rda
VERSION = $(shell grep Version DESCRIPTION | sed 's/^.*: //')

all: $(RDAS)

csv/%.csv: upstream/%.xlsx
	libreoffice -env:UserInstallation=file://$(PWD)/tmp/ --headless --convert-to csv --outdir csv $<

$(RDAS): $(CSVS)
	Rscript ingest.R

