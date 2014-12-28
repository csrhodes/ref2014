XLSXS = $(wildcard upstream/*.xlsx)
CSVS = $(patsubst %.xlsx,%.csv,$(subst upstream/,csv/,$(XLSXS)))
RDAS = R/data/hesa2014.rda R/data/ref2014.rda
VERSION = $(shell grep Version R/DESCRIPTION | sed 's/^.*: //')
PKG = ref2014_$(VERSION).tar.gz

all: $(PKG)

csv/%.csv: upstream/%.xlsx
	libreoffice -env:UserInstallation=file://$(PWD)/tmp/ --headless --convert-to csv --outdir csv $<

$(RDAS): $(CSVS)
	Rscript ingest.R

$(PKG): $(RDAS)
	R CMD build R

check: $(PKG)
	R CMD check $(PKG)
