XLSXS = $(wildcard upstream/*.xlsx)
CSVS = $(patsubst %.xlsx,%.csv,$(subst upstream/,csv/,$(XLSXS)))
RDAS = R/data/hesa2014.rda R/data/ref2014.rda
RDS = R/man/hesa2014.Rd R/man/ref2014.Rd
RNWS = R/vignettes/join.Rnw
VERSION = $(shell grep Version R/DESCRIPTION | sed 's/^.*: //')
PKG = ref2014_$(VERSION).tar.gz

all: $(PKG)

csv/%.csv: upstream/%.xlsx
	libreoffice -env:UserInstallation=file://$(PWD)/tmp/ --headless --convert-to csv --outdir csv $<

$(RDAS): $(CSVS)
	Rscript ingest.R

$(PKG): $(RDAS) $(RDS) $(RNWS)
	R CMD build R

check: $(PKG)
	R CMD check $(PKG)
