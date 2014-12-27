XLSXS = $(wildcard ../upstream/*.xlsx)

all: $(patsubst %.xlsx,%.csv,$(subst ../upstream/,,$(XLSXS)))

%.csv: ../upstream/%.xlsx
	libreoffice -env:UserInstallation=file://$(PWD)/tmp/ --headless --convert-to csv $<
