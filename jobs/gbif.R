# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()

dir.create("data")

## GBIF 
rm(list=ls())

in_url <- "https://hosted-datasets.gbif.org/datasets/backbone/2021-11-26/backbone.zip"
download.file(in_url, basename(in_url), method="wget", quiet=TRUE)
id <- contentid::store(basename(in_url))
in_file <- contentid::resolve(id)
in_file <- fs::link_create(in_file, file.path("jobs/data", basename(in_url)))

output_paths = c(dwc = "jobs/data/dwc_gbif.tsv.gz", common = "jobs/data/common_gbif.tsv.gz")
preprocess_gbif(in_file, output_paths = output_paths)


## And publish provenance
code <- c("R/gbif.R","R/helper-routines.R", "jobs/gbif.R")
prov::write_prov(data_in = in_file,
                 #code = code,
                 data_out =  unname(output_paths),
                 title = "GBIF Taxonomic Name Backbone",
                 description = "Darwin Core formatted version of GBIF Taxonomy, created by rOpenSci",
                 license = "http://creativecommons.org/licenses/by/4.0/legalcode",
                 creator = list("type" = "Organization", 
                                name = "GBIF", 
                                id = "https://www.gbif.org",
                                url= "https://www.gbif.org"),
                 version = "21.12",
                 issued = "2021-11-26",
                 url = "https://www.gbif.org",
                 identifier = "https://doi.org/10.15468/39omei",
                 prov="jobs/schema.json", 
                 append=TRUE,
                 schema="http://schema.org")

