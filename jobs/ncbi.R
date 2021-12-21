# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()

dir.create("2021")
rm(list=ls())

in_url <- "https://ftp.ncbi.nih.gov/pub/taxonomy/taxdump_archive/taxdmp_2021-12-01.zip"

id <- contentid::store(in_url)
in_file <- contentid::resolve(id)
# so that in_file has a human-readable name
in_file <- fs::link_create(in_file, file.path("jobs/data", basename(in_url)))

output_paths <- c("data/dwc_ncbi.tsv.gz",
                  "data/common_ncbi.tsv.gz",
                  "data/dwc_ncbi.parquet",
                  "data/common_ncbi.parquet")

preprocess_ncbi(in_file, output_paths)

code <- c("R/ncbi.R", "R/helper-routines.R", "jobs/ncbi.R")
prov::write_prov(data_in = in_file, 
                 #code = code,
                 data_out =  unname(output_paths),
                 title = "NCBI Taxonomic Names",
                 description = "Darwin Core formatted version of NCBI Taxonomy, created by rOpenSci",
                 license = "Public Domain",
                 creator = list("type" = "Organization", name = "NCBI",
                                id = "https://www.ncbi.nlm.nih.gov",
                                url = "https://www.ncbi.nlm.nih.gov"
                                ),
                 version = "21.12",
                 issued = "2021-12-01",
                 url = "https://www.ncbi.nlm.nih.gov/taxonomy",
                 prov="schema.json",
                 append=TRUE, 
                 schema="http://schema.org")
