# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()


in_url <- "http://files.opentreeoflife.org/ott/ott3.3/ott3.3.tgz"
id <- contentid::store(in_url)
in_file <- contentid::resolve(id)

tbls <- preprocess_ott(in_file)

readr::write_tsv(tbls$dwc, "data/dwc_ott.tsv.gz")
arrow::write_parquet(dwc, "data/dwc_ott.parquet")


#readr::write_tsv(comm_table, "data/common_ott.tsv.gz")
#arrow::write_parquet(comm_table, "data/common_ott.parquet")

output_paths = c(dwc = "data/dwc_ott.tsv.gz", "data/dwc_ott.parquet")


code <- c("R/ott.R","R/helper-routines.R", "jobs/ott.R")
prov::write_prov(data_in = in_file,
                 #code = code, 
                 data_out =  unname(output_paths),
                 title = "OpenTree Taxonomy",
                 description = "Darwin Core formatted version of Open Tree of Life Taxonomic Names, created by rOpenSci",
                 license = "https://creativecommons.org/publicdomain/zero/1.0/legalcode",
                 url = "https://files.opentreeoflife.org/ott",
                 creator = list("type" = "Organization", name = "Open Tree Of Life", url = "https://opentreeoflife.org"),
                 version = "21.12",
                 issued = "2021-06-01",
                 prov="schema.json", append=TRUE, schema="http://schema.org")

