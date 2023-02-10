# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()

## GBIF 
rm(list=ls())

in_url <- "https://hosted-datasets.gbif.org/datasets/backbone/2022-11-23/backbone.zip"
id <- contentid::store(in_url)

## sometimes gbif has download issues
#tmp <- tempfile(fileext = "zip")
#download.file(in_url, tmp, method="wget", quiet=TRUE)
#id <- contentid::store(tmp)

in_file <- contentid::resolve(id)
path <- file.path("data", basename(in_url))
if(!link_exists(path))
  path <- fs::link_create(in_file, path)


output_paths = c(dwc = "data/dwc_gbif.tsv.gz", common = "data/common_gbif.tsv.gz")

# hash-based memoizer for file-based workflow
has_id <- FALSE
if (fs::file_exists("gbif_schema.json")) {
  #prov <- jsonlite::read_json("schema.json")
  prov <- readLines("gbif_schema.json")
  has_id <- any(grepl(id, prov))
}

if (!has_id) {
  gbif <- preprocess_gbif(path)
}


output_paths <- c(fs::dir_ls("data/2022/dwc_gbif", recurse = TRUE),
                  fs::dir_ls("data/2022/common_gbif", recurse = TRUE)) |> unname()
output_urls <- paste0("https://github.com/boettiger-lab/taxadb-cache/raw/master/", 
                      output_paths)


## And publish provenance
#code <- c("R/gbif.R","R/helper-routines.R", "jobs/gbif.R")
prov::write_prov(#data_in = in_url,
                # code = code,
                 data_out =  output_urls,
                 title = "gbif",
                 description = "Darwin Core formatted version of GBIF Taxonomic Names Backbone, created by rOpenSci",
                 license = "http://creativecommons.org/licenses/by/4.0/legalcode",
                 creator = list("type" = "Organization", 
                                name = "GBIF", 
                                id = "https://www.gbif.org",
                                url= "https://www.gbif.org"),
                 version = "22.12",
                 dateCreated = "2022-11-26",
                 url = "https://www.gbif.org",
                 identifier = "https://doi.org/10.15468/39omei",
                 prov="gbif_schema.json", 
                 schema="http://schema.org")



