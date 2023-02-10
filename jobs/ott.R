# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()


in_url <- "http://files.opentreeoflife.org/ott/ott3.4/ott3.4.tgz"
id <- contentid::store(in_url)
in_file <- contentid::resolve(id)
path <- file.path("data", basename(in_url))
if(!link_exists(path))
  path <- fs::link_create(in_file, path)



# hash-based memoizer for file-based workflow
has_id <- FALSE
if (fs::file_exists("ott_schema.json")) {
  #prov <- jsonlite::read_json("schema.json")
  prov <- readLines("ott_schema.json")
  has_id <- any(grepl(id, prov))
}

if(!has_id) {
  tbls <- preprocess_ott(path)
}

output_paths = list(dwc = "data/dwc_ott.tsv.gz",
                 dwc_parquet =  "data/dwc_ott.parquet")
readr::write_tsv(tbls$dwc, output_paths$dwc)
arrow::write_parquet(tbls$dwc, output_paths$dwc_parquet)


#readr::write_tsv(comm_table, "data/common_ott.tsv.gz")
#arrow::write_parquet(comm_table, "data/common_ott.parquet")



#code <- c("R/ott.R","R/helper-routines.R", "jobs/ott.R")

output_paths <- c(fs::dir_ls("data/2022/dwc_ott", recurse = TRUE)) |> unname()
output_urls <- paste0("https://github.com/boettiger-lab/taxadb-cache/raw/master/", 
                      output_paths)
prov::write_prov(#data_in = path,
                 #code = code, 
                 data_out = output_urls,
                 title = "ott",
                 description = "Darwin Core formatted version of Open Tree of Life Taxonomic Names, created by rOpenSci",
                 license = "https://creativecommons.org/publicdomain/zero/1.0/legalcode",
                 url = "https://files.opentreeoflife.org/ott",
                 creator = list("type" = "Organization",
                                name = "Open Tree Of Life",
                                url = "https://opentreeoflife.org",
                                id = "https://opentreeoflife.org"),
                 version = "22.12",
                 issued = "2022-12-01",
                 prov="prov/ott_schema.json",
                 append=FALSE,
                 schema="http://schema.org")

