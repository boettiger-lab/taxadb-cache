# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()
in_url <- "https://ftp.ncbi.nih.gov/pub/taxonomy/taxdump_archive/taxdmp_2022-12-01.zip"

id <- contentid::store(in_url)
in_file <- contentid::resolve(id)
# so that in_file has a human-readable name
path <- file.path("data", basename(in_url))
if (!link_exists(path)) {
  path <- fs::link_create(in_file, path)
}

output_paths <- c("data/dwc_ncbi.tsv.gz",
                  "data/common_ncbi.tsv.gz",
                  "data/dwc_ncbi.parquet",
                  "data/common_ncbi.parquet")

# hash-based memoizer for file-based workflow
has_id <- FALSE
if (fs::file_exists("ncbi_schema.json")) {
  #prov <- jsonlite::read_json("schema.json")
  prov <- readLines("ncbi_schema.json")
  has_id <- any(grepl(id, prov))
}

if (!has_id) {
  ncbi <- preprocess_ncbi(path, output_paths)
}



#code <- c("R/ncbi.R", "R/helper-routines.R", "jobs/ncbi.R")



prov::write_prov(data_out = paste0("https://github.com/boettiger-lab/taxadb-cache/raw/master/", 
                                   fs::dir_ls("data/2022/dwc_ncbi", recurse = TRUE)),
                 title = "v22.12_dwc_ncbi",
                 description = "Darwin Core formatted version of NCBI Taxonomic Names, created by rOpenSci",
                 license = "Public Domain",
                 creator = list("type" = "Organization", name = "NCBI",
                                id = "https://www.ncbi.nlm.nih.gov",
                                url = "https://www.ncbi.nlm.nih.gov"
                                ),
                 version = "22.12",
                 issued = "2022-12-01",
                 url = "https://www.ncbi.nlm.nih.gov/taxonomy",
                 prov="schema.json",
                 append = TRUE,
                 schema="http://schema.org")


prov::write_prov(data_out = paste0("https://github.com/boettiger-lab/taxadb-cache/raw/master/", 
                                   fs::dir_ls("data/2022/common_ncbi", recurse = TRUE)),
                 title = "v22.12_common_ncbi",
                 description = "Common Names table derived from NCBI Taxonomic Names, created by rOpenSci",
                 license = "Public Domain",
                 creator = list("type" = "Organization", name = "NCBI",
                                id = "https://www.ncbi.nlm.nih.gov",
                                url = "https://www.ncbi.nlm.nih.gov"
                 ),
                 version = "22.12",
                 issued = "2022-12-01",
                 url = "https://www.ncbi.nlm.nih.gov/taxonomy",
                 prov="schema.json",
                 append = TRUE,
                 schema="http://schema.org")



