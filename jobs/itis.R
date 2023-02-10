# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()

in_url <-  "https://itis.gov/downloads/itisSqlite.zip"
id <- contentid::store(in_url)
in_file <- contentid::resolve(id)
path <- file.path("data", basename(in_url))
if (!link_exists(path)) {
  path <- fs::link_create(in_file, path)
}
output_paths = c(dwc = "data/dwc_itis.tsv.gz",
                 common = "data/common_itis.tsv.gz",
                 dwc_parquet = "data/dwc_itis.parquet",
                 common_parquet = "data/common_itis.parquet"
)

has_id <- FALSE
if (fs::file_exists("schema.json")) {
  #prov <- jsonlite::read_json("schema.json")
  prov <- readLines("schema.json")
  has_id <- any(grepl(id, prov))
}



if (!has_id) {
preprocess_itis(path, output_paths)
}
## And publish provenance
#code <- c("R/itis.R","R/helper-routines.R", "jobs/itis.R")


prov::write_prov(#data_in = in_url,
    #code = code, 
    data_out =  paste0("https://github.com/boettiger-lab/taxadb-cache/raw/master/", 
                       fs::dir_ls("data/2022/dwc_itis", recurse = TRUE)),
    title = "v22.12_dwc_itis",
    description = "Darwin Core formatted version of Integrated Taxonomic Information System (ITIS), created by rOpenSci",
    license = "https://creativecommons.org/publicdomain/zero/1.0/legalcode",
    creator = list("type" = "Organization", 
                   name = "ITIS",
                   url = "https://itis.gov/",
                   id = "https://itis.gov/"),
    identifier = "https://doi.org/10.5066/F7KH0KBK",
    version = "22.12",
    issued = "2022-12-01",
    url = "https://itis.gov",
    prov="schema.json",
    schema="http://schema.org",
    append= TRUE)

prov::write_prov(#data_in = in_url,
                 #code = code, 
                 data_out =  paste0("https://github.com/boettiger-lab/taxadb-cache/raw/master/", 
                                    fs::dir_ls("data/2022/common_itis", recurse = TRUE)),
                 title = "v22.12_common_itis",
                 description = "Common Names from the Integrated Taxonomic Information System (ITIS), created by rOpenSci",
                 license = "https://creativecommons.org/publicdomain/zero/1.0/legalcode",
                 creator = list("type" = "Organization", 
                                name = "ITIS",
                                url = "https://itis.gov/",
                                id = "https://itis.gov/"),
                 identifier = "https://doi.org/10.5066/F7KH0KBK",
                 version = "22.12",
                 issued = "2022-12-01",
                 url = "https://itis.gov",
                 prov="schema.json",
                 schema="http://schema.org",
                 append= TRUE)


