# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
library(fs)
devtools::load_all()

in_url <- "https://download.checklistbank.org/col/annual/2022_dwca.zip"
#in_url <- paste0("http://www.catalogueoflife.org/DCA_Export/zip-fixed/", 2021, "-annual.zip")
id <- contentid::store(in_url)
in_file <- contentid::resolve(id)
path <- file.path("data", basename(in_url))
if(!fs::link_exists(path))
  path <- fs::link_create(in_file, path)




# hash-based memoizer for file-based workflow
has_id <- FALSE
if (fs::file_exists("col_schema.json")) {
  #prov <- jsonlite::read_json("schema.json")
  prov <- readLines("col_schema.json")
  has_id <- any(grepl(id, prov))
}

if (!has_id) {
  preprocess_col(path)
}

output_paths <- fs::dir_ls("data/2022/dwc_col/")
code <- c("R/col.R")
prov::write_prov(data_in = path,
                 code = code, 
                 data_out =  unname(output_paths),
                 title = "Catalogue Of Life Taxonomic Names",
                 description = "Darwin Core formatted version of Catalogue Of Life Taxonomic Names, created by rOpenSci",
                 license = "http://creativecommons.org/licenses/by/4.0/",
                 identifier = "https://doi.org/10.48580/dfq8",
                 url = "https://www.catalogueoflife.org/",
                 creator = list("type" = "Organization", 
                                name = "Catalogue Of Life",
                                url = "https://www.catalogueoflife.org/",
                                id = "https://www.catalogueoflife.org/"),
                 version = "22",
                 issued = Sys.Date(),
                 prov="col_schema.json",
                 schema="http://schema.org")



prov::write_prov(data_in = path,
                 code = code, 
                 data_out =  unname(output_paths),
                 title = "Catalogue Of Life Taxonomic Names",
                 description = "Darwin Core formatted version of Catalogue Of Life Taxonomic Names, created by rOpenSci",
                 license = "http://creativecommons.org/licenses/by/4.0/",
                 identifier = "https://doi.org/10.48580/dfq8",
                 creator = list("type" = "Organization", 
                                name = "Catalogue Of Life",
                                url = "https://www.catalogueoflife.org/",
                                id = "https://www.catalogueoflife.org/"),
                 version = "22",
                 issued = Sys.Date(),
                 prov="col_schema.json",
                 schema="http://schema.org",
                 append=FALSE)


