# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
library(arrow)
devtools::load_all()


dwc <- redlist()


write_csv(dwc, "data/dwc_iucn.tsv.gz")
arrow::write_parquet("data/dwc_icun.parquet")
code = c("R/redlist.R", "jobs/iucn.R")
output_paths = c("data/dwc_iucn.tsv.gz","data/dwc_iucn.parquet")

prov::write_prov(code = code, 
                 data_out =  unname(output_paths),
                 title = "IUCN Redlist Taxonomy",
                 description = "Darwin Core formatted version of IUCN Redlist Taxonomic Names, created by rOpenSci",
                 license = "https://www.iucnredlist.org/terms/terms-of-use#2.%20Copyrights_Ownership",
                 url = "https://www.iucnredlist.org",
                 creator = list("type" = "Organization",
                                name = "IUCN Redlist",
                                url = "https://www.iucnredlist.org",
                                id = "https://www.iucnredlist.org"),
                 version = "21.12",
                 issued = Sys.Date(),
                 prov="schema.json",
                 append=TRUE,
                 schema="http://schema.org")

