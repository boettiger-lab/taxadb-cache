# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()

## NCBI

# See ftp://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdump_archive/
# taxdump.zip is apparently the same as the old format, not the new format!

in_url <- "ftp://ftp.ncbi.nlm.nih.gov/pub/taxonomy/taxdump_archive/taxdmp_2020-09-01.zip"
in_file <- file.path("2020", basename(in_url))
curl::curl_download(in_url,  in_file)
code <- c("R/ncbi.R", "R/helper-routines.R", "make.R")
output_paths <- c(dwc = "2020/dwc_ncbi.tsv.gz",
                  common = "2020/common_ncbi.tsv.gz")

preprocess_ncbi(in_url, output_paths)

source(system.file("examples/minio_store.R", package="prov"))
minio_store(c(in_file, code, output_paths))
prov::write_prov(data_in = in_file, code = code, data_out =  unname(output_paths), prov="prov.json", append=TRUE)



## COL ## 
rm(list=ls())

in_url <- "http://www.catalogueoflife.org/DCA_Export/zip-fixed/2020-09-01-archive-complete.zip"
#in_url <- paste0("http://www.catalogueoflife.org/DCA_Export/zip-fixed/", 2020, "-annual.zip")
in_file <- "2020/col-2020.zip"
curl::curl_download(in_url, in_file)
code <- c("R/col.R","R/helper-routines.R", "make.R")
output_paths <- c(dwc = "2020/dwc_col.tsv.gz",
                  common = "2020/common_col.tsv.gz")

preprocess_col(url = in_url, output_paths = output_paths)

source(system.file("examples/minio_store.R", package="prov"))
minio_store(c(in_file, code, output_paths))
prov::write_prov(data_in = in_file, code = code, data_out =  unname(output_paths), prov="prov.json", append=TRUE)


### OTT ## 
rm(list=ls())

in_url <- "http://files.opentreeoflife.org/ott/ott3.2/ott3.2.tgz"
in_file <- "2020/ott3.2.tgz"
dir.create(dirname(in_file))
curl::curl_download(in_url, in_file)
code <- c("R/ott.R","R/helper-routines.R", "make.R")
output_paths = c(dwc = "2020/dwc_ott.tsv.gz")

preprocess_ott(url = in_url, output_paths)

## And publish provenance
source(system.file("examples/minio_store.R", package="prov"))
minio_store(c(in_file, code, output_paths))
prov::write_prov(data_in = in_file, code = code, data_out =  unname(output_paths), prov="prov.json", append=TRUE)



## GBIF 
rm(list=ls())

in_url <- "http://rs.gbif.org/datasets/backbone/backbone-current.zip"
in_file <- "2020/gbif-backbone-current.zip"
curl::curl_download(in_url, in_file)
code <- c("R/gbif.R","R/helper-routines.R", "make.R")

output_paths = c(dwc = "2020/dwc_gbif.tsv.gz", common = "2020/common_gbif.tsv.gz")
preprocess_gbif(url = in_url, output_paths = output_paths)


## And publish provenance
source(system.file("examples/minio_store.R", package="prov"))
minio_store(c(in_file, code, output_paths))
prov::write_prov(data_in = in_file, code = code, data_out =  unname(output_paths), prov="prov.json", append=TRUE)


## ITIS
rm(list=ls())

in_url <-  "https://www.itis.gov/downloads/itisSqlite.zip"
in_file <- "2020/itisSqlite.zip"
curl::curl_download(in_url, in_file)

output_paths = c(dwc = "2020/dwc_itis.tsv.gz", common = "2020/common_itis.tsv.gz")
code <- c("R/itis.R","R/helper-routines.R", "make.R")

preprocess_itis(in_url, output_paths)
  
## And publish provenance
source(system.file("examples/minio_store.R", package="prov"))
minio_store(c(in_file, code, output_paths))
prov::write_prov(data_in = in_file, code = code, data_out =  unname(output_paths), prov="prov.json", append=TRUE)







  
  
  
# vroom::vroom("2019/common_ncbi.tsv.bz2") %>% vroom::vroom_write("2019/common_ncbi.tsv.gz")
#setwd("2019")
#piggyback::pb_upload("dwc_ncbi.tsv.gz", repo="boettiger-lab/taxadb-cache", tag = "2019")
#piggyback::pb_upload("common_ncbi.tsv.gz", repo="boettiger-lab/taxadb-cache", tag = "2019")





