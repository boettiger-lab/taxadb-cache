# remotes::install_github("cboettig/prov")
library(prov)
library(tidyverse)
library(Hmisc)
devtools::load_all()


dwc <- redlist()
write_csv(dwc, "data/dwc_iucn.tsv.gz")
