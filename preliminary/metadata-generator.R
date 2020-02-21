library(openssl)
library(fs)
library(tidyverse)
library(here)
library(jsonlite)
library(contenturi) # remotes::install_github("cboettig/contenturi@drop-type")
library(piggyback)



remotes <- piggyback::pb_list("boettiger-lab/taxadb-cache", tag = "2019")
remote_urls <- piggyback::pb_download_url(remotes$file_name)

#remotes$file_name %>% piggyback::pb_download("boettiger-lab/taxadb-cache", tag = "2019")


## register published URLs to a remote registry
ids <- sapply(remote_urls, contenturi::register, registries = "https://hash-archive.org")
## register to a local registry and the local content store
##ids2 <- sapply(remote_urls, contenturi::store)


tag <- "2019"


## DCAT2 terms, http://www.w3.org/ns/dcat#
meta <- fs::dir_info(tag) %>%
  select(path, 
         "dct:byteSize" = size,
         "dct:issued" = modification_time
         ) %>%
  mutate("dct:identifier" = contenturi::content_uri(path),
         "dct:title" = fs::path_file(path),
         "dct:mediaType" = "text/tab-separated-values",
         "dct:compressionFormat" = "bz2",
         "dct:conformsTo" = "https://dwc.tdwg.org/terms/",
         "pav:curatedBy" = "https://orcid.org/0000-0002-1642-628X") %>%
  select(-path)


col_meta <- list("dct:title": "Taxonomic names from the Catalogue of Life",
                 "dct:accessRights": "http://www.catalogueoflife.org/content/terms-use",
                 "prov:hasPrimarySource": "http://www.catalogueoflife.org/DCA_Export/zip-fixed/2019-annual.zip",
                 )

shared_meta <- list("dct:mediaType" = "text/tab-separated-values",
                    "dct:compressionFormat" = "bz2",
                    "dct:conformsTo" = "https://dwc.tdwg.org/terms/",
                    "pav:curatedBy" = "https://orcid.org/0000-0002-1642-628X")


## schema:filesSize is a text field with units!
#fileSize = trimws(as.character(size))

## Lightweight file metadata
write_csv(meta, file.path(tag,"meta.csv"))
meta %>%
  write_json(file.path(tag, "meta.json"),
             pretty = TRUE, auto_unbox=TRUE)



#######

library(EML)


dwc_terms <-
  read_csv("https://github.com/tdwg/dwc/raw/master/vocabulary/term_versions.csv") %>%
  select(attributeName = label, attributeDefinition = definition, definition = term_iri) %>%
  distinct() %>%
  bind_rows(tibble(  #
    attributeName = "isExtinct",
    definition = "logical indicating whether this taxon is now extinct",
    attributeDefinition = definition))


path <- meta$path[[1]]


meta_dataset <- function(path){

  sha256 <- file_hash(path, openssl::sha256, raw = TRUE)
  info <- fs::file_info(path)

  attributeName = path %>% read_tsv(n_max = 1) %>% colnames()
  attrs <- tibble(attributeName = attributeName) %>% left_join(dwc_terms)

  set_attributes(attrs, )
  physical <- set_physical(path)


  dataTable <- eml$dataTable(
    entityName = basename(path),
    entityDescription = "List of recognized taxonomic names in the Darwin Core format",
    physical = physical,
    attributeList = attrList)

}


me <- list(individualName = list(givenName = "Carl",
                                 surName = "Boettiger"),
           electronicMailAddress = "cboettig@berkeley.edu",
           id = "http://orcid.org/0000-0002-1642-628X")
kari <- list(individualName = list(givenName = "Kari",
                                 surName = "Norman"),
           electronicMailAddress = "cboettig@gmail.com",
           id = "http://orcid.org/0000-0002-1642-628X")

dataset = eml$dataset(
  title = "",
  creator = me,
  contact = list(references="http://orcid.org/0000-0002-1642-628X"),
  pubDate = Sys.Date(),
  intellectualRights = "",
  abstract =  "",
  dataTable = dataTable,
  #keywordSet = keywordSet,
 # coverage = coverage,
 # methods = methods
)


