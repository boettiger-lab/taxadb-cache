library(jsonlite)
library(contentid)
library(fs)
library(piggyback)


Sys.setenv(CONTENTID_HOME="~/taxacache")
setwd(Sys.getenv("CONTENTID_HOME"))

## Get identifiers from PROV record and download corresponding files
x <- unlist(jsonlite::read_json("https://raw.githubusercontent.com/boettiger-lab/taxadb-cache/master/prov.json"), recursive = TRUE, use.names = FALSE)
ids <- unique(x[grepl("hash://sha256/.*", x)])
lapply(ids, contentid::resolve, store=TRUE)

## Start here if files are local-only so far.

## Upload files to a redundant location (e.g. GitHub via piggyback)
## note: hard to get download_url back from `pins`
files <- fs::dir_ls(Sys.getenv("CONTENTID_HOME"), recurse = TRUE, type = "file")
files <- fs::path_rel(files)
piggyback::pb_upload(files, repo = "boettiger-lab/taxadb-cache", tag="contentid")
locations <- piggyback::pb_download_url(repo = "boettiger-lab/taxadb-cache", tag="contentid")

## register the new locations
for(x in locations){
  message(basename(x))
  contentid::register(x, registries = c("https://hash-archive.org"))
  contentid::register(x, registries = c("https://hash-archive.thelio.carlboettiger.info"))
}
