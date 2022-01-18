library(jsonlite)
library(contentid)
library(fs)
library(piggyback)
library(magrittr)
tag <- "21.12"

## Upload files to a redundant location (e.g. GitHub via piggyback)
setwd("data")
files <- fs::dir_ls(".", recurse = TRUE, type = "file") |> fs::path_rel()
piggyback::pb_upload(files, repo = "boettiger-lab/taxadb-cache", tag=tag)
locations <- piggyback::pb_download_url(repo = "boettiger-lab/taxadb-cache", tag=tag)
ids <- contentid::register(locations, "https://hash-archive.carlboettiger.info")                                      
ids <- contentid::register(locations[is.na(ids)], "https://hash-archive.carlboettiger.info")                                      

ids <- contentid::register(locations, "https://hash-archive.org")                                      


