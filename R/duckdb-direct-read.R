library(duckdb)
library(DBI)
library(glue)
library(dplyr)

# http-based locations need full paths, no globbing
base <- "https://github.com/boettiger-lab/taxadb-cache/raw/master/data"
repo <- glue::glue(base, "/{year}/{db}", year="2022", db = "dwc_col")
urls <- paste(repo, paste0("part-", 0:19, ".parquet"), sep="/")


## or we can allow contentid to resolve identifiers from metadata
resolve_ids <- function(sdo_json) {
  ## equivalent to:
  # df <- prov::parse_sdo("col_schema.json")
  sdo <- jsonlite::read_json(sdo_json)
  meta <- sdo$`@graph`[[1]]
  shas <- purrr::map_chr(meta$distribution, "id")
  format <- purrr::map_chr(meta$distribution, "encodingFormat")
  ids <- shas[grepl("parquet",format)]
  paths <- purrr::map_chr(ids, contentid::resolve, store=TRUE)
  paths
}


duckdb_con <- function() {
  conn <- getOption("duckdb_mem")
  if(inherits(conn, "duckdb_connection")) return(conn)
  conn <- DBI::dbConnect(duckdb::duckdb(), ":memory:",
                         config=list("temp_directory" = tempdir()))
  DBI::dbExecute(conn, "INSTALL 'httpfs';")
  DBI::dbExecute(conn, "LOAD 'httpfs';")
  options("duckdb_mem" = conn)
  conn
}

duckdb_view <- function(urls,
                        conn = duckdb_con(),  
                        tblname = paste0("z",rlang::hash(urls))) {
  parquet <- glue::glue("['", paste(urls, collapse="', '"), "']")
  view_query <-glue::glue("CREATE VIEW '{tblname}' ",
                          "AS SELECT * FROM parquet_scan({parquet});")
  DBI::dbSendQuery(conn, view_query)
}

duckdb_tbl <- function(urls, tblname = paste0("z",rlang::hash(urls))) {
  conn <- duckdb_con()
  if(!(tblname %in% DBI::dbListTables(conn))) {
    duckdb_view(urls, conn, tblname)
  }
  dplyr::tbl(conn, tblname)
}

paths <- resolve_ids("col_schema.json")
col <- duckdb_tbl(paths)

bench::bench_time( 
print(col |> filter(scientificName == "Homo sapiens"))
)

