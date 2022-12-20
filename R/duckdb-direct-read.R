library(duckdb)
library(DBI)
library(glue)
library(dplyr)

year <- lubridate::year(Sys.Date())
db <- "dwc_col"

base <- "https://minio.thelio.carlboettiger.info/taxadb"
base <- "https://github.com/boettiger-lab/taxadb-cache/raw/master/data"
repo <- glue::glue(base, "/{year}/{db}")

# https locations need full paths, no globbing
urls <- paste(repo, paste0("part-", 0:19, ".parquet"), sep="/")


conn <- DBI::dbConnect(duckdb(), ":memory:",
                       config=list("memory_limit"="12GB",
                                   "temp_directory" = "/tmp"))
DBI::dbExecute(conn, "INSTALL 'httpfs';")
DBI::dbExecute(conn, "LOAD 'httpfs';")

parquet <- glue::glue("['", paste(urls, collapse="', '"), "']")
tblname <- glue::glue(db, "_", year)
view_query <-glue("CREATE VIEW '{tblname}' ",
                  "AS SELECT * FROM parquet_scan({parquet});")

bench::bench_time( DBI::dbSendQuery(conn, view_query))
bench::bench_time( col <- tbl(conn, tblname) )
bench::bench_time( print(col) )

bench::bench_time( 
print(col |> filter(scientificName == "Homo sapiens"))
)

## Let duckdb create a local copy from the remote connection
local <- "col.parquet"
query <- glue::glue("COPY (SELECT * FROM {tblname}) TO '{local}' (FORMAT 'parquet')")
bench::bench_time( DBI::dbSendQuery(conn, query))


## Much faster now to query the local
tblname <- glue::glue(db, "_", year, "_local")
view_query <-glue("CREATE VIEW '{tblname}' ",
                  "AS SELECT * FROM parquet_scan({local});")


bench::bench_time( DBI::dbSendQuery(conn, view_query))
bench::bench_time( col <- tbl(conn, tblname) )
bench::bench_time( print(col) )
