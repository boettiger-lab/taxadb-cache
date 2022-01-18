library(contentid)
col.parquet <- resolve("hash://sha256/74f95ca0fb0049f322d2b827b96e6552abd138b7b43312409d688bb9c12f07af", store=TRUE)
df <- arrow::read_parquet(col.parquet)

fs::dir_delete("dwc_col/")


write_parquet_multi <- function(df, path){

  ## aim for 40 MB objects or 10-MB parquet parts
  partitions <- as.integer( round( lobstr::obj_size(df) / (40*1e6)  ))
  
  n_rows <- nrow(df)
  ## arrow serialize parts
  df %>% 
    dplyr::mutate(partition = rep_len(1:partitions, n_rows)) %>% 
    #dplyr::group_by(partition) %>%
    arrow::write_dataset(path, partitioning = "partition", hive_style = FALSE)

}
  
fs::dir_info("dwc_col/", recurse=TRUE, type = "file")

parquet <- file.path("dwc_col", "*/*")
conn <- DBI::dbConnect(duckdb::duckdb())
tblname <- "dwc_col"
  query <- paste0("CREATE VIEW '", tblname,
                  "' AS SELECT * FROM parquet_scan('",
                  parquet, "');")
  DBI::dbSendQuery(conn, query)

tbl(conn, "dwc_col")
