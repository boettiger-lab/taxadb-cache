init_lmdb <- function(path) {
  if (!requireNamespace("thor", quietly = TRUE)){
    stop("Please install package `thor` to use LMDB backend")
  }
  mdb_env <- getExportedValue("thor", "mdb_env")
  mdb_env(path, mapsize = 1e12) ## ~1 TB
}


lmdb_read <- function(db, id, ...) {
  out <- db$mget(id, FALSE)
  lmdb_parse(out, ...)
}


## parse text string back into a data.frame
lmdb_parse <- function(x, col.names, colClasses = NA){
  ## consider a faster parser than read.table!
  read.table(text = paste0(x, collapse="\n"),
             header = FALSE, sep = "\t",
             quote = "",  colClasses = colClasses,
             col.names = col.names)
}


dwc_cols <- function() c("taxonID", "scientificName", "acceptedNameUsageID",
               "taxonomicStatus", "taxonRank", "kingdom", "phylum",
               "class", "order", "family", "genus", "specificEpithet",
               "infraspecificEpithet", "vernacularName")


lmdb_importer <- function(df, db){
  
  sciname <- df %>%
    group_by(scientificName) %>%
    summarise(x = paste(taxonID, scientificName, acceptedNameUsageID,
                        taxonomicStatus, taxonRank, kingdom, phylum,
                        class, order, family, genus, specificEpithet,
                        infraspecificEpithet, vernacularName,
                        sep="\t", collapse = "\n"))
  db$mput(key = sciname[[1]], value = sciname[[2]])
  rm(sciname)
  
  id <- df %>% mutate(x = paste(taxonID, scientificName, acceptedNameUsageID,
                                taxonomicStatus, taxonRank, kingdom, phylum,
                                class, order, family, genus, specificEpithet,
                                infraspecificEpithet, vernacularName,
                                sep="\t" )) %>% select(taxonID, x)
  db$mput(key = id[[1]], value = id[[2]])
  rm(id)
  
}
create_lmdb <- function(provider){
  
  
  db <- lmdb_init(provider)
  lambda <- function(chunk) lmdb_importer(chunk, db)
  arkdb::process_chunks(file, lambda, lines=5e4L, ...)

}

#' @examples 
#' ids <- c("COL:35517330", "COL:35517332", "COL:35517329", "COL:35517325")
#' sp <- c("Dendrocygna autumnalis", "Dendrocygna bicolor")

lmdb_name <- function(name, provider, version = latest_version()){
  
  db <- init_lmdb(provider)
  col.names <- c("taxonID", "scientificName", "acceptedNameUsageID",
                 "taxonomicStatus", "taxonRank", "kingdom", "phylum",
                 "class", "order", "family", "genus", "specificEpithet",
                 "infraspecificEpithet", "vernacularName")
  lmdb_read(db, name, col.names, colClasses = "character")
  
}

lmdb_id <- lmdb_name


