

#' preprocess_col
#'
#' @param url Source of Catalogue of Life
#' @param output_paths paths where output will be written: must be two paths, one named dwc, one named common
#' @param dir working directory for downloads, will be tempdir() by default
#'
#' @export
#' @import stringi forcats readr dplyr
#' @importFrom methods className
#' @importFrom stats family setNames
#' @importFrom utils download.file untar unzip
#' @details NOTE: A list of  all snapshots available from: http://www.catalogueoflife.org/DCA_Export/archive.php
preprocess_col <- function(archive = "data/2022_dwca.zip",
                           output_paths = c(dwc = "data/dwc_col.tsv.gz",
                                            common = "data/common_col.tsv.gz",
                                            dwc_parquet = "data/dwc_col.parquet",
                                            common_parquet = "data/common_col.parquet")){

  
  dir = file.path(tempdir(), "col")
  dir.create(dir, FALSE, FALSE)
  unzip(archive, exdir=dir)

  ## a better read_tsv
  read_tsv <- function(...) readr::read_tsv(..., quote = "",
                                            col_types = readr::cols(.default = "c"))

  taxon <- read_tsv(file.path(dir, "Taxon.tsv"))
  
  # no prefixes in colnames please
  cols <- colnames(taxon)
  barenames <- gsub("^dwc:", "", cols)
  names(taxon) <- barenames
  
  ## Now the hard part.  
  ## We use recursive tree descent by parentTaxa to determine kingdom, phylum, class, order & family. 
  ids <- taxon %>% filter(taxonomicStatus=="accepted") %>% select(taxonID, parentNameUsageID)
  
  #( currently a depth of p18 and beyond lead to empty ranks...)
  recursive_ids <- taxon %>% select(taxonID, parentNameUsageID, taxonomicStatus, taxonRank, scientificName) %>%
    filter(taxonomicStatus=="accepted", taxonRank == "species") %>%
    left_join(rename(ids, p2 = parentNameUsageID), by = c("parentNameUsageID" = "taxonID")) %>%
    left_join(rename(ids, p3 = parentNameUsageID), by = c("p2" = "taxonID")) %>%
    left_join(rename(ids, p4 = parentNameUsageID), by = c("p3" = "taxonID")) %>%
    left_join(rename(ids, p5 = parentNameUsageID), by = c("p4" = "taxonID")) %>%
    left_join(rename(ids, p6 = parentNameUsageID), by = c("p5" = "taxonID")) %>%
    left_join(rename(ids, p7 = parentNameUsageID), by = c("p6" = "taxonID")) %>%
    left_join(rename(ids, p8 = parentNameUsageID), by = c("p7" = "taxonID")) %>%
    left_join(rename(ids, p9 = parentNameUsageID), by = c("p8" = "taxonID")) %>%
    left_join(rename(ids, p10 = parentNameUsageID), by = c("p9" = "taxonID")) %>%
    left_join(rename(ids, p11 = parentNameUsageID), by = c("p10" = "taxonID")) %>%
    left_join(rename(ids, p12 = parentNameUsageID), by = c("p11" = "taxonID")) %>%
    left_join(rename(ids, p13 = parentNameUsageID), by = c("p12" = "taxonID")) %>%
    left_join(rename(ids, p14 = parentNameUsageID), by = c("p13" = "taxonID")) %>%
    left_join(rename(ids, p15 = parentNameUsageID), by = c("p14" = "taxonID")) %>%
    left_join(rename(ids, p16 = parentNameUsageID), by = c("p15" = "taxonID")) %>%
    left_join(rename(ids, p17 = parentNameUsageID), by = c("p16" = "taxonID")) %>%
    left_join(rename(ids, p18 = parentNameUsageID), by = c("p17" = "taxonID")) %>%
    left_join(rename(ids, p19 = parentNameUsageID), by = c("p18" = "taxonID")) %>%
    left_join(rename(ids, p20 = parentNameUsageID), by = c("p19" = "taxonID"))
  
  # We will only provide hierarchy terms for `taxonRank==species`, taxonomicStatus=="accepted"

  ## Unpacking the recursive ID map requires two pivots -- memory-intensive operations!  
  long_hierarchy <-
    recursive_ids %>%
    tidyr::pivot_longer(starts_with("p"), names_to = "dummy", values_to = "path_id")
  
  ## Now we get accepted ranks:
  ranks <- taxon %>%
    filter(taxonomicStatus == "accepted") %>%
    select(taxonID, taxonRank, scientificName) %>%
    ## even many higher rank names include the authority strings on the name, making string-matching impossible
    mutate(scientificName = stringi::stri_extract_first_words(scientificName))
  ## and here we go; this is slow...
  wide <- long_hierarchy %>% 
    select(id = "taxonID", path_id) %>%
    distinct() %>%  
    left_join(ranks, by = c(path_id = "taxonID")) %>%
    filter(taxonRank %in% c("genus", "family", "order", "class", "phylum", "kingdom")) %>%
    select(-path_id) %>%    # count(id, taxonRank) %>% filter(n>1) # shows duplicates
    group_by(id, taxonRank) %>% slice_head(n=1) %>% # avoid duplicate 'family'. #slow 
    tidyr::spread(taxonRank, scientificName)  # very slow
  
  # test that we have no spaces in names
  # sapply(wide, function(str) sum(grepl(" ",str)))
  
  ## Now we have the higher classification for all accepted scientific names, we can join it to the 
  ## the core table:
  taxon <- left_join(taxon, wide, by = c("taxonID" = "id"))
  
  ## Most scientificName strings for `species` are the genus+specificEpithet+authority
  ## As usual, authority abbreviations are wildly non-standard, preventing most string matching.
  ## We replace them with "genus species"
  
  taxon <- taxon %>% 
    filter(taxonRank=="species") %>% 
    mutate(
      scientificName = case_when(
        taxonRank == "species" ~ paste(genus, specificEpithet), # when taxonRank == species, sci name is binomial 
        !is.na(infraspecificEpithet) ~ infraspecificEpithet     # when infraspecificEpithet is provided, rank is some flavor of subspecies
        )
      )

  ## if taxonomicStatus is accepted, set acceptedNameUsageID = taxonID (not NA)
    taxon <- taxon %>% 
      mutate(
        acceptedNameUsageID = case_when(
          taxonomicStatus %in% c("accepted", "provisionally accepted") ~ taxonID
        )
      )

  taxon <- taxon %>% select(taxonID, acceptedNameUsageID, taxonomicStatus,taxonRank, scientificName,
                   kingdom, phylum, class, order, family, genus, specificEpithet, infraspecificEpithet,
                   namePublishedIn, nameAccordingTo, taxonRemarks)
    
  vernacular <- read_tsv(file.path(dir, "VernacularName.tsv"))
  # no prefixes in colnames please
  cols <- colnames(vernacular)
  barenames <- gsub("^\\w+:", "", cols)
  names(vernacular) <- barenames
  #First we create the separate common names table
  comm_table <- vernacular %>%
    inner_join(bind_rows(filter(taxon, taxonomicStatus=="accepted")), by = "taxonID") %>%
    mutate(taxonID = stringi::stri_paste("COL:", taxonID),
           acceptedNameUsageID = stringi::stri_paste("COL:", acceptedNameUsageID)
           )

  comm_eng <- vernacular %>%
    filter(language == "eng") %>%
    group_by(taxonID) %>% 
    slice_head(n=1) %>% 
    mutate(vernacularName = tolower(vernacularName)) # Current case use very inconsistent

  
  ## stri_paste respects NAs, avoids "<prefix>:NA"
  dwc <- taxon %>%
    left_join(comm_eng, by = "taxonID") %>%
    mutate(taxonID = stringi::stri_paste("COL:", taxonID),
           acceptedNameUsageID = stringi::stri_paste("COL:", acceptedNameUsageID))


  #tmp <- dwc %>% mutate(vernacularName_clean = taxadb:::clean_names(vernacularName, lowercase = FALSE),
  #                      scientificName_clean = taxadb:::clean_names(scientificName, lowercase = FALSE))
  
  
  message("writing COL Output...\n")

 readr::write_tsv(dwc, output_paths[["dwc"]])
 readr::write_tsv(comm_table, output_paths[["common"]])
 
  year <- lubridate::year(Sys.Date())
 arrow::write_dataset(dwc, glue::glue("data/{year}/dwc_col"), max_rows_per_file = 200000L)
 arrow::write_dataset(comm_table, glue::glue("data/{year}/common"), max_rows_per_file = 200000L)
 
}


