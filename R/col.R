

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
preprocess_col <- function(in_file = "data/2022_dwca.zip",
                           output_paths = c(dwc = "data/dwc_col.tsv.gz",
                                            common = "data/common_col.tsv.gz",
                                            dwc_parquet = "data/dwc_col.parquet",
                                            common_parquet = "data/common_col.parquet")){

  
  dir = file.path(tempdir(), "col")
  dir.create(dir, FALSE, FALSE)
  unzip(in_file, exdir=dir)

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
  
  # Important note: only accepted+provisionally accepted names are given parentNameUsageID parents!
  # accepted + provisionally accepted are "accepted" in that they don't map to a different acceptedNameUsageID (i.e. have NA instead)
  # better filtered as havin NA in acceptedNameUsageID
  ids <- taxon %>% 
    #filter(is.na(acceptedNameUsageID)) %>%
    select(taxonID, parentNameUsageID)
  
  #( currently a depth of p18 and beyond lead to only empty ranks...)
  recursive_ids <- taxon %>% 
    filter(is.na(acceptedNameUsageID), taxonRank == "species") %>%
    select(taxonID, parentNameUsageID, taxonomicStatus, taxonRank, scientificName) %>%
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
    left_join(rename(ids, p20 = parentNameUsageID), by = c("p19" = "taxonID")) %>%
    rename(p1 = parentNameUsageID)
  
  ## UGH: sometimes the parentNameUsageID references the taxonID corresponding to a name deemed "provisionally accepted" 
  ## while the identical name is listed under a different identifier as "accepted"
  #taxon |> filter(parentNameUsageID == "9CJ5R")
  

  # We will only provide hierarchy terms for `taxonRank==species`, taxonomicStatus=="accepted"

  ## Unpacking the recursive ID map requires two pivots -- memory-intensive operations!
  ## all at species rank level, only accepted names here
  long_hierarchy <-
    recursive_ids %>% 
    tidyr::pivot_longer(starts_with("p"), names_to = "dummy", values_to = "path_id") |>
    select(-dummy) |> distinct() |> # drop extra rows created by extra depth of possible 20 rank levels
    filter(!is.na(path_id)) |>
    select(-taxonomicStatus, -taxonRank) |>
    rename(name = scientificName)
    
  ## Now we get accepted ranks:
  ranks <- taxon %>%
    filter(is.na(acceptedNameUsageID)) %>%  ## i.e., accepted + provisionally accepted
    select(taxonID, taxonRank, taxonomicStatus, scientificName) %>%
    ## even many higher rank names include the authority strings on the name, making string-matching impossible
    ## note at this stage 'scientificName' is a name-rank pair
    mutate(scientificName = stringi::stri_extract_first_words(scientificName))
  
  ## and here we go; this is slow...
  wide <- long_hierarchy %>% 
    left_join(ranks, by = c(path_id = "taxonID")) %>%
    filter(taxonRank %in% c("genus", "family", "order", "class", "phylum", "kingdom")) %>%
    select(-path_id) %>%    # count(taxonID, taxonRank) %>% filter(n>1) # shows duplicates
    group_by(taxonID, taxonRank) %>% slice_head(n=1) %>% # avoid duplicate 'family', e.g. 
    tidyr::spread(taxonRank, scientificName)  # very slow (pivot_wider)
  
  # test that we have no spaces in names
  # sapply(wide, function(str) sum(grepl(" ",str)))
  
  ## If taxonID appears as both accepted and provisionally accepted, keep only former
  dups <- wide |> count(taxonID) |> filter(n > 1) 
  dropme <- dups |> mutate(drop = paste(taxonID, "provisionally accepted"))
  wide_unique <- wide |> 
    mutate(drop = paste(taxonID, taxonomicStatus)) |> 
    anti_join(dropme, by="drop") |> select(-drop)

  ## Now we have the higher classification for all accepted scientific names, we can join it to the 
  ## the core table:
  taxon_wide <- left_join(taxon, select(wide_unique, -name, -taxonomicStatus))
  
  ## Most scientificName strings for `species` are the genus+specificEpithet+authority
  ## As usual, authority abbreviations are wildly non-standard, preventing most string matching.
  ## We replace them with "genus species"
  
  taxon_wide <- taxon_wide %>% 
    mutate(
      genericName = case_when(is.na(genericName) ~ genus,
                              .default= genericName),
      genus = genericName,
      scientificName = case_when(
        taxonRank == "species" ~ paste(genus, specificEpithet), # when taxonRank == species, sci name is binomial 
        !is.na(infraspecificEpithet) ~ infraspecificEpithet     # when infraspecificEpithet is provided, rank is some flavor of subspecies
        ),
      )

  ## if taxonomicStatus is accepted, set acceptedNameUsageID = taxonID (not NA)
  taxon_wide <- taxon_wide %>% 
      mutate(
        acceptedNameUsageID = case_when(
          taxonomicStatus %in% c("accepted", "provisionally accepted") ~ taxonID,
          .default = acceptedNameUsageID
        )
      ) %>% select(taxonID, acceptedNameUsageID, scientificName,
                   taxonomicStatus, taxonRank, 
                   kingdom, phylum, class, order, family, genus, specificEpithet,
                   infraspecificEpithet, cultivarEpithet, datasetID, 
                   namePublishedIn, nameAccordingTo, taxonRemarks,
                   nomenclaturalStatus, nomenclaturalCode,
                   parentNameUsageID, originalNameUsageID,
                   "dcterms:references")
    
                
  
  
  vernacular <- read_tsv(file.path(dir, "VernacularName.tsv"))
  # no prefixes in colnames please
  cols <- colnames(vernacular)
  barenames <- gsub("^\\w+:", "", cols)
  names(vernacular) <- barenames
  #First we create the separate common names table
  comm_table <- vernacular %>%
    inner_join(bind_rows(filter(taxon_wide, taxonomicStatus=="accepted")), by = "taxonID") %>%
    mutate(taxonID = stringi::stri_paste("COL:", taxonID),
           acceptedNameUsageID = stringi::stri_paste("COL:", acceptedNameUsageID)
           )  %>% select(taxonID, language, vernacularName,
                         acceptedNameUsageID, scientificName,
                         taxonomicStatus, taxonRank, 
                         kingdom, phylum, class, order, family, genus, specificEpithet,
                         infraspecificEpithet, cultivarEpithet)

  comm_eng <- vernacular %>%
    filter(language == "eng") %>%
    group_by(taxonID) %>% 
    slice_head(n=1) %>% 
    mutate(vernacularName = tolower(vernacularName)) # Current case use very inconsistent

  
  ## stri_paste respects NAs, avoids "<prefix>:NA"
  dwc <- taxon_wide %>%
    left_join(comm_eng, by = "taxonID") %>%
    mutate(taxonID = stringi::stri_paste("COL:", taxonID),
           acceptedNameUsageID = stringi::stri_paste("COL:", acceptedNameUsageID))


  #tmp <- dwc %>% mutate(vernacularName_clean = taxadb:::clean_names(vernacularName, lowercase = FALSE),
  #                      scientificName_clean = taxadb:::clean_names(scientificName, lowercase = FALSE))
  
  
  message("writing COL Output...\n")

 #readr::write_tsv(dwc, output_paths[["dwc"]])
 #readr::write_tsv(comm_table, output_paths[["common"]])
 
  year <- lubridate::year(Sys.Date())
 arrow::write_dataset(dwc, glue::glue("data/{year}/dwc_col"), max_rows_per_file = 200000L)
 arrow::write_dataset(comm_table, glue::glue("data/{year}/common_col"), max_rows_per_file = 200000L)
 
}


