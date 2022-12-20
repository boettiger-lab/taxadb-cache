
redlist <- function(){
  
  ## Public token from Redlist API website examples:
  key <- "9bb4facb6d23f48efbf424bb05c0c1ef1cf6f468393bc745d42179ac4aca5fee"
  
  ## How many pages of records?
  x <- httr::GET(paste0("http://apiv3.iucnredlist.org/api/v3/speciescount?token=",key))
  max <- ceiling(as.numeric(httr::content(x)$count) / 10000) - 1
  
  ## Get em all
  links <- paste0("http://apiv3.iucnredlist.org/api/v3/species/page/",
                  0:max,
                  "?token=", key)
  system.time({
    
    full <- links %>%
      purrr::map_df(function(link){
        httr::GET(link) %>%
          httr::content() %>%
          getElement("result") %>%
          purrr::map_df(function(x){
            x %>% purrr::flatten() %>% as.tibble()
          })
      })
  })
  
  
  sentence_case <- function(x) {
    Hmisc::capitalize(str_to_lower(x))
    #gsub("(. )([A-Z])(.+)", "\\1\\U\\2\\L\\3", x)
  }

  
  dwc <- full %>% mutate(category = as.factor(category)) %>%
    mutate(across(where(is.character), sentence_case)) %>%
    mutate(taxonid = paste0("IUCN:", taxonid),
           acceptedNameUsageId = taxonid,
           specificEpithet = stringi::stri_split_boundaries(scientific_name, simplify = TRUE)[,2],
           taxonomicStatus = "accepted") %>%
    select(taxonId = taxonid, kingdom = kingdom_name, phylum = phylum_name,
           class = class_name, order = order_name, family = family_name,
           genus = genus_name, specificEpithet, infraspecificEpithet = infra_name,
           scientificName = scientific_name,
           vernacularName = main_common_name,
           nameAccordingTo = taxonomic_authority,
           acceptedNameUsageId,
           population,
           category)
  ## population is not a dwc term; but IUCN taxonIds are unique to population, not necessarily to subspecies
  ## category is obviously not a dwc term, but seems valuable/convenient enough to be retained
  dwc

  
  
  year <- lubridate::year(Sys.Date())
  arrow::write_dataset(dwc, glue::glue("data/{year}/dwc_redlist"), max_rows_per_file = 200000L)

  
  
}