
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
  dwc <- full %>% select(taxonId = taxonid, kingdom = kingdom_name, phylum = phylum_name,
                               class = class_name, order = order_name, family = family_name,
                               genus = genus_name, specificEpithet = species_name, 
                               scientificName = scientific_name) %>%
    mutate_if(is.character, sentence_case) %>%
    mutate(id = paste0("IUCN:", id))
  
  dwc

}