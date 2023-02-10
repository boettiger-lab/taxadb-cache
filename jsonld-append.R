
ld_append <- function(x) {
  rdf <- lapply(x, jsonld::jsonld_to_rdf)
  rdf <- do.call(paste, rdf)
  jsonld::jsonld_from_rdf(rdf)
}


json <- fs::dir_ls("prov") |> ld_append()


# to include only these fields, add:
# "@explicit" : true 
jsonld::jsonld_frame(json, '{
  "@context": "http://schema.org/",
  "name": {},
  "type": "Dataset",
  "version": {},
  "distribution": {
    "@explicit" : true,
    "description": "output data",
    "encodingFormat": "application/vnd.apache.parquet",
    "name": {},
    "contentUrl": {}
    
  }
}') |> 
  readr::write_lines("schema.json")

