
ld_append <- function(x) {
  rdf <- lapply(x, jsonld::jsonld_to_rdf)
  rdf <- do.call(paste, rdf)
  out <- jsonld::jsonld_from_rdf(rdf)
  jsonld::jsonld_compact(out)
}


json <- fs::dir_ls("prov") |> ld_append()


# to include only these fields, add:
# "@explicit" : true 
jsonld::jsonld_frame("schema.json",
'{
  "@context": "http://schema.org/",
  "@explicit" : true,
  "name": {},
  "description": {},
  "type": "Dataset",
  "version": {},
  "distribution": {
    "@explicit" : true,
    "description": "output data",
    "encodingFormat": "application/x-parquet",
    "name": {},
    "contentUrl": {}
    
  }
}') |> 
  readr::write_lines("schema.json")

