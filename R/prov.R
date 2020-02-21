
context <- list(
  "dcat" = "http://www.w3.org/ns/dcat#",
  "vocab" = "http://www.w3.org/ns/dcat#",
  "dcat" =  "http://www.w3.org/ns/dcat#",
  "prov" = "http://www.w3.org/ns/prov#",
  "dc" = "http://purl.org/dc/terms/",
  "foaf" = "http://xmlns.com/foaf/0.1/",
  "schema" = "http://schema.org",
  "id" = "@id",
  "type" = "@type"
)

as_triple <- function(l){
  if(is.null(l$id))
    l$id <- paste("urn", "uuid", uuid::UUIDgenerate(), sep = ":")
  rest <- within(l, rm(id))
  tibble::tibble(
    subject = rep(l$id, length(rest)),
    predicate = names(rest),
    object = vapply(unname(rest), as.character, character(1L)),
    type = vapply(rest, function(x) class(x)[[1]], character(1L)) 
  )
}

init_prov_log <- function(dir = rappdirs::user_data_dir("prov")){
  if(!dir.exists(dir)) dir.create(dir, FALSE)
  path <- file.path(dir, "prov.tsv.gz")
  if(!file.exists(path)){
    file.create(path, showWarnings = FALSE)
    r <- data.frame(subject = NA, predicate = NA, object = NA, type = NA)
    readr::write_tsv(r[0,], path)
  }
  path
}

prov_log <- function(entry, dir = rappdirs::user_data_dir("prov")){
  log <- init_prov_log(dir)
  df <- as_triple(compact(entry))
  write_tsv(df, log, append = TRUE)
}

compact <- function (l){
  Filter(Negate(is.null), l)
}
with_prov <- function(fn, url, output_paths, 
                      generatedBy = NULL,
                      dir = rappdirs::user_data_dir("prov")){
  ## register inputs
  input_id <- contenturi::register(url)
  prov_log(list(
    type = "prov:Activity",
    "prov:used" = url,
    "prov:generated" = input_id,
    "prov:endedAtTime"= Sys.time()),
    dir)

  out <- fn(url, output_paths)
## We generated some output content from that input content,
## using a script at a given time
  lapply(output_paths, function(output){
    id <- content_uri(output)
    prov_log(list(
         id = id,
         "dcat:identifier" = id,
         "prov:wasDerivedFrom" = input_id, 
         "prov:wasGeneratedBy" = generatedBy,
         "prov:endedAtTime" = Sys.time(),
         "dct:byteSize" = fs::file_size(output),
         "dct:mediaType" = "text/tab-separated-values",
         "dct:compressionFormat" = "bz2",
         "dct:conformsTo" = "https://dwc.tdwg.org/terms/",
         "pav:curatedBy" = "https://orcid.org/0000-0002-1642-628X"),
         dir)
  })

  out
}




## Don't explicitly type characters as strings, since this is default
xs_class <- function(x){
  type <- switch(class(x)[[1]],
                 "numeric" = "xs:decimal",
                 "factor" = "xs:string",
                 "logical" = "xs:boolean",
                 "integer" = "xs:integer",
                 "Date" = "xs:date",
                 "POSIXct" = "xs:dateTime",
                 NULL
  )
  string <- gsub("^xs:",
                 "http://www.w3.org/2001/XMLSchema#",
                 type)
  ## consistent return length, character(1)
  if (length(string) == 0) {
    string <- as.character(NA)
  }
  string
}


read_prov <- function(dir = rappdirs::user_data_dir("prov")){
  log <- init_prov_log(dir)
  readr::read_tsv(log)
  
}

testfn <- function(url, output_paths){
  download.file(url, output_paths[[1]])
}


