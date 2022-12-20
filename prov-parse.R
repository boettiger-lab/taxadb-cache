

sdo <- jsonlite::read_json("col_schema.json")
graph <- sdo$`@graph`
type <- prov:::lookup(graph, "type")
dd <- df <- data.frame()
if ("Dataset" %in% type) {
  i <- which(type == "Dataset")
  dataset <- graph[[i]] ## assumes 1 Dataset
  types <- prov:::lookup(dataset$distribution, "type")
  datadownload <- dataset$distribution[types == "DataDownload"]
  df <- prov:::parseDataDownload(datadownload)
  df$version <- graph$version
  df$name <- dataset$name
  df
}
if ("DataDownload" %in% type) {
  datadownload <- graph[type == "DataDownload"]
  dd <- parseDataDownload(datadownload)
}
rbind(df, dd)