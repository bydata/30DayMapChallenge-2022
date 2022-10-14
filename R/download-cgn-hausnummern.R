library(tidyverse)
library(httr)
library(here)
library(jsonlite)

url <- "https://geoportal.stadt-koeln.de/arcgis/rest/services/Statistische_Daten/OSM_Hausnummern/MapServer/0/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&returnIdsOnly=false&returnCountOnly=false&orderByFields=OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson"
foo <- GET(url)
data_dir <- here("data", "cologne-hausnummern")
if (!dir.exists(data_dir)) dir.create(data_dir)

download.file(url, destfile = "foo.json")

download_data <- function(offset, data_dir, file_prefix = "hausnummern_") {
  url <- sprintf("https://geoportal.stadt-koeln.de/arcgis/rest/services/Statistische_Daten/OSM_Hausnummern/MapServer/0/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&returnIdsOnly=false&returnCountOnly=false&orderByFields=OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=%d&resultRecordCount=&f=geojson", offset)
  dest_file <- file.path(data_dir, sprintf("%s%d.json" ,file_prefix, offset))
  download.file(url, destfile = dest_file)
  print(offset)
  Sys.sleep(1)
}

offsets <- seq(0, 2e5, 1000)
download_data(100000, data_dir = data_dir)
walk(offsets, download_data, data_dir = data_dir)

