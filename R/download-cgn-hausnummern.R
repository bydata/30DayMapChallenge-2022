library(tidyverse)
library(httr)
library(here)
library(jsonlite)

# Directory to store the data (will be created if it doesn't exist)
data_dir <- here("data", "cologne-hausnummern")
if (!dir.exists(data_dir)) dir.create(data_dir)

# Download the data for a single batch of 1000 records at at time
download_data <- function(offset, data_dir, file_prefix = "hausnummern_") {
  url <- sprintf("https://geoportal.stadt-koeln.de/arcgis/rest/services/Statistische_Daten/OSM_Hausnummern/MapServer/0/query?where=objectid+is+not+null&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=4326&returnIdsOnly=false&returnCountOnly=false&orderByFields=OBJECTID&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=%d&resultRecordCount=&f=geojson", offset)
  dest_file <- file.path(data_dir, sprintf("%s%d.json" ,file_prefix, offset))
  download.file(url, destfile = dest_file)
  print(offset)
  Sys.sleep(1)
}

# Download all records
offsets <- seq(0, 2e5, 1000)
walk(offsets, download_data, data_dir = data_dir)
