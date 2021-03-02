#data loader
library(rgdal)
library(janitor)

today <- format(Sys.time(), '%y%m%d')

download.file("https://data.melbourne.vic.gov.au/api/views/gh7s-qda8/rows.csv?accessType=DOWNLOAD",
              paste0("./../data/Development_Activity_Monitor_", today,".csv"))

# unzip
for (file in list.files(path = "./../data", pattern="*.zip")){
  print(paste("./../data/", file, sep = ""))
  unzip(zipfile = paste("./../data/", file, sep = ""), exdir = "./../data", overwrite = TRUE)
}

for (file in list.files(path = "./../data", pattern="*.shp")){
  print(file)
  assign(paste0("raw_", gsub(".shp", "",file)), readOGR(dsn = paste("./../data/", file, sep = "")))
}

for (file in list.files(path = "./../data", pattern="*.csv")){
  print(paste("./../data/", file, sep = ""))
  assign(paste0("raw_", gsub(".csv", "",file)), read.csv(paste("./../data/", file, sep = "")))
}
