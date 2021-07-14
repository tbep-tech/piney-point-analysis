library(httr)
library(jsonlite)
library(tidyverse)

# up to April -------------------------------------------------------------

url <- 'https://www.ncei.noaa.gov/archive/archive-management-system/OAS/bin/prd/jquery/download/120767.5.5.tar.gz'

loc1 <- tempfile()
loc2 <- tempfile()

download.file(url, destfile = loc1)
untar(loc1, exdir = loc2)
fls <- list.files(loc2, recursive = T, full.names = T)
fls <- grep('\\.csv$', fls, value = T)
histdat <- read.csv(fls)

# from API - recent -------------------------------------------------------

path <- 'https://gis.ncdc.noaa.gov/arcgis/rest/services/ms/HABSOS_CellCounts/MapServer/0/query?'

request <- GET(
  url = path,
  query= list(       
    # where = "STATE_ID='FL'",
    where = "LATITUDE<28.2 AND LATITUDE > 27 AND LONGITUDE > -83.4 AND LONGITUDE < -82.08",
    outFields = 'DESCRIPTION,SAMPLE_DATE,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    # outFields = 'DESCRIPTION,SAMPLE_DATE,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT',
    f = 'pjson'
  )
)

response <- content(request, as = "text", encoding = "UTF-8")
results <- fromJSON(response, flatten = T)
dat <- results$features
names(dat) <- gsub('^attributes\\.', '', names(dat))

tmp <- dat %>% 
  filter(DESCRIPTION %in% 'Indian Shores Beach')

# what is the date??