library(httr)
library(jsonlite)
library(tidyverse)
library(tbeptools)
library(extrafont)
library(sf)
library(lubridate)

loadfonts(device = 'win', quiet = T)

data(segmask)

# from API - recent -------------------------------------------------------

# query api 
path <- 'https://gis.ncdc.noaa.gov/arcgis/rest/services/ms/HABSOS_CellCounts/MapServer/0/query?'

request <- GET(
  url = path,
  query= list(       
    # where = "STATE_ID='FL'",
    where = "LATITUDE < 28.2 AND LATITUDE > 27 AND LONGITUDE > -83.4 AND LONGITUDE < -82.08",
    outFields = 'DESCRIPTION,SAMPLE_DATE,LATITUDE,LONGITUDE,SALINITY,SALINITY_UNIT,WATER_TEMP,WATER_TEMP_UNIT,GENUS,SPECIES,CATEGORY,CELLCOUNT,CELLCOUNT_UNIT',
    f = 'pjson'
  )
)

response <- content(request, as = "text", encoding = "UTF-8")
results <- fromJSON(response, flatten = T)

# format data
dat <- results$features %>% 
  rename_all(function(x) gsub('^attributes\\.', '', x)) %>% 
  rename_all(tolower) %>% 
  mutate(
    date = format(sample_date, scientific = F),
    date = as.numeric(gsub('000$', '', date)), 
    date = as.POSIXct(date, origin = c('1970-01-01'), tz = 'UTC'), 
    date = as.Date(date)
  ) %>% 
  select(
    description, date, sal_ppt = salinity, temp_c = water_temp, kb_cell_l = cellcount, category, longitude, latitude
  ) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  .[tbshed, ] %>%
  .[segmask, ]

# MTB subset
toplo <- dat %>%
  .[tbseg[3, ], ] %>% 
  mutate(
    dtgrp = quarter(date), 
    yr = year(date), 
    kb_cell_l = kb_cell_l / 1e5
  ) %>% 
  # filter(dtgrp %in% c(2, 3)) %>% 
  st_set_geometry(NULL) %>%
  # filter(year(date) >= 1990) %>% 
  group_by(yr) %>% 
  summarise(
    cnt = n(), 
    minv = quantile(kb_cell_l, prob = 0.1, na.rm = T),
    medv = quantile(kb_cell_l, prob = 0.5, na.rm = T),
    maxv = quantile(kb_cell_l, prob = 0.9, na.rm = T),
    .groups = 'drop'
  )

# plot
p <- ggplot(toplo, aes(x = factor(yr))) + 
  geom_crossbar(aes(ymin = minv, y = medv, ymax = maxv), width = 0.75, fill = '#00806E', fatten = 1) +
  theme_minimal() + 
  theme(
    axis.title.x = element_blank(), 
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = 'Cells (10000 / L)', 
    title = expression(paste(italic('K. brevis'), ' concentrations in Middle Tampa Bay')), 
    subtitle = paste('Bars are 10h/90th percentile with median, n =', sum(toplo$cnt))', 
    caption = 'Data from FWC/NOAA HABSOS; https://www.ncei.noaa.gov/maps/habsos/maps.htm\nPlot created by TBEP'
  )

png('figure/kbrevis.png', family = 'Lato', height = 4, width = 8, res = 300, units = 'in')
print(p)
dev.off()
