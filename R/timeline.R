library(tidyverse)
library(extrafont)
library(lubridate)
library(patchwork)
library(here)
library(sf)
library(tbeptools)
library(ggmap)
library(units)

loadfonts(device = 'win', quiet = T)

data(kbrdat)
data(rswqdat)
data(rsstatloc)
data(bswqdat)

##
# fish kill data

# data from https://public.myfwc.com/fwri/FishKillReport/searchresults.aspx
fishdat <- read.csv(here('data-raw/FishKillResultReport-lng.csv')) %>% 
  select(
    date = textBox6, 
    county = tEMPDataTextBox,
    city = cOUNTYDataTextBox, 
    waterbody = lOCATIONDataTextBox,
    species = textBox18
  ) %>% 
  mutate(
    date = mdy(date),
    yr = year(date),
    week = floor_date(date, unit = 'week'), 
    week = factor(format(week, '%b %d')), 
    week = factor(week, levels = as.character(unique(week))), 
    county = case_when(
      county %in% c('Pinellas ', 'Pinellas', 'pinellas') ~ 'Pinellas', 
      county %in% c('Hillsborough', 'Hillsborough ') ~ 'Hillsborough', 
      T ~ county
    ),
    city = gsub('\\s+$', '', city),
    city = gsub('^St\\s', 'St. ', city),
    city = case_when(
      city %in% c('St. pete Beach', 'St. Pete Beach', 'St. Petersburg Beach') ~ 'St. Petersburg Beach', 
      city %in% 'Tierra Ceia' ~ 'Terra Ceia', 
      city %in% 'dunedin' ~ 'Dunedin',
      T ~ city
    )
  )

# levels for week 
weeklv <- c("Apr 04", "Apr 11", "Apr 18", "Apr 25", "May 02", "May 09", "May 16", 
            "May 23", "May 30", "Jun 06", "Jun 13", "Jun 20", "Jun 27", "Jul 04", 
            "Jul 11", "Jul 18")

# k brevis timeline -------------------------------------------------------

# MTB subset
toplo <- kbrdat %>%
  .[tbseg[tbseg$bay_segment == 'MTB', ], ] %>%
  filter(var == 'kb') %>% 
  filter(year(date) >= 2021) %>%
  mutate(
    week = floor_date(date, unit = 'week'),
    week = factor(format(week, '%b %d')), 
    week = factor(week, levels = weeklv)
  ) %>%
  st_set_geometry(NULL) %>%
  group_by(week) %>%
  summarise(
    cnt = n(),
    y0 = min(val, na.rm = T), 
    y10 = quantile(val, prob = 0.1, na.rm = T),
    y50 = quantile(val, prob = 0.5, na.rm = T),
    y90 = quantile(val, prob = 0.9, na.rm = T),
    y100 = max(val, na.rm = T),
    .groups = 'drop'
  ) %>%
  complete(week) %>% 
  filter(week != 'Jul 18')

# plot
p <- ggplot(toplo, aes(x = week)) +
  geom_boxplot(
    aes(ymin = y0, lower = y10, middle = y50, upper = y90, ymax = y100),
    stat = "identity", width = 0.75, fill = '#00806E'
  ) +
  scale_y_log10(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  theme_minimal(base_size = 14) +
  theme(
    # axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, size = 12, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = 'Cells (100k / L)',
    x = 'Week of',
    title = expression(paste(italic('K. brevis'), ' concentrations in Middle Tampa Bay')),
    subtitle = 'Boxplots show min, 10th %tile, median, 90th %tile, and max',
    caption = 'Data from FWC/NOAA HABSOS'
  )

png(here('figure/kbrtimeline.png'), height = 4, width = 6, units = 'in', res = 200, family = 'Lato')
print(p)
dev.off()

# fish kill reports -------------------------------------------------------

toplo <- fishdat %>% 
  filter(city %in% c('Tampa', 'St. Petersburg')) %>% 
  filter(yr >= 2021) %>%  
  group_by(week, city) %>% 
  summarise(
    cnt = n(), 
    .groups = 'drop'
  ) %>% 
  mutate(
    week = factor(week, levels = weeklv)
  ) %>% 
  complete(week) %>% 
  filter(week != 'Jul 18')

p <- ggplot(toplo, aes(x = week, fill = city, y = cnt)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    title = 'Reported fish kills by city',
    x = 'Week of',
    y = 'Counts',
    caption = 'Counts are from FWRI Fish Kill Database, attributed to Red Tide'
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_minimal(base_size = 14, base_family = 'Lato') + 
  theme(
    axis.ticks.x = element_line(),
    # axis.title.x = element_blank(), 
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    legend.title = element_blank(), 
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )

png(here('figure/fiskilltimeline.png'), height = 4, width = 6, units = 'in', res = 200, family = 'Lato')
print(p)
dev.off()

# nutrients time line -----------------------------------------------------

# combine tbeptools stations with pp station
stat1 <- st_as_sf(stations, coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  mutate(station = as.character(epchc_station)) %>% 
  select(station)  
stat2 <- rsstatloc %>% 
  select(station)
stat <- bind_rows(stat1, stat2) %>% 
  arrange(station) %>% 
  filter(!duplicated(station))

bsdat <- bswqdat %>% 
  select(station, date, var, uni, val)

wqdat <- rswqdat %>% 
  filter(source %in% c('epchc', 'fldep')) %>% 
  bind_rows(bsdat) %>% 
  select(station, date, var, uni, val) %>%
  filter(var %in% 'tn') %>% 
  mutate(
    uni = 'mgl'
  ) %>% 
  inner_join(stat, ., by = 'station') %>% 
  .[tbseg[tbseg$bay_segment %in% 'MTB', ], ] %>% 
  select(date, station, date, var, uni, val)

bsrng <- wqdat %>% 
  st_set_geometry(NULL) %>% 
  filter(year(date) < 2021 & year(date) > 2010) %>% 
  mutate(
    # week = floor_date(date, unit = 'week'),
    # week = factor(format(week, '%b %d')), 
    # week = factor(week, levels = weeklv), 
    mo = month(date) 
  ) %>% 
  filter(mo %in% c(4:6)) %>% 
  pull(val) %>% 
  median(na.rm = T)
  summarise(
    tnmed = median(val, na.rm = T), 
    .groups = 'drop'
  ) %>% 
  filter(mo %in% c(4:6))

jns <- tibble(
    week = paste0('2021 ', weeklv)
  ) %>% 
  mutate(
    week = ymd(week), 
    mo = month(week)
  ) %>% 
  inner_join(bsrng, by = 'mo') %>% 
  mutate(
    week = factor(format(week, '%b %d')),
    week = factor(week, levels = weeklv)
  )
  
# MTB subset
toplo <- wqdat %>% 
  filter(year(date) >= 2021) %>%
  mutate(
    week = floor_date(date, unit = 'week'),
    week = factor(format(week, '%b %d')), 
    week = factor(week, levels = weeklv)
  ) %>%
  filter(!is.na(week)) %>% 
  st_set_geometry(NULL) %>%
  group_by(week) %>%
  summarise(
    cnt = n(),
    y0 = min(val, na.rm = T), 
    y10 = quantile(val, prob = 0.1, na.rm = T),
    y50 = quantile(val, prob = 0.5, na.rm = T),
    y90 = quantile(val, prob = 0.9, na.rm = T),
    y100 = max(val, na.rm = T),
    .groups = 'drop'
  ) %>%
  complete(week) %>% 
  filter(!week %in% c('Jul 04', 'Jul 11', 'Jul 18'))

# plot
p <- ggplot(toplo, aes(x = week)) +
  geom_boxplot(
    aes(ymin = y0, lower = y10, middle = y50, upper = y90, ymax = y100),
    stat = "identity", width = 0.75, fill = '#958984'
  ) +
  geom_hline(yintercept = bsrng, size = 1.5, color = 'tomato1') + 
  geom_text(x = 12.5, y = bsrng - 0.03, label = 'baseline', color = 'tomato1') + 
  # scale_y_log10(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  theme_minimal(base_size = 14) +
  theme(
    # axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, size = 12, hjust = 1),
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = 'Concentration (mg/L)',
    x = 'Week of',
    title = 'Nitrogen concentrations in Middle Tampa Bay',
    subtitle = 'Boxplots show min, 10th %tile, median, 90th %tile, and max',
    caption = 'View all data at https://shiny.tbep.org/piney-point'
  )


png(here('figure/nutrientstimeline.png'), height = 4, width = 6, units = 'in', res = 200, family = 'Lato')
print(p)
dev.off()


# nutrients map -----------------------------------------------------------

library(ggplot2)
library(tidyr)
library(gganimate)
library(sf)
library(lubridate)
library(dplyr)
library(here)
library(magick)
library(ggmap)
library(units)

load(file = here('data/rswqdat.RData'))
load(file = here('data/rsstatloc.RData'))

# nonbay stations
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# colors 
vrscols <- rev(RColorBrewer::brewer.pal(n = 9, name = 'Spectral'))

# gif dims
dm <- 500

# gif frame
frms <- 50

# ggplot base size
bssz <- 15

# combine water quality data with locations
wqdat <- rswqdat %>% 
  filter(var %in% c('chla', 'tn')) %>% 
  filter(!station %in% nonbay) %>% 
  inner_join(rsstatloc, ., by = 'station') %>% 
  mutate(
    lng = st_coordinates(.)[, 1], 
    lat = st_coordinates(.)[, 2],
    dategrp = floor_date(date, unit = 'week'), 
    inrng = case_when(
      inrng %in% c('below', 'in range') ~ 'in range', 
      T ~ inrng
    )
  ) %>% 
  select(station, date, dategrp, var, val, lat, lng, inrng) %>% 
  group_by(dategrp) %>% 
  filter(length(unique(var)) == 2) %>% # filter dates where both chl and tn are available
  ungroup %>% 
  arrange(var, dategrp)

# reference data for ggsn, MUST have geometry named column
dat_ext <- wqdat %>% 
  st_bbox %>% 
  st_as_sfc %>% 
  st_buffer(dist = set_units(0.01, degree)) %>%
  st_bbox %>% 
  unname

# stamen base map
bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 11)

# change opacity of basemap
mapatt <- attributes(bsmap1)
bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                         alpha.f = 0.1), 
                             nrow = nrow(bsmap1))
attributes(bsmap1_transparent) <- mapatt

# base map
bsmap <- ggmap(bsmap1_transparent)

