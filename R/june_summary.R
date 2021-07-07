library(tidyverse)
library(lubridate)
library(here)
library(extrafont)
library(sf)
library(googlesheets4)
library(googledrive)
library(sf)
library(ggmap)
library(units)
library(ggspatial)

# deauth all so it can build with gh actions
drive_deauth()
gs4_deauth()

data(rstrnwts)
data(rstrnpts)
data(rstrndat)
data(bstransect)

# fish kills --------------------------------------------------------------

fishdat <- read.csv(here('data-raw/FishKillResultReport.csv')) %>% 
  select(
    date = textBox6, 
    county = tEMPDataTextBox,
    location = cOUNTYDataTextBox, 
    species = textBox18
  ) %>% 
  mutate(
    date = mdy(date),
    week = floor_date(date, unit = 'week'), 
    week = factor(format(week, '%b %d')), 
    week = factor(week, levels = as.character(unique(week)))
  )

p <- ggplot(fishdat, aes(x = week, fill = county)) + 
  geom_bar(stat = 'count', colour = 'darkgrey') + 
  labs(
    x = 'Week of', 
    y = 'No. of fish kill reports',
    caption = 'Counts are from FWRI Fish Kill Database, attributed to Red Tide'
    ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_minimal(base_size = 16, base_family = 'Lato') + 
  theme(
    axis.ticks.x = element_line(), 
    legend.title = element_blank(), 
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )

png(here('figure/fishkills.png'), family = 'Lato', height = 4, width = 6, units = 'in', res = 300)
print(p)
dev.off()

# macroalgae --------------------------------------------------------------

rssub <- rstrndat %>% 
  filter(typ == 'mcr') %>% 
  select(station, location, date, taxa, bb)

bssub <- bstransect %>% 
  filter(var == 'Abundance') %>% 
  select(
    station = Transect, 
    location = Site, 
    date = Date,
    taxa = Savspecies,
    bb = aveval
  ) %>% 
  filter(date %in% rstrnwts$date & station %in% rstrnwts$station & location %in% rstrnwts$location) %>% 
  filter(grepl('^DA', taxa)) %>% 
  mutate(
    location = as.numeric(location), 
    taxa = case_when(
      grepl('Red', taxa) ~ 'Red', 
      grepl('Green', taxa) ~ 'Green', 
      grepl('Macroalgae', taxa) ~ 'Red',
      T ~ taxa# verified that the location/date/transect match with weights was red, only one instance
    )
  ) 

tojn <- bind_rows(rssub, bssub)

wtssub <- rstrnwts %>% 
  select(-genus) %>% 
  rename(taxa = group) %>% 
  filter(!grepl('and', taxa)) %>% 
  left_join(tojn, by = c('station', 'date', 'location', 'taxa')) %>% 
  mutate(
    weight = weight_g * 0.004 # g / 0.25m2 to kg / m2
  )

wtsmod <- wtssub %>% 
  group_by(taxa) %>% 
  nest() %>% 
  mutate(
    mod = purrr::map(data, lm, formula = weight ~ 0 + bb)
  ) %>% 
  select(taxa, mod)

wtsest <- rstrndat %>% 
  filter(taxa %in% c('Red', 'Green', 'Cyanobacteria')) %>% 
  group_by(taxa) %>% 
  nest %>% 
  left_join(wtsmod, by = 'taxa') %>% 
  mutate(
    weight_kgm2 = purrr::pmap(list(object = mod, newdata = data), predict)
  ) %>% 
  select(-mod) %>% 
  unnest(c('data', 'weight_kgm2')) %>% 
  mutate(
    taxa = factor(taxa, levels = c('Red', 'Green', 'Cyanobacteria'), labels = c('Red', 'Green', 'Lyngbya'))
  )

mcrsel <- c("Red", "Green", "Brown", "Lyngbya")
mcrcol <- c('tomato1', 'lightgreen', 'burlywood3', 'lightblue')
names(mcrcol) <- mcrsel
mcrcol <- mcrcol[as.character(unique(wtsest$taxa))]

# summarize
wtssum <- wtsest %>% 
  unite('datetrn', date, station, remove = F) %>% 
  mutate(
    dateflr = floor_date(date, unit = 'month')
  ) %>% 
  group_by(dateflr, taxa) %>% 
  summarize(
    kgest = sum(weight_kgm2) / 4 / length(unique(datetrn)), 
    kgest = kgest * 0.8
  )

p <- ggplot(wtssum, aes(x = dateflr, y = kgest, fill = taxa)) + 
  geom_bar(stat = 'identity', color = 'darkgrey') +
  theme_minimal(base_size = 14) + 
  scale_fill_manual(values = mcrcol) +
  labs(
    y = 'Biomass (kg / m2)',
    caption = 'Biomass is an overall average across all transects'
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  theme_minimal(base_size = 16, base_family = 'Lato') + 
  theme(
    axis.ticks.x = element_line(), 
    axis.title.x = element_blank(),
    legend.title = element_blank(), 
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )

png(here('figure/macroalgae.png'), family = 'Lato', height = 4, width = 6, units = 'in', res = 300)
print(p)
dev.off()

# DO ----------------------------------------------------------------------

# June in situ EPC data

flsht <- read_sheet("15Pya71XQF-pVdpDpUb74Fe1AOV0C5yOPf1SSiUkFeUU")
epctmp <- flsht %>% 
  select(
    station = `EPCStation`,
    date = `Date`, 
    lon = Lon, 
    lat = Lat,
    depthm = `Depth-m`,
    dosat = `DO%-Sat`, 
    do = `DO-mg/L`
  ) %>% 
  mutate(
    date = date(date), 
    station = as.character(station),
    station = gsub('^PP', '', station), 
    dosat = round(dosat, 0)
  ) %>% 
  group_by(station, date, lon, lat) %>%
  filter(depthm == max(depthm)) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326)



# reference data for ggsn, MUST have geometry named column
dat_ext <- epctmp %>% 
  st_bbox %>% 
  st_as_sfc %>% 
  st_buffer(dist = set_units(0.025, degree)) %>%
  st_bbox %>% 
  unname

# epctmp <- epctmp %>% 
#   st_transform(crs = 6443)

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

p <- bsmap + 
  geom_sf(data = epctmp, aes(color = dosat, size = dosat), pch = 19, inherit.aes = F) +
  theme(
    legend.title = element_blank(), 
    panel.grid = element_blank(), 
    axis.title = element_blank(),
    legend.position = 'none',
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA),
    text = element_text(family = 'Lato')
  ) +
  labs(
    subtitle = '% DO saturation of bottom waters in June'
  ) +
  annotation_scale(location = 'tl') +
  annotation_north_arrow(location = 'br', which_north = "true", height = grid::unit(0.75, "cm"), 
                         width = grid::unit(0.75, "cm")) +
  geom_sf_text(data = epctmp, aes(label = dosat), inherit.aes = F, nudge_x = 0.015)
  
png(here('figure/junedosat.png'), width = 5.5, height = 5, units = 'in', res = 300, family = 'Lato')
print(p)
dev.off()
