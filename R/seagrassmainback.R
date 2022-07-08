library(tidyverse)
library(lubridate)
library(sf)
library(ggmap)
library(ggspatial)
library(patchwork)
library(units)
library(grid)
library(scales)
library(here)
library(googlesheets4)
library(googledrive)

data(rswqdat)
data(bswqdat)
data(bsstatloc)
data(rsstatloc)
data(ppseg)
data(parms)
data(rsphydat)
data(rsphypts)
data(trnpts)
data(rstrndat)
data(rstrnpts)
data(bstransect)
data(rstrnwts)
data(rsallpts)
data(segmask)

# deauth all so it can build with gh actions
drive_deauth()
gs4_deauth()

source("R/funcs.R")

sf_use_s2(FALSE)

# segments
ppseg <- ppseg %>% 
  rename(area = Name) %>% 
  group_by(area) %>% 
  summarise() %>% 
  st_buffer(dist = set_units(0.0001, degree)) %>% 
  st_buffer(dist = set_units(-0.0001, degree)) %>% 
  mutate(
    area = factor(area)
  )

cols <- c("#E16A86", "#50A315", "#009ADE")
names(cols) <- levels(ppseg$area)

# nonbay
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

trns <- read_sheet('1_wxkXJPjSlVRt9oVLO_AGxjupg8PI_PHkkLA6rq2zGQ') %>% 
  select(station, group) %>% 
  unique %>% 
  filter(!is.na(group))

rstrndat <- rstrndat %>% 
  left_join(trns, by = 'station')

mcrsel <- c("Red", "Green", "Brown", "Cyanobacteria")
savsel <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme')

colpal <- colorRampPalette(RColorBrewer::brewer.pal(n = 8, name = 'Dark2'))
savlevs <- c('Thalassia testudinum', 'Halodule wrightii', 'Syringodium filiforme', 'Ruppia maritima', 'Halophila engelmannii', 'Halophila decipiens')
savcol <- colpal(length(savlevs))
names(savcol) <- savlevs
savcol <- savcol[savsel]
mcrcol <- c('tomato1', 'lightgreen', 'burlywood3', 'lightblue')
names(mcrcol) <- mcrsel
mcrcol <- mcrcol[mcrsel]
cols <- c(mcrcol, savcol)

cols <- c(cols, Total = 'white')

# add area
trnsum <- rstrndat %>%
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(ppseg) %>% 
  st_set_geometry(NULL) %>%
  dplyr::group_by(area, group, typ, date, station, taxa, location) %>%
  dplyr::summarise(
    pa = as.numeric(any(bb > 0))
  ) %>%  
  mutate(
    date = floor_date(date, unit = 'month'), 
    typ = factor(typ, levels = c('mcr', 'sav'), labels = c('Macroalgae', 'Seagrass'))
  ) %>% 
  group_by(area, group, typ, date, taxa) %>% 
  summarize(
    foest = sum(pa) / length(pa)
  ) %>% 
  filter(taxa %in% c(mcrsel, savsel))

trnsumtots <- rstrndat %>%
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(ppseg) %>% 
  st_set_geometry(NULL) %>%
  dplyr::group_by(area, group, typ, date, station, location) %>%
  dplyr::summarise(
    pa = as.numeric(any(bb > 0))
  ) %>%  
  mutate(
    date = floor_date(date, unit = 'month'), 
    typ = factor(typ, levels = c('mcr', 'sav'), labels = c('Macroalgae', 'Seagrass'))
  ) %>% 
  group_by(area, group, typ, date) %>% 
  summarize(
    foest = sum(pa) / length(pa)
  ) %>% 
  mutate(taxa = 'Total')

toplo <- bind_rows(trnsum, trnsumtots)

dodge <- position_dodge(width=0) 

p1 <- ggplot(toplo, aes(x = date, y = foest)) + 
  geom_line(aes(group = taxa), position = dodge) +
  geom_point(aes(fill = taxa, group = taxa), pch = 21, stat = 'identity', color = 'black', size = 4, position = dodge, stroke = 1.5) +
  facet_grid(typ ~ area + group) +
  theme_minimal(base_size = 14) + 
  scale_x_date(date_breaks = '1 month', date_labels = '%b') + 
  scale_y_continuous(limits = c(0, 1))+
  scale_fill_manual(values = cols) +
  labs(
    y = 'Freq. occurrence'
  ) +
  theme(
    legend.position = 'top', 
    legend.title = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 14), 
    axis.title.x = element_blank(), 
    axis.ticks.x = element_line(), 
    panel.grid.minor = element_blank(),
    # panel.spacing=unit(2, "lines"), 
    panel.background = element_rect(fill = 'grey95', color = 'white'), 
    panel.grid.major = element_line(color = 'grey90')
  )

toplo2 <- rstrndat %>% 
  inner_join(rstrnpts, ., by = 'station') %>% 
  st_intersection(ppseg) %>% 
  st_set_geometry(NULL) %>%
  select(date, station, area, group) %>% 
  unique %>% 
  mutate(
    mo = month(date, label = T, abbr = T)
  ) %>% 
  group_by(mo, area, group) %>% 
  summarise(
    cnt = n(),
    .groups = 'drop'
  )

p2 <- ggplot(toplo2, aes(x = mo, y = cnt)) + 
  geom_bar(stat = 'identity') +
  facet_grid( ~ area + group) +
  theme_minimal(base_size = 14)+ 
  labs(
    y = 'Transect visits', 
    x = NULL
  )
  
png('~/Desktop/foestbyloc.png', height = 6, width = 12, units = 'in', res = 300)
print(p1)
dev.off()

png('~/Desktop/trneff.png', height = 4, width = 12, units = 'in', res = 300)
print(p2)
dev.off()
