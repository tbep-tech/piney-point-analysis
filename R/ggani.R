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

# chlorophyll -------------------------------------------------------------

# format chlorophyll data
toplo <- wqdat %>% 
  filter(var %in% 'chla')

# static plot
pbase <- bsmap +
  geom_point(data = toplo, aes(x = lng, y = lat, size = val, fill = val, group = dategrp, color = inrng), pch = 21, alpha = 0.8) +
  geom_point(pch = 21, color = 'black', alpha = 0.8) +
  scale_fill_gradientn('Chl-a (mg/L)', trans = 'log10', colours = vrscols) +
  scale_color_manual('In normal\nrange?', values = c('black', 'lightgrey')) +
  scale_size('Chl-a (mg/L)', range = c(1, 10), trans = 'log10') + 
  coord_map() + 
  guides(
    fill = guide_legend(), 
    size = guide_legend(), 
    color = guide_legend(override.aes = list(size = 10))
  ) + 
  theme_minimal(base_size = bssz) +
  theme(
    axis.title = element_blank()
  )

# create animation
chlanim <- pbase +
  transition_states(
    dategrp, 
    transition_length = frms, 
    state_length = frms
  ) +
  enter_grow() + 
  exit_shrink() + 
  ggtitle('Week of {closest_state}') +
  shadow_trail(alpha = 0.05)

anim_save(here('chlani.gif'), chlanim)
chlgif <- animate(chlanim, width = dm, height = dm)

# total nitrogen ----------------------------------------------------------

# format nitrogen data
toplo <- wqdat %>% 
  filter(var %in% 'tn')

# static plot
pbase <- bsmap +
  geom_point(data = toplo, aes(x = lng, y = lat, size = val, fill = val, group = dategrp, color = inrng), pch = 21, alpha = 0.8) +
  geom_point(pch = 21, color = 'black', alpha = 0.8) +
  scale_fill_gradientn('TN (mg/L)', trans = 'log10', colours = vrscols) +
  scale_color_manual('In normal\nrange?', values = c('black', 'lightgrey'), guide = 'none') +
  scale_size('TN (mg/L)', range = c(1, 10), trans = 'log10') + 
  coord_map() + 
  guides(
    fill = guide_legend(), 
    size = guide_legend()
  ) + 
  theme_minimal(base_size = bssz) +
  theme(
    axis.title = element_blank()
  )

# create animation
tnanim <- pbase +
  transition_states(
    dategrp, 
    transition_length = frms, 
    state_length = frms
  ) +
  enter_grow() + 
  exit_shrink() + 
  ggtitle('') +
  shadow_trail(alpha = 0.05)

anim_save(here('tnani.gif'), tnanim)
tngif <- animate(tnanim, width = dm, height = dm)

# combine gifs ------------------------------------------------------------

wqgif <- image_append(c(chlgif[1], tngif[1]))
for(i in 2:100){
  combined <- image_append(c(chlgif[i], tngif[i]))
  wqgif <- c(wqgif, combined)
}

anim_save(here('wqani.gif'), wqgif)