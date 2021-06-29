library(ggplot2)
library(gganimate)
library(sf)
library(lubridate)
library(dplyr)
library(here)

load(file = here('data/rswqdat.RData'))
load(file = here('data/rsstatloc.RData'))

nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# colors 
vrscols <- RColorBrewer::brewer.pal(n = 9, name = 'Blues')

# format chlorophyll data
toplo <- rswqdat %>% 
  filter(!station %in% nonbay) %>% 
  filter(var %in% 'chla') %>% 
  inner_join(rsstatloc, ., by = 'station') %>% 
  mutate(
    lng = st_coordinates(.)[, 1], 
    lat = st_coordinates(.)[, 2],
    dategrp = floor_date(date, unit = 'week')
  ) %>% 
  st_set_geometry(NULL) %>% 
  select(station, date, dategrp, val, lat, lng, inrng)

# static plot
pbase <- ggplot(toplo, aes(x = lng, y = lat, size = val, fill = val, group = dategrp)) + 
  geom_point(pch = 21, color = 'black', alpha = 0.8) +
  scale_fill_gradientn('Chl-a (ug/L)', trans = 'log10', colours = vrscols) +
  scale_size('Chl-a (ug/L)', range = c(1, 10), trans = 'log10') + 
  coord_map() + 
  guides(
    fill = guide_legend(), 
    size = guide_legend()
  ) + 
  theme_minimal() +
  theme(
    axis.title = element_blank()
  )

# create animation
anim <- pbase +
  transition_states(
    dategrp, 
    transition_length = 20, 
    state_length = 20
  ) +
  enter_grow() + 
  exit_shrink() + 
  ggtitle('Week of {closest_state}') +
  shadow_trail(alpha = 0.05)

anim_save(here('chlani.gif'), anim)