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
library(tbeptools)

data(kbrdat)

# levels for week, starts on first of week from jan through july
weeklv <- seq.Date(from = as.Date('2021-01-01'), to = Sys.Date(), by = 'days') %>% 
  lubridate::floor_date(unit = 'week') %>% 
  unique %>% 
  tibble(
    dt = ., 
    yr = year(dt), 
    mo = month(dt), 
    lb = format(dt, '%b %d')
  ) %>%
  filter(yr > 2020) %>% 
  filter(mo < 8) %>% 
  pull(lb)

# fish kills --------------------------------------------------------------

fishdat <- read.csv(here('data-raw/FishKillResultReport-lng.csv')) %>% 
  select(
    date = textBox6, 
    county = tEMPDataTextBox,
    location = cOUNTYDataTextBox, 
    species = textBox18
  ) %>% 
  mutate(
    date = mdy(date),
    county = gsub('\\s+$', '', county), 
    week = floor_date(date, unit = 'week'),
    week = factor(format(week, '%b %d')), 
    week = factor(week, levels = weeklv)
  ) %>%
  filter(year(date) >= 2021) %>% 
  complete(week, county) %>% 
  mutate(
    date = ymd(paste(2021, week)),
    county = factor(county)
    ) %>% 
  filter(month(date) > 3) %>% 
  select(week, date, county, species) %>% 
  group_by(week, date, county) %>% 
  summarize(
    cnt = sum(!is.na(species)),
    .groups = 'drop'
  )

mosums <- fishdat %>% 
  mutate(
    mo = month(date)
  ) %>% 
  group_by(mo, county) %>% 
  summarise(
    cnt = sum(cnt)
  )
p <- ggplot(fishdat, aes(x = week, y = cnt, fill = county)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    x = 'Week of', 
    y = 'No. of fish kill reports'
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = 'Pastel1', drop = T) + 
  theme_minimal(base_size = 14, base_family = 'Lato') + 
  theme(
    axis.ticks.x = element_line(), 
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
    legend.title = element_blank(), 
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )

png(here('figure/fishkillsjuly.png'), family = 'Lato', height = 3.25, width = 6, units = 'in', res = 300)
print(p)
dev.off()

# k brevis ----------------------------------------------------------------

toplo <- kbrdat %>%
  .[tbseg[tbseg$bay_segment %in% c('LTB', 'MTB'), ], ] %>%
  filter(var == 'kb') %>%
  filter(year(date) >= 2021 & month(date) > 3) %>%
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
    y10 = quantile(val, prob = 0.25, na.rm = T),
    y50 = quantile(val, prob = 0.5, na.rm = T),
    y90 = quantile(val, prob = 0.75, na.rm = T),
    y100 = max(val, na.rm = T),
    .groups = 'drop'
  )

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
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = expression(paste(italic('K. brevis '), ' cells (100k / L)')),
    x = 'Week of'
    )

png(here('figure/kbrevisjuly.png'), family = 'Lato', height = 3, width = 6, units = 'in', res = 300)
print(p)
dev.off()

# benthic results ---------------------------------------------------------

pineypoint <- c(-82.52469352586753, 27.629819505234703)

tomap <- rsbntdat %>% 
  inner_join(rsbntpts, ., by = 'station') %>% 
  mutate(TBBICat = factor(TBBICat, levels = c('Degraded', 'Intermediate', 'Healthy')))

# reference data for ggsn, MUST have geometry named column
dat_ext <- tomap %>% 
  st_bbox %>% 
  st_as_sfc %>% 
  st_buffer(dist = set_units(0.04, degree)) %>%
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

p <- bsmap + 
  geom_sf(data = tomap, aes(fill = TBBICat), pch = 21, inherit.aes = F, size = 5) +
  geom_point(aes(x = pineypoint[1], y = pineypoint[2]), inherit.aes = F, pch = 25, size = 7, fill = 'lightblue') + 
  theme_minimal() + 
  theme(
    # legend.title = element_blank(), 
    panel.grid = element_blank(), 
    axis.title = element_blank(),
    legend.position = 'right',
    panel.background = element_rect(fill = 'white'),
    axis.ticks = element_line(colour = 'grey'),
    panel.border = element_rect(colour = 'grey', fill = NA),
    text = element_text(family = 'Lato')
  ) +
  scale_fill_manual('Benthic condition', values = c('red', 'yellow', 'forestgreen')) +
  annotation_scale(location = 'tl') #+
  # annotation_north_arrow(location = 'br', which_north = "true", height = grid::unit(0.75, "cm"), 
  #                        width = grid::unit(0.75, "cm")) +
  # geom_sf_text(data = tomap, aes(label = round(TBBI, 0)), inherit.aes = F, nudge_x = 0.015)

png(here('figure/bntmap.png'), width = 5, height = 3.75, units = 'in', res = 300, family = 'Lato')
print(p)
dev.off()

