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
  filter(mo < 9) %>% 
  pull(lb)


# k brevis concentations middle/lower tb ----------------------------------

# MTB subset
toplo <- kbrdat %>%
  .[tbseg[tbseg$bay_segment %in% c('LTB', 'MTB'), ], ] %>%
  filter(var == 'kb') %>% 
  filter(year(date) >= 2021) %>%
  filter(month(date) < 9) %>% 
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
  complete(week)
s
# plot
p <- ggplot(toplo, aes(x = week)) +
  geom_boxplot(
    aes(ymin = y0, lower = y10, middle = y50, upper = y90, ymax = y100),
    stat = "identity", width = 0.75, fill = '#00806E'
  ) +
  scale_y_log10(labels = function(x) as.numeric(format(x, scientific = FALSE))) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, size = 10, hjust = 1),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = 'Cells (100k / L)',
    title = expression(paste(italic('K. brevis'), ' concentrations in Middle and Lower Tampa Bay, log-scale')),
    subtitle = paste('Boxplots show the entire range of values from min, 10th %tile, median, 90th %tile, and max, n =', sum(toplo$cnt, na.rm = T)),
    caption = 'Data from FWC/NOAA HABSOS; https://www.ncei.noaa.gov/maps/habsos/maps.htm\nPlot created by TBEP'
  )

png(here('figure/aug_redtide.png'), height = 4, width = 8, units = 'in', res = 300)
print(p)
dev.off()

# hydrographs -------------------------------------------------------------

library(dataRetrieval)
alph <- 0.5

hyddat <- readNWISdv(siteNumber,parameterCd,
                           "1980-01-01","2010-01-01")


start <- '2006-08-01'
end <- '2021-10-01'
alaf <- '02301500'

# flow in cfs
alafhyd <- dataRetrieval::readNWISdv(alaf, "00060", start, end) %>% 
  dataRetrieval::renameNWISColumns(alafhyd)

toplo <- alafhyd %>% 
  mutate(
    Flow = stats::filter(Flow, rep(1 / 61, 61), sides = 1),
    perc = 100 * ecdf(Flow)(Flow),
    wk = week(Date)
  ) %>% 
  # group_by(wk) %>% 
  # nest() %>% 
  # mutate(
  #   data = map(data, function(x){
  #     
  #     out <- x %>% 
  #       mutate(
  #         perc = 100 * ecdf(Flow)(Flow)
  #       )
  #     
  #     return(out)
  #   
  #   })
  # ) %>% 
  # unnest('data') %>% 
  # arrange(Date) %>% 
  filter(Date >= as.Date('2020-09-01'))


ggplot(data = toplo, aes(x = Date, y = perc)) +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 10, alpha = alph, fill = 'red') +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 10, ymax = 25, alpha = alph, fill = 'orange') +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 25, ymax = 75, alpha = alph, fill = 'yellow') +
  geom_rect(xmin = -Inf, xmax = Inf, ymin = 75, ymax = Inf, alpha = alph, fill = 'green') +
  geom_line() +
  scale_x_date(expand = c(0, 0), labels = scales::date_format('%b %y'), breaks = '1 month') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + 
  labs(
    x = NULL, 
    y = 'Flow percentile'
  )
plot(perc~ Date, toplo, type = 'l', ylim = c(0, 1))
