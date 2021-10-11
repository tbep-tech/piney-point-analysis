
library(tidyverse)
library(lubridate)
library(sf)
library(patchwork)
library(units)
library(grid)
library(scales)
library(extrafont)

data(rswqdat)
data(bswqdat)
data(bsstatloc)
data(rsstatloc)

vr <- 'chla'
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# monitoring data
rswqtmp <- rswqdat %>% 
  filter(var == vr) %>% 
  filter(!station %in% nonbay) %>% 
  select(-qual, -bswqstation, -nrmrng, -source, -uni, -lbunis, -inrng) %>% 
  mutate(
    date = floor_date(date, unit = 'week'), 
    mo = month(date)
  ) 

# baseline data
bswqtmp <- bswqdat %>% 
  select(-source, -uni) %>% 
  filter(var == vr) %>% 
  filter(yr > 2005) %>% 
  group_by(mo, var) %>% 
  summarise(   
    maxv = quantile(val,  0.84, na.rm = T), 
    minv = quantile(val, 0.16, na.rm = T),
    .groups = 'drop'
  ) %>%
  left_join(parms, by = 'var') %>% 
  mutate(
    lbunis = gsub('^.*\\s(\\(.*\\))$', '\\1', lbs), 
    lbunis = gsub('pH', '', lbunis), 
    datestr= paste0('2021-', mo, '-01'), 
    datestr = ymd(datestr), 
    dateend = ceiling_date(datestr, unit = 'month')
  )

ylab <- unique(rswqtmp$lbs)
brks <- seq.Date(min(rswqtmp$date), max(rswqtmp$date), by = 'week')

p1 <- ggplot() + 
  geom_rect(data = bswqtmp, aes(xmin = datestr, xmax = dateend, ymin = minv, ymax = maxv, group = mo, fill = 'Monthly baseline (mean +/- 1 sd)'), alpha = 0.2) +
  geom_boxplot(data = rswqtmp, aes(x = date, y = val, group = date), fill= 'lightblue', outlier.colour = NA, lwd = 0.75, alpha = 0.8, show.legend = F) + 
  geom_jitter(data = rswqtmp, aes(x = date, y = val, group = date), alpha = 0.4, size = 0.75) + 
  scale_fill_manual(NULL, values = 'blue') +
  scale_linetype_manual(values = 'dashed') + 
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  labs(
    y = ylab, 
    x = NULL
  ) + 
  coord_cartesian(xlim = range(rswqtmp$date)) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = 'top', 
    strip.background = element_blank(), 
    strip.text = element_text(size = 14), 
    panel.grid.minor = element_blank(), 
    axis.text.x = element_blank()
  ) +
  scale_y_log10(paste0('log-', ylab))

vr <- 'secchi'
nonbay <- c('BH01', 'P Port 2', 'P Port 3', 'PM Out', '20120409-01', 'PPC41', 'P Port 4', 'PMB01', 'NGS-S Pond')

# monitoring data
rswqtmp <- rswqdat %>% 
  filter(var == vr) %>% 
  filter(!station %in% nonbay) %>% 
  select(-qual, -bswqstation, -nrmrng, -source, -uni, -lbunis, -inrng) %>% 
  mutate(
    date = floor_date(date, unit = 'week'), 
    mo = month(date)
  ) 

# baseline data
bswqtmp <- bswqdat %>% 
  select(-source, -uni) %>% 
  filter(var == vr) %>% 
  filter(yr > 2005) %>% 
  group_by(mo, var) %>% 
  summarise(   
    maxv = quantile(val,  0.84, na.rm = T), 
    minv = quantile(val, 0.16, na.rm = T),
    .groups = 'drop'
  ) %>%
  left_join(parms, by = 'var') %>% 
  mutate(
    lbunis = gsub('^.*\\s(\\(.*\\))$', '\\1', lbs), 
    lbunis = gsub('pH', '', lbunis), 
    datestr= paste0('2021-', mo, '-01'), 
    datestr = ymd(datestr), 
    dateend = ceiling_date(datestr, unit = 'month')
  )

ylab <- unique(rswqtmp$lbs)
brks <- seq.Date(min(rswqtmp$date), max(rswqtmp$date), by = 'week')

p2 <- ggplot() + 
  geom_rect(data = bswqtmp, aes(xmin = datestr, xmax = dateend, ymin = minv, ymax = maxv, group = mo, fill = 'Monthly baseline (mean +/- 1 sd)'), alpha = 0.2) +
  geom_boxplot(data = rswqtmp, aes(x = date, y = val, group = date), fill= 'lightblue', outlier.colour = NA, lwd = 0.75, alpha = 0.8, show.legend = F) + 
  geom_jitter(data = rswqtmp, aes(x = date, y = val, group = date), alpha = 0.4, size = 0.75) + 
  scale_fill_manual(NULL, values = 'blue') +
  scale_linetype_manual(values = 'dashed') + 
  scale_x_date(breaks = brks, date_labels = '%b %d') +
  labs(
    y = ylab, 
    x = 'Week of'
  ) + 
  coord_cartesian(xlim = range(rswqtmp$date), ylim = c(0, 6)) +
  theme_minimal(base_size = 14) + 
  theme(
    legend.position = 'top', 
    strip.background = element_blank(), 
    strip.text = element_text(size = 14), 
    panel.grid.minor = element_blank(), 
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1)
  )

p <- p1 + p2 + plot_layout(ncol = 1, guides = 'collect') & theme(legend.position = 'top')
  
png('figure/chlsecchiall.png', height = 5, width = 9, res = 300, units = 'in', family = 'Lato')
print(p)
dev.off()
