library(tidyverse)
library(readxl)
library(patchwork)
library(here)

rawdat <- read_excel(here('data-raw/FWC_Manatee_Mortality.xlsx')) |> 
  filter(County %in% c('Pinellas', 'Hillsborough', 'Manatee')) |> 
  filter(!COD %in% c('Natural: Cold Stress', 'Human Related: Watercraft Collision')) |> 
  mutate(
    date = as.Date(`Report date`), 
    yr = year(date)
  )

weeklv <- seq.Date(from = as.Date('2021-01-03'), to = as.Date('2021-12-31'), by = 'days') %>% 
  lubridate::floor_date(unit = 'week') %>% 
  unique

toplo1 <- rawdat |> 
  mutate(
    yr = year(date)
  ) |> 
  summarise(
    cnt = n(), 
    .by = c(yr, County)
  ) |> 
  complete(
    yr = seq(min(yr), max(yr), 1), 
    County, 
    fill = list(cnt = 0)
  )

toplo2 <- rawdat|> 
  filter(year(date) == 2021) |> 
  mutate(
    month = lubridate::floor_date(date, unit = 'month')
  ) |> 
  summarise(
    cnt = n(), 
    .by = c(month, County)
  )

p1 <- ggplot(toplo1, aes(x = yr, y = cnt, fill = County)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    x = NULL,
    y = 'No. of manatee mortality reports', 
    subtitle = '(a) By year'
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_continuous(breaks = seq(min(toplo1$yr), max(toplo1$yr), 1), expand = c(0.01, 0.01)) +
  scale_fill_brewer(palette = 'Pastel1', drop = T) + 
  theme_minimal() + 
  theme(
    axis.ticks.x = element_line(), 
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )

p2 <- ggplot(toplo2, aes(x = month, y = cnt, fill = County)) + 
  geom_bar(stat = 'identity', colour = 'darkgrey') + 
  labs(
    x = NULL, 
    y = 'No. of manatee mortality reports',
    subtitle = '(b) 2021 month', 
    caption = 'Note: boat collision and cold stress reports removed'
  ) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_x_date(breaks = unique(toplo2$month), date_labels = '%b', expand = c(0.01, 0.01)) +
  scale_fill_brewer(palette = 'Pastel1', drop = T) + 
  theme_minimal() + 
  theme(
    axis.ticks.x = element_line(), 
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1),
    legend.position = 'top', 
    panel.grid.minor.y = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_blank(), 
    plot.caption = element_text(size = 10)
  )

p <- p1 + p2 + guide_area() +  
  plot_layout(ncol = 1, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 0.1))

png(here('figure/manatee_mortality.png'), height = 4.5, width = 8, units = 'in', res = 300)
print(p)
dev.off()