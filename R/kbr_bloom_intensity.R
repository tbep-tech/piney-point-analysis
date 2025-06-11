# from Stumpf et al. 2022 https://doi.org/10.1371/journal.pone.0260755

library(tidyverse)
library(sf)

data(tbseg, package = 'tbeptools')
data(kbrdat)

# LTB, MTB subset
ltbkb <- kbrdat[tbseg[tbseg$bay_segment == 'LTB', ], ] |>
  mutate(bay_segment = 'Lower Tampa Bay')
mtbkb <- kbrdat[tbseg[tbseg$bay_segment == 'MTB', ], ] |>
  mutate(bay_segment = 'Middle Tampa Bay')

toplo <- bind_rows(ltbkb, mtbkb) |>
  filter(var == 'kb') |> 
  sf::st_set_geometry(NULL) |>
  mutate(
    date = floor_date(date, unit = 'month'),
    val = 1e5 * val,
    blm = cut(val, 
              breaks = c(-Inf, 5000, 50000, 100000, 1000000, Inf), 
              labels = c('<= 5,000', '> 5,000', '> 50,000', '> 100,000', '> 1,000,000'),
              ), 
    date = floor_date(date, 'month')
  ) |>
  summarise(
    sumblm = n(),
    .by = c(date, bay_segment, blm)
  ) |> 
  filter(blm != '<= 5,000') |> 
  mutate(
    blm = fct_drop(blm)
  ) |> 
  mutate(
    maxvl = sum(sumblm), 
    .by = c(date, bay_segment)
  ) |> 
  mutate(
    maxvlall = max(maxvl)
  ) |> 
  mutate( 
    sumblm = 10 * sumblm / maxvlall
  )

lbs <-  c(expression("> 5" %*% 10^3), expression("> 5" %*% 10^4), expression("> 1" %*% 10^5), expression("> 1" %*% 10^6))

p <- ggplot(toplo, aes(x = date)) +
  geom_col(aes(y = sumblm, fill = blm), width = 30) +
  scale_fill_manual(
    values = c('#FCBBA1', '#FC9272', '#EF3B2C', '#99000D'),
    name = 'Cells / L',
    labels = lbs,
    drop = FALSE
  ) +
  scale_x_date(
    breaks = floor_date(seq.Date(min(toplo$date), 
                 max(toplo$date), 
                 by = '2 years'), unit = 'year'),
    date_labels = '%Y', 
    sec.axis = dup_axis(name = NULL), 
    expand = c(0, 0)
  ) +
  scale_y_continuous(breaks = seq(0, 10, by = 2), expand = c(0, 0)) +
  theme_bw() +
  facet_wrap(~bay_segment, ncol = 1) + 
  theme(
    axis.title.x = element_blank(),
    axis.ticks = element_line(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    legend.position = 'bottom', 
    strip.background = element_blank(),
    strip.text = element_text(size = 12, hjust = 0), 
    strip.placement = 'outside'
  ) +
  labs(
    y = 'BSI',
    title = expression(paste(italic('K. brevis'), ' bloom intensity'))
  )

png(here::here('figure/tb_bsi.png'), width = 13, height = 5, units = 'in', res = 300)
print(p)
dev.off()

