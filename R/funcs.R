# fit gam, estimate metrics, compile results in a nice way
gamfunc <- function(alldat, sta, ylb){
  
  tomod <-  alldat %>% 
    filter(station == !!sta) %>% 
    mutate(
      doy = yday(date), 
      cont_year = decimal_date(date), 
      # cont_year = as.numeric(cont_year),
      yr = year(date), 
      mo = month(date, label = T), 
      param = !!var
    ) %>% 
    select(date, station, param, value = var, doy, cont_year, yr, mo) %>% 
    as.data.frame
  
  gammod <- anlz_gam(tomod, trans = 'log10')
  browser()
  p1 <- show_prdseries(gammod, ylab = ylb)
  p2 <- show_metseason(gammod, metfun = mean, doystr = 60, doyend = 90, yrstr = NULL, yrend = NULL, ylab = ylb)
  p3 <- show_metseason(gammod, metfun = max, doystr = 60, doyend = 90, yrstr = NULL, yrend = NULL, ylab = ylb)
  p4 <- show_metseason(gammod, metfun = mean, doystr = 91, doyend = 120, yrstr = NULL, yrend = NULL, ylab = ylb)
  p5 <- show_metseason(gammod, metfun = max, doystr = 91, doyend = 120, yrstr = NULL, yrend = NULL, ylab = ylb)
  p <- p1 + ((p2 + p4) / (p3 + p5)) + plot_layout(ncol = 1, heights = c(0.3, 0.7)) & theme_bw(base_size = 9.5)
  
  return(p)
  
}

# fit wrtds, compile plots
wrtdsfunc <- function(alldat, sta, ylb){
  
  tomod <- alldat %>%
    filter(station == !!sta) %>%
    mutate(
      res = log(var),
      res = ifelse(is.infinite(res), NA, res),
      lim = 0,
    ) %>%
    select(date, res, flo = sal, lim)
  
  wrtdsmod <- tomod %>%
    as.data.frame %>%
    tidal %>%
    modfit(flo_div = 10, fill_empty = T, tau = c(0.5, 0.9))
  
  p1 <- fitplot(wrtdsmod, annuals = F, logspace = F) 
  p2 <- prdnrmplot(wrtdsmod, annuals = F, logspace = F) 
  p3 <- seasyrplot(wrtdsmod, predicted = F, logspace = F, tau = 0.5) + labs(subtitle = '50th %tile')
  p4 <- seasyrplot(wrtdsmod, predicted = F, logspace = F, tau = 0.9) + labs(subtitle = '90th %tile')
  
  p <- p1 + p2 + (p3 | p4) + plot_layout(ncol = 1) & scale_y_log10(ylb)
  
  return(p)
  
}