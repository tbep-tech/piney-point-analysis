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
  browser()
  gammod <- anlz_gam(tomod, trans = 'log10')
  
  p1 <- show_prdseries(gammod, ylab = ylb)
  p2 <- seasyrplotgam(gammod, ylb)
  p3 <- show_metseason(gammod, metfun = mean, doystr = 60, doyend = 90, yrstr = NULL, yrend = NULL, ylab = ylb)
  p4 <- show_metseason(gammod, metfun = max, doystr = 60, doyend = 90, yrstr = NULL, yrend = NULL, ylab = ylb)
  p5 <- show_metseason(gammod, metfun = mean, doystr = 91, doyend = 120, yrstr = NULL, yrend = NULL, ylab = ylb)
  p6 <- show_metseason(gammod, metfun = max, doystr = 91, doyend = 120, yrstr = NULL, yrend = NULL, ylab = ylb)
  p <- (p1 + p2) / ((p3 + p5) / (p4 + p6)) + plot_layout(ncol = 1, heights = c(0.3, 0.7)) & theme_bw(base_size = 9.5) + theme(axis.title.x = element_blank())
  
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
  p3 <- seasyrplottmp(wrtdsmod, predicted = F, logspace = F, tau = 0.5) + labs(subtitle = '50th %tile')
  p4 <- seasyrplottmp(wrtdsmod, predicted = F, logspace = F, tau = 0.9) + labs(subtitle = '90th %tile')
  
  p <- p1 + p2 + (p3 | p4) + plot_layout(ncol = 1) & scale_y_log10(ylb)
  
  return(p)
  
}

# seasonal year plot for gam
seasyrplotgam <- function(gammod, ylb){
  
  # doy plot
  toplo <- anlz_prd(gammod) %>% 
    mutate(
      dumdate = date,
      sz = case_when(
        yr == 2021 ~ 2, 
        T ~ 0.5
      )
    ) 
  year(toplo$dumdate) <- 2000
  
  p <- ggplot(toplo, aes(x = dumdate, y = value, group = yr, colour = yr)) + 
    geom_line(size = toplo$sz) +
    scale_colour_gradientn('Year', colours = gradcols()) +
    # guides(colour = guide_colourbar(barwidth = 10)) +
    theme_bw() +
    theme(
      legend.position = 'top', 
      axis.title.x = element_blank()
    ) +
    scale_y_continuous(ylb) +
    scale_x_date(NULL, date_labels = "%m/%d")
  
  return(p)
  
}

seasyrplottmp <- function(dat_in, years = NULL, tau = NULL, predicted = TRUE, logspace = TRUE, col_vec = NULL, grids = TRUE, pretty = TRUE, lwd = 0.5, alpha = 1, ...){
  
  # sanity check
  if(!any(grepl('^fit|^norm', names(dat_in))))
    stop('No fitted data in tidal object, run modfit function')
  
  # convert to df for plotting, get relevant columns
  to_plo <- data.frame(dat_in)
  sel_vec <- grepl('^year$|^month$|^fit|^norm', names(to_plo))
  to_plo <- to_plo[, sel_vec]
  
  # subset data by years
  if(!is.null(years)){ 
    
    to_plo <- filter(to_plo, year %in% years)
    
  }
  
  # get names of the quantiles for norms and preds to plot
  if(is.null(tau)){
    
    tau_fits <- grep('^fit', names(dat_in))
    tau_fits <- floor(median(tau_fits))
    tau_fits <- names(dat_in)[tau_fits]
    tau_nrms <- gsub('^fit', 'norm', tau_fits)
    
  } else {
    
    if(length(tau) > 1) 
      stop('Only one quantile can be plotted')
    if(length(grep(paste0(tau, '$'), names(dat_in))) == 0)
      stop('Specified tau not in object')
    
    tau_fits <- paste0('fit', tau)
    tau_nrms <- paste0('norm', tau)
    
  }
  
  # create date vector with common year
  names(to_plo)[names(to_plo) %in% tau_nrms] <- 'nrm'
  names(to_plo)[names(to_plo) %in% tau_fits] <- 'fit'
  to_plo <- select(to_plo, year, month, nrm, fit) %>% 
    group_by(year, month) %>% 
    summarize(
      nrm = mean(nrm, na.rm = TRUE), 
      fit = mean(fit, na.rm = TRUE)
    ) %>% 
    ungroup %>% 
    mutate(
      year_dum = '2000',   
      day = '01', 
      sz = case_when(
        year == 2021 ~ 2, 
        T ~ 0.5
      )
    ) %>% 
    tidyr::unite('date', year_dum, month, day, sep = '-') %>% 
    mutate(date = as.Date(date, '%Y-%m-%d')) %>% 
    na.omit
  nrms <- select(to_plo, -fit)
  fits <- select(to_plo, -nrm)
  
  # y-axis label
  ylabel <- attr(dat_in, 'reslab')
  
  # back-transform if needed
  if(!logspace){
    
    nrms$nrm <- exp(nrms$nrm)
    fits$fit <- exp(fits$fit)
    
    # strip log, ln  from yaxs label if there
    ylabel <- gsub('ln-|log-', '', as.character(ylabel))
    ylabel <- as.expression(parse(text = ylabel))
    
  }
  
  # plot fits or nrms
  if(predicted){
    
    p <- ggplot(fits, aes(x = date, y = fit, group = year, colour = year)) + 
      geom_line(alpha = alpha, na.rm = TRUE, size = fits$sz)
    
  } else {
    
    p <- ggplot(nrms, aes(x = date, y = nrm, group = year, colour = year)) + 
      geom_line(alpha = alpha, na.rm = TRUE, size = nrms$sz)
    
  }
  
  # exit if pretty is F
  if(!pretty) return(p)
  
  ##
  # change aesthetics
  
  # pick colors
  cols <- gradcols(col_vec = col_vec)
  
  # modify aesthetics
  p <- p + 
    scale_colour_gradientn('Year', colours = cols) +
    guides(colour = guide_colourbar(barwidth = 10)) +
    theme_bw() +
    theme(
      legend.position = 'top', 
      axis.title.x = element_blank()
    ) +
    scale_y_continuous(ylabel) +
    scale_x_date(xlab, date_labels = "%m/%d")
  
  
  # remove grid lines
  if(!grids) 
    p <- p + 
    theme(      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  return(p)
  
}

# function for formatting p-values in tables
p_ast <- function(x){
  
  sig_cats <- c('**', '*', 'ns')
  sig_vals <- c(-Inf, 0.005, 0.05, Inf)
  
  out <- cut(x, breaks = sig_vals, labels = sig_cats, right = FALSE)
  out <- as.character(out)
  
  return(out)
  
}

# lm function on facets, modified from https://gist.github.com/kdauria/524eade46135f6348140
stat_smooth_func_with_pval <- function(mapping = NULL, data = NULL,
                                       geom = "smooth", position = "identity",
                                       ...,
                                       method = "auto",
                                       formula = y ~ x,
                                       se = TRUE,
                                       n = 80,
                                       span = 0.75,
                                       fullrange = FALSE,
                                       level = 0.95,
                                       method.args = list(),
                                       na.rm = FALSE,
                                       show.legend = NA,
                                       inherit.aes = TRUE,
                                       xpos = NULL,
                                       ypos = NULL,
                                       xpos2 = NULL,
                                       ypos2 = NULL) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSmoothFunc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      method = method,
      formula = formula,
      se = se,
      n = n,
      fullrange = fullrange,
      level = level,
      na.rm = na.rm,
      method.args = method.args,
      span = span,
      xpos = xpos,
      ypos = ypos,
      xpos2 = xpos2,
      ypos2 = ypos2,
      ...
    )
  )
}

StatSmoothFunc <- ggproto("StatSmooth", Stat,
                          
                          setup_params = function(data, params) {
                            # Figure out what type of smoothing to do: loess for small datasets,
                            # gam with a cubic regression basis for large data
                            # This is based on the size of the _largest_ group.
                            if (identical(params$method, "auto")) {
                              max_group <- max(table(data$group))
                              
                              if (max_group < 1000) {
                                params$method <- "loess"
                              } else {
                                params$method <- "gam"
                                params$formula <- y ~ s(x, bs = "cs")
                              }
                            }
                            if (identical(params$method, "gam")) {
                              params$method <- mgcv::gam
                            }
                            
                            params
                          },
                          
                          compute_group = function(data, scales, method = "auto", formula = y~x,
                                                   se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                   xseq = NULL, level = 0.95, method.args = list(),
                                                   na.rm = FALSE, xpos=NULL, ypos=NULL, 
                                                   xpos2=NULL, ypos2=NULL) {
                            if (length(unique(data$x)) < 2) {
                              # Not enough data to perform fit
                              return(data.frame())
                            }
                            
                            if (is.null(data$weight)) data$weight <- 1
                            
                            if (is.null(xseq)) {
                              if (is.integer(data$x)) {
                                if (fullrange) {
                                  xseq <- scales$x$dimension()
                                } else {
                                  xseq <- sort(unique(data$x))
                                }
                              } else {
                                if (fullrange) {
                                  range <- scales$x$dimension()
                                } else {
                                  range <- range(data$x, na.rm = TRUE)
                                }
                                xseq <- seq(range[1], range[2], length.out = n)
                              }
                            }
                            # Special case span because it's the most commonly used model argument
                            if (identical(method, "loess")) {
                              method.args$span <- span
                            }
                            
                            if (is.character(method)) method <- match.fun(method)
                            
                            base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                            model <- do.call(method, c(base.args, method.args))
                            
                            m = model
                            
                            if(length(coef(m)) == 2){
                              eq1 <- substitute(italic(y) == a + b %.% italic(x), 
                                              list(a = format(coef(m)[[1]], digits = 3), 
                                                   b = format(coef(m)[[2]], digits = 3)))
                            
                              eq2 <- substitute(italic(r)^2~"="~r2*","~~italic(p)~"="~pval, 
                                              list(r2 = format(summary(m)$r.squared, digits = 2),
                                                   pval = format(summary(m)$coef[2,4], digits = 2)))
                            
                            }
                            
                            if(length(coef(m)) == 1){
                              
                              eq1 <- substitute(italic(y) == b %.% italic(x), 
                                                list(b = format(coef(m)[[1]], digits = 3))
                                                )
                              
                              eq2 <- substitute(italic(r)^2~"="~r2*","~~italic(p)~"="~pval, 
                                                list(r2 = format(summary(m)$r.squared, digits = 2),
                                                     pval = format(summary(m)$coef[1,4], digits = 2)))
                              
                            }
                            
                            func_string1 = as.character(as.expression(eq1))
                            func_string2 = as.character(as.expression(eq2))
                            
                            if(is.null(xpos)) xpos = min(data$x)*0.9
                            if(is.null(ypos)) ypos = max(data$y)*0.9
                            if(is.null(xpos2)) xpos2 = xpos
                            if(is.null(ypos2)) ypos2 = max(data$y)*0.6
                            
                            data.frame(x = rbind(xpos, xpos2), 
                                       y = rbind(ypos, ypos2), 
                                       label = rbind(func_string1, func_string2))
                            
                          },
                          
                          required_aes = c("x", "y")
)