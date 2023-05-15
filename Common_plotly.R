


#' @title FindMatchTicks.Plotly.2y: align primary and secondary axies
#'
#' @description This function is to find the range and distance of ticks for plotly of 2 y axes to achive overlay match.
#'
#' @param y1 (required) array to be shown in primary y axis.
#' @param y2 (required) array to be shown in secondary y axis.
#' @param init.dtick.ratio (optional) starting no. of ticks, default to be 4, adjusted for ranges of y1 and y2
#' @param y1.axis.min (optional) lower bound of primary y axis to be shown, default to be min(y1), adjusted for distance of ticks
#' @param y2.axis.min (optional) lower bound of secondary y axis to be shown, default to be min(y2), adjusted for distance of ticks
#'
#' @return legends
#'
#' @export
#'
#' @examples ## load data
#' y1 = runif(100, 0, 200)
#' y2 = runif(100, -50, 50)
#' 
#'
#' ## extract legend
#' FindMatchTicks.Plotly.2y(y1, y2)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
FindMatchTicks.Plotly.2y <- function(y1, y2, init.dtick.ratio = 4, y1.axis.min=NULL, y2.axis.min=NULL, margin.pct = 0.05) {
  
  
  ## Step: collect basic info from y1
  #------------------------------------------------#
  if (is.null(y1.axis.min)) y1.min = min(y1, na.rm = TRUE) else y1.min = y1.axis.min ## min of y1
  y1.max = max(y1, na.rm = TRUE) ## max of y1
  y1.range = y1.max - y1.min  ## range of y1
  
  y1.power10 = 10^floor(log(y1.range)/log(10))  ## find unit of y1 range, e.g. 6789 -> 1000
  y1.range.base = y1.power10 * floor(y1.range/y1.power10) ## find the base of y1 range, rounded by y1.power10, e.g. 6789 -> 6000
  #------------------------------------------------#
  ## End of Step;
  
  
  ## Step: collect basic info from y2
  #------------------------------------------------#
  
  if (is.null(y2.axis.min)) y2.min = min(y2, na.rm = TRUE) else y2.min = y2.axis.min ## min of y2
  y2.max = max(y2, na.rm = TRUE) ## max of y2
  y2.range = y2.max - y2.min  ## range of y2
  
  y2.power10 = 10^floor(log(y2.range)/log(10))  ## find unit of y2 range, e.g. 2345 -> 1000
  y2.range.base = y2.power10 * floor(y2.range/y2.power10) ## find the base of y2 range, rounded by y2.power10, e.g. 2345 -> 2000
  
  #------------------------------------------------#
  ## End of Step;
  
  
  ## Step: calculate the distance between ticks, and determine the min to be shown in yaxis
  #------------------------------------------------#
  
  ## primary yaxis
  y1.dtick = y1.range.base/init.dtick.ratio ## distance between ticks
  y1.axis.min = floor(y1.min/y1.dtick) * y1.dtick - margin.pct*y1.dtick## determine the min value to be shown in yaxis
  
  ## secondary yaxis
  y2.dtick = y2.range.base/init.dtick.ratio ## distance between ticks
  y2.axis.min = floor(y2.min/y2.dtick) * y2.dtick - margin.pct*y2.dtick
  #------------------------------------------------#
  ## End of Step;
  
  
  ## Step: calculate the max to be shown in yaxis
  #------------------------------------------------#
  
  ## note that dtick is calculated based on range.base, which is always less than or equal to range
  ## so the no. of ticks needed in the yaxis will be greater than or equal to init.dtick.ratio
  ## adjust the no. of ticks through max value in yaxis to ensure all the values are shown
  ## and to ensure the ticks coudl be match between primary and secondary yaxis
  y1.dtick.ratio = (y1.max - y1.axis.min)/y1.dtick ## >= init.dtick.ratio
  y2.dtick.ratio = (y2.max - y2.axis.min)/y2.dtick ## >= init.dtick.ratio
  
  ## take the larger of two, and increased by 5% to leave some margin on the plot
  max.dtick.ratio = round2(max(y1.dtick.ratio, y2.dtick.ratio)) * (1+margin.pct)
  
  ## determine the max values in yaxis based on max.dtick.ratio
  y1.axis.max = max.dtick.ratio * y1.dtick + y1.axis.min
  y2.axis.max = max.dtick.ratio * y2.dtick + y2.axis.min
  
  #------------------------------------------------#
  ## End of Step;
  
  
  ## output the results
  return(list(y1 = list(range = list(y1.axis.min, y1.axis.max),
                        dtick = y1.dtick),
              y2 = list(range = list(y2.axis.min, y2.axis.max),
                        dtick = y2.dtick)))
  
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





## Function: This function is to delete duplicate legend in subplots of plotly
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
Dedup_Legend_Plotly <- function(fig) {
  
  ## required library
  require(plotly)
  require(plotly)
  
  # Get the names of the legend entries
  Tmp.df.Legend <- data.frame(id = seq_along(fig$x$data), 
                              legend_entries = unlist(lapply(fig$x$data, `[[`, "name"))) %>%
    # Extract the group identifier
    mutate(legend_group = gsub("^\\((.*?),\\d+\\)", "\\1", legend_entries)) %>%
    # Add an indicator for the first entry per group
    mutate(is_first = !duplicated(legend_group))
  
  ## loop through all the traces
  for (i in Tmp.df.Legend$id) {
    
    # Assign the group identifier to the name and legendgroup arguments
    fig$x$data[[i]]$name <- Tmp.df.Legend$legend_group[[i]]
    fig$x$data[[i]]$legendgroup <- fig$x$data[[i]]$name
    
    # Is the layer the first entry of the group?
    is_first <- Tmp.df.Legend$is_first[[i]]
    # Show the legend only for the first layer of the group 
    if (!is_first) fig$x$data[[i]]$showlegend <- FALSE
  }
  
  return(fig)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





## Function: This function is used to plot the fitted trend
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
FittedTrendPlot.Plotly <- function(data, col_seg, col_bal, col_date, col_trend, col_version = NULL,
                                   plot_end_dt = NULL, master_lookup = master.lkp, 
                                   knot_set_cur = 'knots', knot_set_last = NULL,
                                   out_dir = NULL) {
  
  ## load required package
  require(data.table)
  require(plotly)
  require(lubridate)
  require(ValidationFuncs)  ## self-defined package
  
  ## load customized color
  create.validation.cols()
  
  ## reformat the data to data table and keep only the necessary field
  tmp_keep_var = c(col_seg, col_bal, col_trend, col_date, col_version)
  tmp_data_1 = data.table(data)[, ..tmp_keep_var]
  
  ## rename the columns to facilitate the following calculation
  setnames(x = tmp_data_1, 
           old = c(col_seg, col_bal, col_trend, col_date), 
           new = c('SEG', 'BALANCE', 'Fitted_Trend', 'DATE'))
  
  ## Add a temp variable if col_version is not given to facilitate the process
  if (is.null(col_version)) {
    tmp_data_1$Version = 'Trend'
  } else {
    setnames(x = tmp_data_1, old = col_version, new = 'Version')
    tmp_data_1[, Version := paste0('TREND_', Version)]
  }
  
  ## transpose data from wide to long
  tmp_data_2 = dcast(data = tmp_data_1, formula = SEG + DATE + BALANCE ~ Version, value.var = 'Fitted_Trend')
  
  ## transpose data from long to wide
  tmp_data_3 = melt(tmp_data_2, id.vars = c('SEG', 'DATE'), value.name = 'BALANCE', variable.name = 'TYPE')
  
  ## get the list of the unique segment
  tmp_seg_list = unique(tmp_data_2$SEG)
  
  ## create plots
  FittedTrendPlot = lapply(
    tmp_seg_list, 
    function(sSeg) {
      
      ## debug only
      # sSeg = tmp_seg_list[1]
      
      ## determine the starting and ending date of plot
      tmp_start_dt = master_lookup[[sSeg]][["start.date"]]  ## extract the fitting start date from the master lookup table
      tmp_end_dt = as.Date(if_else(is.null(plot_end_dt), max(tmp_data_2$DATE), plot_end_dt))
      
      ## extract current knots from the master lookup table
      fit_knots_cur = master_lookup[[sSeg]][[knot_set_cur]]  ## current set of knots
      fit_knots_cur = fit_knots_cur[fit_knots_cur < tmp_end_dt]
      
      ## extract last knots
      if (is.null(knot_set_last)) {
        fit_knots_last = fit_knots_cur
      } else {
        fit_knots_last = master_lookup[[sSeg]][[knot_set_last]]  ## current set of knots
        fit_knots_last = fit_knots_last[fit_knots_last < tmp_end_dt]
      }
      
      ## find the difference between these knots
      knots_diff_delete = as.Date(setdiff(as.character(sort(fit_knots_last)), as.character(sort(fit_knots_cur))))
      knots_diff_add = as.Date(setdiff(as.character(sort(fit_knots_cur)), as.character(sort(fit_knots_last))))
      knots_common = as.Date(intersect(as.character(sort(fit_knots_last)), as.character(sort(fit_knots_cur))))
      
      ## grid.expand the date to ensure missing dates are also in the plot
      date_exp = data.table(stringsAsFactors = FALSE, DATE = seq(from = tmp_start_dt, to = tmp_end_dt, by = 1))
      
      ## extract the data for each segment 
      tmp_data_4 = merge(x = date_exp, y = tmp_data_3[SEG == sSeg], by = 'DATE', all.x = TRUE)[order(DATE)]
      
      ## Simplify the segment for titling
      if (str_detect(string = toupper(sSeg), pattern = 'INSURED|OPERATING')) {
        short_seg = trimws(word(string = sSeg, sep = ';', start = -2, end = -1)) 
      } else { 
        short_seg = trimws(word(string = sSeg, sep = ';', start = -1)) 
      }
      
      ## list of vertical lines for knots
      tmp.list.knots = lapply(knots_common, vline, color = FRB_theme_color['Clay_Green'])
      if (length(knots_diff_delete) > 0) {
        tmp.list.knots = c(tmp.list.knots,
                           lapply(knots_diff_delete, vline, color = FRB_theme_color['Light_Grey']))
      }
      if (length(knots_diff_add) > 0) {
        tmp.list.knots = c(tmp.list.knots,
                           lapply(knots_diff_add, vline, color = FRB_theme_color['Clay_Tan']))
      }
      
      ## create plot
      p <- plot_ly(data = tmp_data_4, x= ~DATE, y = ~BALANCE, type = 'scatter', mode = 'lines', color = ~TYPE, 
                   colors = unname(FRB_theme_color[c('Gold', 'Clay_Blue', 'Green', 'Clay_Grey', 'Clay_Brown')])) %>%
        layout(shapes = tmp.list.knots,
               title = list(text = short_seg, standoff = 10),
               legend = list(orientation = 'h'))
      
      ## save the plot if output directory is give
      if (!is.null(out_dir)) {
        
        ## remove special characters that are not allowed in names
        tmp.short.seg = str_replace(string = short_seg, pattern = '[[:punct:]]', replacement = '-')
        
        ## save the plot 
        htmlwidgets::saveWidget(p, file = file.path(out_dir, paste0(tmp.short.seg, ".html")))
        
        
      }
      
      ## return plot
      return(p)
      
    })
  
  ## assign names to the lsit
  names(FittedTrendPlot) = tmp_seg_list
  
  ## return the plot
  return(FittedTrendPlot)
  
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





## Function: This function plots delta over time with key statistics
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
TS.Delta.Plotly <- function(data, col_seg, col_horizon, col_date, col_delta, col_coef, return_period = return_period, col_version = NULL, out_dir = NULL) {
  
  ## load required package
  require(data.table)
  require(plotly)
  require(ggplot2)
  require(lubridate)
  require(ValidationFuncs)  ## self-defined package
  
  ## load customized color
  create.validation.cols()
  
  ## reformat the data to data table and keep only the necessary field
  tmp_keep_var = c(col_seg, col_horizon, col_date, col_delta, col_coef, col_version)
  tmp_data_1 = data.table(data)[, ..tmp_keep_var]
  
  ## rename the columns to facilitate the following calculation
  setnames(x = tmp_data_1, 
           old = c(col_seg, col_horizon, col_date, col_delta, col_coef), 
           new = c('SEG', 'HORIZON', 'DATE', 'DELTA', 'COEF'))
  
  ## Add a temp variable if col_version is not given to facilitate the process
  if (is.null(col_version)) {
    tmp_data_1$VERSION = 'Initial Coef'
  } else {
    setnames(x = tmp_data_1, old = col_version, new = 'VERSION')
    tmp_data_1[, VERSION := paste0('Initial Coef - ', VERSION)]
  }
  
  ## remove missing value
  tmp_data_2 = tmp_data_1[!is.na(DELTA)]
  tmp_data_2[, ':=' (Delta_Max = max(DELTA),
                     EMP_COEF = quantile(DELTA, probs = 1- 1/return_period/365.25),
                     Delta_p9995 = quantile(DELTA, probs = 0.9995),
                     Delta_p995 = quantile(DELTA, probs = 0.995),
                     Delta_p99 = quantile(DELTA, probs = 0.99)),
             by = .(SEG, HORIZON, VERSION, COEF)]
  
  ## get the list of the unique segment
  tmp_seg_list = unique(tmp_data_2$SEG)
  
  ## loop through all segment to plot
  DeltaTSPlot = lapply(
    tmp_seg_list, 
    function(sSeg) {
      
      ## debug only
      # sSeg = tmp_seg_list[1]
      
      ## extract data 
      tmp_data_3 = tmp_data_2[SEG == sSeg]
      
      ## Simplify the segment for titling
      if (str_detect(string = toupper(sSeg), pattern = 'INSURED|OPERATING')) {
        short_seg = trimws(word(string = sSeg, sep = ';', start = -2, end = -1)) 
      } else { 
        short_seg = trimws(word(string = sSeg, sep = ';', start = -1)) 
      }
      
      ## get list of version 
      tmp.list.version = unique(tmp_data_3$VERSION)
      
      ## separate delta and coefficient
      tmp_data_delta = unique(tmp_data_3[, .(SEG, HORIZON, DATE, DELTA, Delta_Max, EMP_COEF, Delta_p9995, Delta_p995, Delta_p99)])
      tmp_data_coef = tmp_data_3[, .(SEG, HORIZON, DATE, COEF, VERSION)]
      
      ## create plot
      p <- ggplot(data = tmp_data_delta, mapping = aes(x = DATE)) +
        geom_line(mapping = aes(y = DELTA, color = 'Actual')) +
        geom_line(mapping = aes(y = Delta_Max, color = 'Actual_Max', linetype = 'Actual_Max')) +
        geom_line(mapping = aes(y = Delta_p995, color = 'Actual_P99.5', linetype = 'Actual_P99.5')) +
        geom_line(mapping = aes(y = Delta_p99, color = 'Actual_P99', linetype = 'Actual_P99')) +
        geom_line(mapping = aes(y = EMP_COEF, color = 'Actual_7yr', linetype = 'Actual_7yr')) +
        geom_line(data = tmp_data_coef, mapping = aes(y = COEF, color = VERSION)) +
        scale_color_manual(values = unname(c(FRB_theme_color['Gold'], FRB_theme_color['Light_Gold'], rep(FRB_theme_color['Clay_Grey'], 3), FRB_Greens[c(6, 4, 2)]))) + 
        scale_y_continuous(labels = percent_format(accuracy = 0.1))+
        facet_wrap(~HORIZON, scales = 'free_y', ncol = 2) +
        theme(legend.title = element_blank()) +
        labs(title = short_seg)
      
      
      ## convert it to plotly
      ply <- Dedup_Legend_Plotly(ggplotly(p)) 
      
      ## save plot
      if (!is.null(out_dir)) {
        tmp_PlotSuffix = paste0(str_replace(string = short_seg, pattern = '[[:punct:]]', replacement = '-'),'.html')
        htmlwidgets::saveWidget(ply, file = file.path(out_dir, paste("Backtesting LT Coef -", tmp_PlotSuffix)))
      }
      
      ## output the results
      return(ply)
      
    })
  
  ## add the names to the list
  names(DeltaTSPlot) = tmp_seg_list
  
  ## output the results
  return(DeltaTSPlot)
  
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;




## Function: This function is used to analyze threshold for one segment, one horizon, and one given threshold
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ThresholdSelection.One <- function(aDelta, nTH, method = 'normal', R = 200, alpha = 0.05) {
  
  ## require library
  require(extRemes)
  
  ## fit GPD
  tmp.fit <- fevd(x = aDelta, threshold = nTH, type = 'GP')
  
  
  ## extract excess delta
  Tmp.ExcessDelta = sort(aDelta[aDelta > nTH])
  
  ## QQ plot
  Tmp.p.Emp = ppoints(Tmp.ExcessDelta)
  Tmp.Scale = tmp.fit$results$par[['scale']]
  Tmp.Shape = tmp.fit$results$par[['shape']]
  Tmp.QQ.Fit = ((1-Tmp.p.Emp)**(-Tmp.Shape) - 1) * Tmp.Scale / Tmp.Shape + nTH  ## match with the results qevd
  ## using this just to see if the calculation could be a bit faster
  
  
  ## MSE 
  if (method == 'normal') {
    
    ## calculate the parameter covariance matrix
    tmp.MSE <- parcov.fevd(tmp.fit)
    
    ## calculate Z based on alpha
    z.alpha = abs(qnorm(1 - alpha/2))
    
    ## calculate lower and upper bound
    if (is.null(tmp.MSE)) {
      Scale.LB = NA
      Scale.UB = NA
      Shape.LB = NA
      Shape.UB = NA
    } else {
      SE.Scale = tmp.MSE['scale', 'scale']**0.5
      SE.Shape = tmp.MSE['shape', 'shape']**0.5
      Scale.LB = Tmp.Scale - z.alpha * SE.Scale
      Scale.UB = Tmp.Scale + z.alpha * SE.Scale
      Shape.LB = Tmp.Shape - z.alpha * SE.Shape
      Shape.UB = Tmp.Shape + z.alpha * SE.Shape
    }
    
  } else {
    
    ## set seed
    set.seed(314159)
    tmp_ci <- ci(x = tmp.fit, alpha = alpha, type = 'parameter', method = method, R = R)
    Scale.LB = tmp_ci['scale', 1]
    Scale.UB = tmp_ci['scale', 3]
    Shape.LB = tmp_ci['shape', 1]
    Shape.UB = tmp_ci['shape', 3]
    
  }
  
  ## combine results and output
  return(data.table(stringsAsFactors = FALSE,
                    Threshold = nTH,
                    ## QQ plot
                    QQ.RSQ = cor(Tmp.QQ.Fit, Tmp.ExcessDelta)**2,
                    QQ.Beta = cov(Tmp.QQ.Fit, Tmp.ExcessDelta) / var(Tmp.QQ.Fit), ## x: fitted; y: empirical
                    ## MSE Plot
                    Rate = tmp.fit$rate,
                    Scale = Tmp.Scale, 
                    Shape = Tmp.Shape,
                    Scale.LB = Scale.LB,
                    Scale.UB = Scale.UB,
                    Shape.LB = Shape.LB,
                    Shape.UB = Shape.UB
                    
  ))
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





## Function: This function is used to analyze threshold
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ThresholdSelection <- function(data, col_delta, col_threshold, col_seg, col_horizon, range_Th = c(0.75, 0.96), 
                               alpha = 0.05, method = 'boot', nint = 22, out_dir = NULL, R = 200) {
  
  ## load required package
  require(data.table)
  require(plotly)
  require(dplyr)
  require(scales)
  require(ValidationFuncs) ## self-defined package
  
  ## load customized color
  create.validation.cols()
  
  ## reformat the data to data table and keep only the necessary field
  tmp_keep_var = c(col_delta, col_threshold, col_seg, col_horizon)
  tmp_data_1 = as.data.table(data)[, ..tmp_keep_var]
  
  ## rename the columns to faciliate the following calculation
  setnames(x = tmp_data_1, old = tmp_keep_var, new = c('Delta', 'Threshold', 'SEG', 'Horizon'))
  
  ## get the list of the unique segment and horizon
  tmp_seg_list = unique(tmp_data_1$SEG)
  tmp_hor_list = unique(tmp_data_1$Horizon)
  
  ## calculate Z based on alpha
  z.alpha = abs(qnorm(1 - alpha/2))
  
  
  ## loop through all the segment
  Tmp.Res.Seg = lapply(
    tmp_seg_list, 
    function(sSeg) {
      
      
      ## Simplify the segment for titling
      if (str_detect(string = toupper(sSeg), pattern = 'INSURED|OPERATING')) {
        
        short_seg = trimws(word(string = sSeg, sep = ';', start = -2, end = -1)) 
        
      } else { 
        short_seg = trimws(word(string = sSeg, sep = ';', start = -1)) 
      }
      
      ## show the progress
      writeLines(paste('Processing Segment:', short_seg))
      
      Tmp.Res.Hor = lapply(
        tmp_hor_list, 
        function(sHorizon){
          
          ## show the progress
          writeLines(paste('Processing Horizon:', sHorizon))
          
          ## debug only
          # sSeg = tmp_seg_list[1]
          # sHorizon = tmp_hor_list[1]
          
          ## extract the data
          tmp_data_2 = tmp_data_1[SEG == sSeg & Horizon == sHorizon & !is.na(Delta)]
          
          ## extract current threshold & Delta
          curr_th = unique(tmp_data_2$Threshold)
          Tmp.Delta = sort(tmp_data_2$Delta)
          
          ## list of thresholds to be analyze
          tmp_th_list = quantile(Tmp.Delta, seq(from = range_Th[1], to = range_Th[2], length.out = nint))
          
          
          ## Step: Mean Excess/Mean Residual Life plot
          #------------------------------------------------#
          ## for each threshold > min threshold to be analyzed, calculate mean excess
          tmp.ME.TH = unique(sort(Tmp.Delta[Tmp.Delta > tmp_th_list[1]]))
          
          ## remove last 2 (need at least 2 to calculate variance)
          tmp.ME.TH = tmp.ME.TH[1: (length(tmp.ME.TH) - 2)]
          
          ## calculate key statistics
          tmp.ME.N = sapply(tmp.ME.TH, function(nTH) sum(Tmp.Delta > nTH))
          tmp.ME.mean = sapply(tmp.ME.TH, function(nTH) mean(Tmp.Delta[Tmp.Delta > nTH]) - nTH)
          tmp.ME.var = sapply(tmp.ME.TH, function(nTH) var(Tmp.Delta[Tmp.Delta > nTH]))
          tmp.ME.SE = sqrt(tmp.ME.var/tmp.ME.N)
          
          
          ## measure the linearity of mean excess 
          ## 1) use corr(mean excess, threshold)
          ## 2) regress Mean Excess ~ threshold^2 + threshold, and use the coefficient of quadratic term 
          # tmp.beg <- Sys.time()
          tmp.ME.TH.2 = tmp.ME.TH ** 2
          
          ## calculate the length of tmp.ME.TH
          Tmp.ME.Len = length(tmp.ME.TH)
          
          ## loop through min threshold to max threshold
          ## note that tmp.ME.TH has been ordered from low to high
          ## and that the first one is the min threshold
          ## so the position of max threshold in tmp.ME.TH should be sum(tmp.ME.TH<= tmp_th_list[length(tmp_th_list)])
          # tmp.beg <- Sys.time()
          tmp.ME.Lim = lapply(
            1:sum(tmp.ME.TH<= tmp_th_list[length(tmp_th_list)]), 
            function(nRow) {
              ## debug only
              # nRow = 1
              
              ## regress Mean Excess ~ threshold^2 + threshold
              tmp.fit.lim = lm(formula = tmp.ME.mean[nRow:Tmp.ME.Len] ~ tmp.ME.TH.2[nRow:Tmp.ME.Len] + tmp.ME.TH[nRow:Tmp.ME.Len])
              
              return(data.table(stringsAsFactors = FALSE,
                                Threshold = tmp.ME.TH[nRow],
                                ME.Corr = cor(tmp.ME.mean[nRow:Tmp.ME.Len], tmp.ME.TH[nRow:Tmp.ME.Len]),
                                ME.qr.Coef = tmp.fit.lim$coefficients['tmp.ME.TH.2[nRow:Tmp.ME.Len]']
              ))
            })
          # tmp.end <- Sys.time()
          # tmp.end - tmp.beg
          
          ## combine all results
          ME.Lim = do.call(bind_rows, tmp.ME.Lim)
          RES.ME = data.table(stringsAsFactors = FALSE,
                              N = tmp.ME.N,
                              Threshold = tmp.ME.TH,
                              ExcessMean = tmp.ME.mean,
                              ExcessMean.SE = tmp.ME.SE)
          RES.ME = merge(x = RES.ME, y = ME.Lim, by = 'Threshold', all.y = TRUE)
          
          ## add hover text
          RES.ME[, hover.MRL.CI := paste0('CI: [', percent(ExcessMean - z.alpha * ExcessMean.SE, accuracy = 0.01),
                                          ', ', percent(ExcessMean + z.alpha * ExcessMean.SE, accuracy = 0.01), ']')]
          
          
          ## MRL or ME plot
          p1 <- plot_ly(data = RES.ME, x = ~Threshold) %>%
            add_lines(y = ~ ExcessMean, line = list(color = FRB_theme_color['Green']),
                      hovertemplate = paste('<b>Excess Mean</b>: %{y:.1%}',
                                            '<br><b>Threshold</b>: %{x: .4%}<br><extra></extra>'),
                      showlegend = F) %>%
            add_ribbons(ymin = ~ExcessMean - z.alpha * ExcessMean.SE,  ymax = ~ExcessMean + z.alpha * ExcessMean.SE,
                        text = ~hover.MRL.CI,
                        hovertemplate = paste('%{text}',
                                              '<br>Threshold: %{x: .4%}<br><extra></extra>'),
                        line = list(color = FRB_theme_color['Green'], dash = 'dash'), 
                        fillcolor = FRB_theme_color['Clay_Green'], opacity = 0.2,
                        name = paste(percent(max(1-alpha, alpha)), 'CI'),
                        showlegend = F) %>%
            add_lines(y = ~ ME.Corr, line = list(color = FRB_theme_color['Clay_Blue'], dash = 'dot'),
                      hovertemplate = paste('<b>Corr</b>: %{y:.1%}',
                                            '<br><b>Threshold</b>: %{x: .4%}<br><extra></extra>'),
                      yaxis = "y2",
                      showlegend = F) %>%
            layout(
              yaxis = list(tickformat = ".2%", title = list(text = 'Mean Excess', standoff = 10)),
              xaxis = list(tickformat = ".2%", title = list(text = 'Threshold', standoff = 20)),
              yaxis2 = list(tickformat = ".1%", title = list(text = 'Pearson Correlation', standoff = 10),
                            zerolinecolor = FRB_theme_color['Light_Grey'], zerolinewidth = 1,
                            overlaying = "y",side = "right", showgrid = F),
              margin = list(r=50),
              shapes = list(vline(curr_th))
            )  %>%
            add_text(showlegend = FALSE, x = curr_th - abs(curr_th )* 0.05, y = max(RES.ME$ExcessMean + z.alpha * RES.ME$ExcessMean.SE), 
                     text = paste0('Current TH: ', percent(curr_th, accuracy = 0.0001)),
                     textposition = 'middle left',
                     textfont = list(color = FRB_theme_color['Gold']),
                     hoverinfo='skip') %>%
            add_annotations(
              text = ~ 'Mean Excess Plot',
              x = 0.5,
              y = 1,
              yref = "paper",
              xref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              bgcolor= unname(FRB_theme_color['Light_Grey']),
              font = list(size = 15),
              showarrow = FALSE
            )
          
          #------------------------------------------------#
          ## End of Step;
          
          
          
          ## Step: QQ plots, MSE 
          #------------------------------------------------#
          ## loop through all the thresholds
          # tmp.beg <- Sys.time()
          tmp.Res.TH = lapply(tmp_th_list, ThresholdSelection.One, aDelta = Tmp.Delta, alpha = alpha, method = method, R = R) 
          # tmp.end <- Sys.time()
          # tmp.end - tmp.beg
          
          ## combine all results
          Res.TH = do.call(bind_rows, tmp.Res.TH)
          
          ## CI of Scale
          Res.TH[, hover.MSE.Scale := paste0('CI: [', percent(Scale.LB, accuracy = 0.001),
                                             ', ', percent(Scale.UB, accuracy = 0.001), ']')]
          ## CI of Shape
          Res.TH[, hover.MSE.Shape := paste0('CI: [', percent(Shape.LB, accuracy = 0.01),
                                             ', ', percent(Shape.UB, accuracy = 0.01), ']')]
          
          
          
          ## GoF of QQ plot
          p2 <- plot_ly(data = Res.TH, x = ~Threshold) %>%
            add_lines(y = ~ QQ.RSQ, line = list(color = FRB_theme_color['Green']),
                      hovertemplate = paste('<b>RSQ</b>: %{y:.1%}',
                                            '<br><b>Threshold</b>: %{x: .4%}<br><extra></extra>'),
                      showlegend = F) %>%
            add_lines(y = ~ QQ.Beta, line = list(color = FRB_theme_color['Clay_Blue']),
                      hovertemplate = paste('<b>Beta</b>: %{y:.1%}',
                                            '<br><b>Threshold</b>: %{x: .4%}<br><extra></extra>'),
                      showlegend = F) %>%
            layout(
              yaxis = list(tickformat = ".2%", title = list(text = 'RSQ/Beta', standoff = 10)),
              xaxis = list(tickformat = ".2%", title = list(text = 'Threshold', standoff = 20)),
              shapes = list(vline(curr_th))
            )  %>%
            add_text(showlegend = FALSE, x = curr_th - abs(curr_th )* 0.05, y = max(Res.TH$QQ.RSQ, Res.TH$QQ.Beta), 
                     text = paste0('Current TH: ', percent(curr_th, accuracy = 0.0001)),
                     textposition = 'middle left',
                     textfont = list(color = FRB_theme_color['Gold']),
                     hoverinfo='skip') %>%
            add_annotations(
              text = ~ 'GoF of QQ Plot',
              x = 0.5,
              y = 1,
              yref = "paper",
              xref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              bgcolor= unname(FRB_theme_color['Light_Grey']),
              font = list(size = 15),
              showarrow = FALSE
            )
          
          
          ## MSE - Scale
          p3 <- plot_ly(data = Res.TH, x = ~Threshold) %>%
            add_lines(y = ~ Scale, line = list(color = FRB_theme_color['Green']),
                      hovertemplate = paste('<b>Scale</b>: %{y:.3%}',
                                            '<br><b>Threshold</b>: %{x: .4%}<br><extra></extra>'),
                      showlegend = F) %>%
            add_ribbons(ymin = ~Scale.LB,  ymax = ~ Scale.UB,
                        text = ~hover.MSE.Scale,
                        hovertemplate = paste('%{text}',
                                              '<br>Threshold: %{x: .4%}<br><extra></extra>'),
                        line = list(color = FRB_theme_color['Green'], dash = 'dash'), 
                        fillcolor = FRB_theme_color['Clay_Green'], opacity = 0.2,
                        name = paste(percent(max(1-alpha, alpha)), 'CI'),
                        showlegend = F) %>%
            layout(
              yaxis = list(tickformat = ".2%", title = list(text = 'Scale', standoff = 10)),
              xaxis = list(tickformat = ".2%", title = list(text = 'Threshold', standoff = 20)),
              # margin = list(l=50),
              shapes = list(vline(curr_th))
            )  %>%
            add_text(showlegend = FALSE, x = curr_th - abs(curr_th )* 0.05, y = max(Res.TH$Scale.UB, na.rm = TRUE), 
                     text = paste0('Current TH: ', percent(curr_th, accuracy = 0.0001)),
                     textposition = 'middle left',
                     textfont = list(color = FRB_theme_color['Gold']),
                     hoverinfo='skip') %>%
            add_annotations(
              text = ~ 'Parameter MSE Plot - Scale',
              x = 0.5,
              y = 1,
              yref = "paper",
              xref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              bgcolor= unname(FRB_theme_color['Light_Grey']),
              font = list(size = 15),
              showarrow = FALSE
            )
          
          
          
          ## MSE - Shape
          p4 <- plot_ly(data = Res.TH, x = ~Threshold) %>%
            add_lines(y = ~ Shape, line = list(color = FRB_theme_color['Green']),
                      hovertemplate = paste('<b>Shape</b>: %{y:.3%}',
                                            '<br><b>Threshold</b>: %{x: .4%}<br><extra></extra>'),
                      showlegend = F) %>%
            add_ribbons(ymin = ~Shape.LB,  ymax = ~ Shape.UB,
                        text = ~hover.MSE.Shape,
                        hovertemplate = paste('%{text}',
                                              '<br>Threshold: %{x: .4%}<br><extra></extra>'),
                        line = list(color = FRB_theme_color['Green'], dash = 'dash'), 
                        fillcolor = FRB_theme_color['Clay_Green'], opacity = 0.2,
                        name = paste(percent(max(1-alpha, alpha)), 'CI'),
                        showlegend = F) %>%
            layout(
              yaxis = list(tickformat = ".2%", title = list(text = 'Shape', standoff = 10)),
              xaxis = list(tickformat = ".2%", title = list(text = 'Threshold', standoff = 20)),
              # margin = list(l=50),
              shapes = list(vline(curr_th))
            )  %>%
            add_text(showlegend = FALSE, x = curr_th - abs(curr_th )* 0.05, y = max(Res.TH$Shape.UB, na.rm = TRUE), 
                     text = paste0('Current TH: ', percent(curr_th, accuracy = 0.0001)),
                     textposition = 'middle left',
                     textfont = list(color = FRB_theme_color['Gold']),
                     hoverinfo='skip') %>%
            add_annotations(
              text = ~ 'Parameter MSE Plot - Shape',
              x = 0.5,
              y = 1,
              yref = "paper",
              xref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              bgcolor= unname(FRB_theme_color['Light_Grey']),
              font = list(size = 15),
              showarrow = FALSE
            )
          
          #------------------------------------------------#
          ## End of Step;
          
          
          
          ## put all the plot on the same page
          p <- subplot(p1, p3, p2, p4, shareX = TRUE, titleY = TRUE, nrows = 2, margin = 0.045) %>%
            layout(title = paste0('<b>', short_seg, ' - ', sHorizon, 'D Horizon</b>'), margin = list(t = 75))
          
          ## save the results
          if (!is.null(out_dir)) {
            tmp_PlotSuffix = paste0(str_replace(string = short_seg, pattern = '[[:punct:]]', replacement = '-'), ' - ', sHorizon ,'.html')
            htmlwidgets::saveWidget(p, 
                                    file = file.path(out_dir, paste("Threshold Analysis -", tmp_PlotSuffix)))
          }
          
          ## output the results
          return(p)
        })
      
      ## add the names to the list
      names(Tmp.Res.Hor) = tmp_hor_list
      
      ## insert an empty line
      writeLines('')
      
      ## output the results
      return(Tmp.Res.Hor)
      
    })
  
  ## add the names to the list
  names(Tmp.Res.Seg) = tmp_seg_list
  
  ## output the results
  return(Tmp.Res.Seg)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;






## Function: This function is to analyze the sensitivity of threshold to initial coefficient
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
SA.Threshold.Plotly <- function(data, col_seg, col_horizon, col_delta, col_threshold, range_Th = c(0.75, 0.95), 
                                return_period = 7, out_dir = NULL) {
  
  
  ## load required package
  require(data.table)
  require(plotly)
  require(lubridate)
  require(ValidationFuncs)  ## self-defined package
  
  ## load customized color
  create.validation.cols()
  
  ## reformat the data to data table and keep only the necessary field
  tmp_keep_var = c(col_delta, col_threshold, col_seg, col_horizon)
  tmp_data_1 = as.data.table(data)[, ..tmp_keep_var]
  
  ## rename the columns to faciliate the following calculation
  setnames(x = tmp_data_1, old = tmp_keep_var, new = c('Delta', 'Threshold', 'SEG', 'Horizon'))
  
  ## get the list of the unique segment and horizon
  tmp_seg_list = unique(tmp_data_1$SEG)
  tmp_hor_list = unique(tmp_data_1$Horizon)
  
  ## create the plot
  ## loop through all segments
  Tmp.Res.Seg = lapply(
    tmp_seg_list, 
    function(sSeg) {
      
      ## display the progress
      # if (bProgress) writeLines(paste('Processing Segment:', sSeg))
      
      ## loop through all horizons
      Tmp.Res.Horizon = lapply(
        tmp_hor_list, 
        function (sHorizon) {
          
          ## display the progress
          # if (bProgress) writeLines(paste('Processing Horizon:', sHorizon))
          
          ## debug only
          # sSeg = tmp_seg_list[1]
          # sHorizon = tmp_hor_list[4]
          
          ## extract the data
          tmp_data_2 = tmp_data_1[SEG == sSeg & Horizon == sHorizon & !is.na(Delta)]
          
          ## extract current threshold
          curr_th = unique(tmp_data_2$Threshold)
          
          ## list of thresholds to be analyze
          tmp_th_q = seq(from = range_Th[1], to = range_Th[2], by = 0.01)
          tmp_th_list = quantile(tmp_data_2$Delta, tmp_th_q)
          
          ## loop through all thresholds
          Tmp.Res.TH = lapply(
            1:length(tmp_th_list), 
            function(nTH) {
              
              ## debug only
              # nTH = 1
              
              ## fit GPD
              tmp.fit <- fevd(x = tmp_data_2$Delta, threshold = tmp_th_list[nTH], type = 'GP')
              
              ## collect results
              tmp.sa.res = data.table(stringsAsFactors = FALSE,
                                      SEG = sSeg,
                                      Horizon = sHorizon,
                                      Current_TH = curr_th,
                                      Threshold = tmp_th_list[nTH],
                                      TH_q = names(tmp_th_list)[nTH],
                                      Init_OutCoef = return.level(tmp.fit, return_period)
              )
              
              ## output the results
              return(tmp.sa.res)
            })
          
          ## combine all the results and output
          return(do.call(rbind, Tmp.Res.TH))
          
        })
      
      ## combine all the results
      Res.Horizon = copy(bind_rows(Tmp.Res.Horizon))
      
      ## Simplify the segment for titling
      if (str_detect(string = toupper(sSeg), pattern = 'INSURED|OPERATING')) {
        short_seg = trimws(word(string = sSeg, sep = ';', start = -2, end = -1)) 
      } else { 
        short_seg = trimws(word(string = sSeg, sep = ';', start = -1)) 
      }
      
      ## cap the intial coefficient at 1 for plotting purpose
      Res.Horizon[, Init_OutCoef_Capped := pmin(Init_OutCoef, 1)]
      Res.Horizon[, hover.text := paste0('<b>Initial Coef</b>: ', percent(as.numeric(Init_OutCoef), accuracy = 0.01),
                                         '<br><b>Threshold Quantile</b>: ', TH_q)]
      
      
      ## plotly template
      tmp.plotly <- . %>% 
        plot_ly(., x = ~Threshold) %>%
        add_lines(y = ~Init_OutCoef_Capped, name = "Initial LT Outflow Coefficient", showlegend = FALSE, text = ~hover.text,
                  hovertemplate = paste('%{text}',
                                        '<br><b>Threshold Vaue</b>: %{x:0.2%}<br><extra></extra>'), 
                  line = list(color = FRB_theme_color['Green'])) %>%
        add_annotations(showlegend = FALSE, 
                        x = ~unique(Current_TH), 
                        y = ~max(Init_OutCoef_Capped), 
                        text = ~unique(paste0('Current TH: ', percent(Current_TH, accuracy = 0.0001))),
                        xanchor="right",
                        font = list(color = FRB_theme_color['Gold']),
                        textangle=-90,
                        # arrowcolor = FRB_theme_color['Gold'],
                        hoverinfo='skip',
                        showarrow = FALSE) %>%
        layout(yaxis = list(tickformat = ".4%", zeroline = F, title = ''),
               xaxis = list(title = ''),
               shapes = list(vline(~unique(Current_TH)))) %>%
        add_annotations(
          text = ~ paste0('Horizon = ', unique(Horizon)),
          x = 0.5,
          y = 1,
          yref = "paper",
          xref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          bgcolor= unname(FRB_theme_color['Light_Grey']),
          # font = list(size = 15),
          showarrow = FALSE
        )
      
      ply <- Res.Horizon %>%
        group_by (Horizon) %>% 
        do(p = tmp.plotly(.)) %>%
        subplot(nrows = 2,  margin = c(0.03, 0.03, 0.05, 0.05)) %>%
        layout(
          title = list(text = short_seg),
          yaxis = list(title = list(text = '<b>Intial LT Outflow Coefficient</b>', standoff = 20)),
          xaxis5 = list(title = list(text = '<b>Threshold</b>', standoff = 25)),
          margin = list(t = 80)
        )
      
      
      ## save plot
      if (!is.null(out_dir)) {
        tmp_PlotSuffix = paste0(str_replace(string = short_seg, pattern = '[[:punct:]]', replacement = '-'), '.html')
        htmlwidgets::saveWidget(ply, file = file.path(out_dir, paste("Threshold SA -", tmp_PlotSuffix)))
      }
      
      ## inset an empty line between different segments
      # if (bProgress) writeLines("")
      
      ## return results
      return(list(data = Res.Horizon, plot = ply))
      
    })
  
  
  ## consolidate data and results
  Tmp.Res.Seg.Data = copy(bind_rows(lapply(Tmp.Res.Seg, '[[', 'data')))
  Tmp.Res.Seg.Plot = lapply(Tmp.Res.Seg, '[[', 'plot')
  
  ## rename the columns back
  setnames(x = Tmp.Res.Seg.Data, old = c('Threshold', 'SEG', 'Horizon'), new = c(col_threshold, col_seg, col_horizon))
  
  ## name the list
  names(Tmp.Res.Seg.Plot) = tmp_seg_list
  
  ## output result
  return(list(data = Tmp.Res.Seg.Data, plot = Tmp.Res.Seg.Plot))
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





## Function: This function is to plot actual vs fitted/selected overtime
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
BacktestingPlot <- function(data, col_seg = NULL, col_horizon, col_date, col_actual, col_pred, 
                            label_y, label_actual = NULL, label_pred = NULL, out_dir = NULL) {
  
  ## load required package
  require(data.table)
  require(ggplot2)
  
  ## load customerized color
  source('/mnt/dc1_mrm/fchai/General/FRB_Colors.R')
  
  ## reformat the data to data table and keep only the necessary field
  tmp_keep_var = c(col_horizon, col_date, col_actual, col_pred, col_seg)
  tmp_data_1 = data.table(data)[, ..tmp_keep_var]
  
  ## rename the columns to faciliate the following calculation
  setnames(x = tmp_data_1, old = tmp_keep_var[1:4], new = c('Horizon', 'DATE', 'Actual', 'Pred'))
  if (is.null(col_seg)) {
    tmp_data_1$SEG = 1
  } else {
    setnames(x = tmp_data_1, old = col_seg, new = 'SEG')
  }
  
  ## transpose data from wide to long
  tmp_data_2 = melt(tmp_data_1, id.vars = c('Horizon', 'DATE', 'SEG'))
  
  ## calculate average and max
  tmp_stats = tmp_data_2[, .(max = max(value, na.rm = TRUE),
                             min = min(value, na.rm = TRUE),
                             avg = mean(value, na.rm = TRUE),
                             x.start = min(DATE, na.rm = TRUE),
                             x.end = max(DATE, na.rm = TRUE)),
                         by = .(Horizon, SEG, variable)]
  
  
  ## transpose data from wide to long
  tmp_stats_trans = melt(tmp_stats, id.vars = c('Horizon', 'SEG', 'variable', 'x.start', 'x.end'),
                         variable.name = 'Stats',
                         value.name = 'y.pos')
  
  ## check whether the pred is unique
  bUniquePred = nrow(tmp_stats[variable == 'Pred']) == sum(tmp_stats[variable == 'Pred', .(tmp = (min == max & max == avg))]$tmp)
  
  
  ## finalize lables
  label_actual = ifelse(is.null(label_actual), 'Actual', label_actual)
  label_pred = ifelse(is.null(label_pred), 'Pred', label_pred)
  tmp_stats_trans[variable =='Actual', text.x.pos := x.start %m+% months(1)]
  tmp_stats_trans[variable !='Actual', text.x.pos := x.end %m-% months(1)]
  tmp_stats_trans[, y.just := ifelse(Stats =='avg' & variable == 'Actual', 1.5 , -1)]
  tmp_stats_trans[, Stats := as.character(Stats)]
  tmp_stats_trans[, text.labels := paste0(ifelse(variable == 'Actual' | !bUniquePred, Stats, ''),
                                          ifelse(variable == 'Pred', paste0(" ", label_pred), ''), 
                                          ': ', percent(y.pos, accuracy = 0.1))]
  
  ## keep desired stats
  tmp_stats_trans = tmp_stats_trans[(variable == 'Actual' | Stats == 'avg') &  Stats != 'min']
  
  ## list of segment
  tmp_seg_list = unique(tmp_data_2$SEG)
  
  ## create plot
  tmp.p = lapply(
    tmp_seg_list, 
    function(sSeg) {
      
      ## debug only
      # sSeg = tmp_seg_list[1]
      
      ## extract data
      tmp.dt.plot = tmp_data_2[SEG == sSeg]
      tmp.dt.stats = tmp_stats_trans[SEG == sSeg]
      
      ## Simplify the segment for titling
      if (str_detect(string = toupper(sSeg), pattern = 'INSURED|OPERATING')) {
        
        short_seg = trimws(word(string = sSeg, sep = ';', start = -2, end = -1)) 
        
      } else { 
        short_seg = trimws(word(string = sSeg, sep = ';', start = -1)) 
      }
      
      ## create plot
      p <- ggplot(data = tmp.dt.plot, mapping = aes(x = DATE, color = variable)) +
        geom_line(mapping = aes(y = value)) +
        scale_y_continuous(name = label_y, labels = percent_format(accuracy = 0.1)) + 
        geom_hline(data = tmp.dt.stats[variable == 'Actual'], mapping = aes(yintercept = y.pos, color = variable), linetype = 'dashed') + 
        geom_blank(data=tmp.dt.stats, aes(x=text.x.pos, y=y.pos*1.1, label=y.pos)) + 
        # geom_text(data = tmp.dt.stats, mapping = aes(x = text.x.pos, y=y.pos, color = variable, label = text.labels, vjust = y.just)) + 
        geom_text(data = tmp.dt.stats, mapping = aes(x = text.x.pos, y=y.pos, color = variable, label = text.labels, vjust = y.just),
                  hjust = 'inward') + 
        scale_color_manual(name = '', values = unname(FRB_theme_color[c('Gold', 'Green')]),  labels= c(label_actual, label_pred)) +
        theme(legend.position = 'bottom') + 
        facet_wrap(~ Horizon, scales = 'free_x')
      
      ## add title if col_seg is given
      if(!is.null(col_seg)) {
        p <- p + labs(title = short_seg)
      }
      
      ## save the plot 
      tmp_PlotSuffix = '.png'
      if(!is.null(col_seg)) {
        tmp_PlotSuffix = paste0(' - ', str_replace(string = short_seg, pattern = '[[:punct:]]', replacement = '-'), '.png')
      }
      
      ## save results in output path is given
      if (!is.null(out_dir)) {
        ggsave(filename = paste0(label_y, tmp_PlotSuffix), plot = p, 
               device = 'png', path = out_dir, width = 16, height = 9, units = 'in', dpi = 100)
      }
      
      
      ## return plot
      return(p)
    })
  
  ## return plot
  if (is.null(col_seg)) {
    return(tmp.p[[1]])
  } else {
    names(tmp.p) = tmp_seg_list
    return(tmp.p)
  }
  
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





## Function: This function is to compare modeled balance coefficient against actual balance coefficient
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
TS.BalCoef.Plotly <- function(data, col_seg, col_horizon, col_date, col_bal, col_ModelOutflow, return_period, dSnapshot = NULL, out_dir = NULL) {
  
  ## load required package
  require(data.table)
  require(plotly)
  require(scales)
  require(lubridate)
  require(ValidationFuncs)  ## self-defined package
  
  ## load customized color
  create.validation.cols()
  
  ## reformat the data to data table and keep only the necessary field
  tmp_keep_var = c(col_seg, col_horizon, col_date, col_bal, col_ModelOutflow)
  tmp_data_1 = as.data.table(data)[, ..tmp_keep_var]
  
  ## rename the columns to faciliate the following calculation
  setnames(x = tmp_data_1, old = tmp_keep_var, new = c('SEG', 'Horizon', 'Date', 'Balance', 'Outflow_Model'))
  
  ## aggregate to total deposit level
  tmp_data_2 = tmp_data_1[, .(SEG = 'Total Deposits',
                              Balance = sum(Balance),
                              Outflow_Model = sum(Outflow_Model),
                              OutCoef_Model = sum(Outflow_Model)/sum(Balance)),
                          by = .(Date, Horizon)]
  
  ## calculated actual balance outflow coefficient
  tmp_data_3 = CalDelta(data = unique(tmp_data_2[, .(Date, SEG, Balance)]), 
                        col_seg = 'SEG', 
                        col_bal = 'Balance', 
                        col_date = 'Date', 
                        horizons = unique(tmp_data_2$Horizon))
  
  ## add the actual coefficient back
  tmp_data_2 = merge(x = tmp_data_2, 
                     y = tmp_data_3, 
                     by = c('SEG', 'Balance', 'Date', 'Horizon'),
                     all.x = TRUE)
  
  ## rename the column to avoid confusion
  setnames(x = tmp_data_2, old = 'Delta', new = 'OutCoef_Actual')
  
  ## using the OutCoef_Model as of dep_end_dt as the final model result
  tmp_data_2[, ':=' (OutCoef_Model_Fnl = max(ifelse(Date == dSnapshot, OutCoef_Model, NA), na.rm = TRUE)),
             by = .(Horizon)]
  
  
  ## remove missing values
  tmp_data_4 = tmp_data_2[!is.na(OutCoef_Actual)]
  
  ## calculate stats
  tmp_data_4[, 
             ':='(max = max(OutCoef_Actual, na.rm = TRUE),
                  P99 = quantile(OutCoef_Actual, 0.99),
                  P995 = quantile(OutCoef_Actual, 0.995),
                  P_7y = quantile(OutCoef_Actual, 1-1/return_period/365.25)),
             by = 'Horizon']
  
  ## plotly template
  tmp.plotly <- . %>% 
    plot_ly(., x = ~Date) %>%
    add_lines(y = ~OutCoef_Actual, name = "Actual", 
              hovertemplate = paste('<b>Actual</b>: %{y:.1%}',
                                    '<br><b>Date</b>: %{x}<br><extra></extra>'), 
              line = list(color = FRB_theme_color['Gold'])) %>%
    add_lines(y = ~OutCoef_Model_Fnl, name = paste0('Model_', year(dSnapshot), 'Q', quarter(dSnapshot)), 
              hovertemplate = paste('<b>Model</b>: %{y:.1%}<extra></extra>'), 
              line = list(color = FRB_theme_color['Green'])) %>%
    add_lines(y = ~P_7y, name = "Worst in 7 Years", hovertemplate = 'Actual Worst in 7-yr: %{y:.1%}<extra></extra>',
              line = list(color = FRB_theme_color['Gold'], dash = 'dot')) %>% 
    add_lines(y = ~P995, name = "P_99.5", hovertemplate = '99.5%: %{y:.1%}<extra></extra>',
              line = list(color = FRB_theme_color['Clay_Grey'], dash = 'dash')) %>%
    add_lines(y = ~P99, name = "P_99", hovertemplate = '99%: %{y:.1%}<extra></extra>',
              line = list(color = FRB_theme_color['Light_Grey'], dash = 'dash')) %>%
    
    layout(yaxis = list(tickformat = ".1%", title = ''),
           xaxis = list(title = '')) %>%
    add_annotations(
      text = ~ paste0('Horizon = ', unique(Horizon)),
      x = 0.5,
      y = 1,
      yref = "paper",
      xref = "paper",
      xanchor = "center",
      yanchor = "bottom",
      bgcolor= unname(FRB_theme_color['Light_Grey']),
      # font = list(size = 15),
      showarrow = FALSE
    )
  
  
  ## create plot for each horizon
  tmp.plotly = tmp_data_4 %>%
    group_by (Horizon) %>% 
    do(p = tmp.plotly(.)) %>%
    subplot(nrows = 2, shareX = TRUE, shareY = TRUE) %>%
    layout(
      yaxis = list(title = list(text = '<b>Balance Outflow Coefficient</b>', standoff = 20)),
      xaxis2 = list(title = list(text = '<b>Date</b>', standoff = 25))
    )
  
  ## delete duplicate legend
  tmp.plotly = Dedup_Legend_Plotly(tmp.plotly)
  
  ## save results if path is given
  if (!is.null(out_dir)) {
    htmlwidgets::saveWidget(tmp.plotly, file = file.path(out_dir, "Balance Coefficient.html"))
  }
  
  ## output results
  return(tmp.plotly)
  
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;