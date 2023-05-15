######################################################################################################################################################
#----------------------------------------------------------------------------------------------------------------------------------------------------#
## Project: Validation Package for validation
## Script purpose: This script contains functions to generate plots that are commonly used in validation
## Date: 2022-10-17
## Author: fchai
#----------------------------------------------------------------------------------------------------------------------------------------------------#
######################################################################################################################################################





#' @title ggplots.extract.legend: extract legend
#'
#' @description This function extracts legends from ggplots.
#'
#' @param ggp (required) a ggplot object.
#'
#' @return legends
#'
#' @export
#'
#' @examples ## load data
#' data("midwest", package = "ggplot2")
#' ## create plot
#' p <- ggplot(midwest, aes(x=area, y=poptotal)) +
#' geom_point(aes(col=state, size=popdensity))
#' ## extract legend
#' ggplots.extract.legend(p)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ggplots.extract.legend <- function(ggp) {
  step1 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title find.axis.format: Find the number format
#'
#' @description This function finds the appropriate number format for axis in ggplot
#'
#' @param val (required) reference value based on which format will be found.
#'
#' @return returns a function with single parameter x, a numeric vector, that returns a character vector
#'
#' @export
#'
#' @examples ## if reference value <1, return percent format
#' find.axis.format(0.2)(0.3) \cr
#' ## if reference value >1, return number format
#' find.axis.format(1000000)(200000)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
find.axis.format <- function(val) {

  ## stop and output error when val == 0
  if (val == 0) {
    stop('val should not be 0')
  }

  ## count the number of integer digits
  tmp.n = floor(log10(abs(val)))

  ## count the number of digits after decimal
  tmp.m = ifelse(val %% 1 == 0, 0,
                 nchar(strsplit(as.character(format(val, scientific = F)), "\\.")[[1]][2]))

  ## find scale
  tmp.scale = 10**(as.integer((tmp.n-1)/3) * 3)

  ## suffix (or unit)
  tmp.suffix = ifelse(tmp.scale == 1000000000, 'BN',
                      ifelse(tmp.scale == 1000000, 'MM',
                             ifelse(tmp.scale == 1000, 'K', '')))

  ## accuracy
  tmp.acc = ifelse(val <= 1, max(min(10**(2-tmp.m), 10**(1+tmp.n)), 10**(tmp.n)),
                   ifelse(val < 10, min(0.1, max(0.01, 10**(-tmp.m))),
                          ifelse(tmp.n %% 3 == 1, 0.1, 1)))


  ## output the format
  if (val > 1) {
    return(scales::number_format(accuracy = tmp.acc, scale = 1/tmp.scale, suffix = tmp.suffix, big.mark = ','))
  } else {
    return(scales::percent_format(accuracy = tmp.acc))
  }


}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title ggplots.SVA.bin: Single Variable Analysis - Binned Plot
#'
#' @description This function plots average of dependent variable (y) versus binned independent variable (x), and creates bar plots for binned x
#'
#' @param df (required) data frame.
#' @param sDepVar (required) name of dependent variable (y) in the df; must be a string.
#' @param sIndVar (required) name of independent variable (x) in the df; must be a string.
#' @param sPredVar (optional) name of predicted dependent variable in the df; must be a string.
#' @param sWtVar (optional) name of weights in the df; must be a string.
#' @param iMaxBin (optional) name of maximum number of bins, default to be 10; must be an integer.
#' @param sTitle (optional) Title of plot; must be a string.
#' @param sYLab (optional) a label for the primary y axis, default to be the same as `sDepVar`; must be a string.
#' @param sY2Lab (optional) a label for the secondary y axis, default to be `Population` if `sWtVar` is NULL or `sWtVar` if it is provided; must be a string.
#' @param sXLab (optional) a label for the x axis, default to be the same as `sIndVar`; must be a string.
#' @param bEqualDist (optional) Boolean to decide whether the binning is based on equal frequency or equal distance; default to be FALSE (equal frequency).
#' @param bShowPct (optional) Boolean to decide whether percentage, instead of amount, should be displayed for the bar plot of binned x, default to be FALSE (display amount)
#' @param bPrintFig (optional) Boolean to decide whether the figure should be printed, default to be TRUE.
#'
#' @return a ggplot of averaged y versus binned x
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples ##----- without prediction
#' ggplots.SVA.bin(mtcars, 'vs', 'mpg') \cr
#' ##----- with prediction
#' ## assign mtcars to another df to avoid overwriting
#' tmp.df = mtcars
#' ## fit glm model
#' tmp.fit = glm(vs ~ 1+mpg + cyl + wt, tmp.df, family = binomial(link='logit'))
#' ## add prediction to data
#' tmp.df$pred = tmp.fit$fitted.values
#' ## plot
#' ggplots.SVA.bin(tmp.df, 'vs', 'mpg', 'pred')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ggplots.SVA.bin <- function(df, sDepVar, sIndVar, ## required
                         ## optional
                         sPredVar = NULL, sWtVar = NULL, iMaxBin = 10,
                         sTitle = NULL, sYLab = sDepVar, sY2Lab = sWtVar, sXLab = sIndVar,
                         bEqualDist = FALSE, bShowPct = FALSE, bPrintFig = TRUE) {


  ## keep only necessary variables
  tmp_data = df[, c(sIndVar, sDepVar, sWtVar, sPredVar)]

  ## rename for variable for remaining quote
  names(tmp_data)[1:2] = c('sIndVar', 'sDepVar')

  ## if weight variable is not given, assign 1 to each observation for convenience
  if (is.null(sWtVar)) tmp_data$sWtVar = 1 else data.table::setnames(x = tmp_data, old = sWtVar, new  = 'sWtVar')

  ## if predicted dependent is not given, using 1 as place holder
  if (is.null(sPredVar)) tmp_data$sPredVar = 1 else data.table::setnames(x = tmp_data, old = sPredVar, new  = 'sPredVar')

  ## bin the variable if it is numeric
  if (is.numeric(tmp_data$sIndVar)) {

    if (!bEqualDist) { ## if FALSE, bin the data based on quantile

      ## check the no. of distinct values
      element_level = length(unique(tmp_data$sIndVar))

      ## only need to bin the data if the no. of distinct values are greater than maximum number of bin
      if (element_level > iMaxBin) {
        ## calculate quantile
        tmp_quantile = unique(quantile(tmp_data$sIndVar, seq(0, 1, by = 1/iMaxBin), na.rm = TRUE))
        ## bin the data based on quantile
        tmp_data$sIndVar = cut( tmp_data$sIndVar, breaks = tmp_quantile, dig.lab = 4, include.lowest = TRUE)
      }

    } else tmp_data$sIndVar = cut( tmp_data$sIndVar, breaks = iMaxBin, dig.lab = 4, include.lowest = TRUE)

  }

  ## Add NA as a separate category
  tmp_data$sIndVar = addNA(as.factor(tmp_data$sIndVar))

  ## define %>% without loading dplyr
  `%>%` <- magrittr::`%>%`

  ## calculate the population distribution and average dependent variable in each bin
  tmp_avg_rate <- tmp_data %>%
    dplyr::group_by(sIndVar) %>%
    dplyr::summarise(avg_dep = weighted.mean(sDepVar, sWtVar, na.rm = TRUE),
                     avg_pred = weighted.mean(sPredVar, sWtVar, na.rm = TRUE),
                     freq = sum(sWtVar),
                     .groups = 'drop')

  ## calculate distribution if bShowPct = TRUE
  if(bShowPct) {tmp_avg_rate = tmp_avg_rate %>% dplyr::mutate(freq = freq/sum(freq))}


  ## create scale and int for secondary y axis
  ## mapping relationship: primary = int + scale * secondary
  ## especially, primary max = int + scale * secondary max; and primary min = int + scale * secondary min
  ## primary y axis: average dependent variable
  p.y.max = max(tmp_avg_rate$avg_dep, na.rm = TRUE)
  p.y.min = min(tmp_avg_rate$avg_dep, na.rm = TRUE)
  ## secondary y axis: population distribution
  s.y.max = max(tmp_avg_rate$freq, na.rm = TRUE)
  # s.y.min = min(tmp_avg_rate$freq, na.rm = TRUE)
  s.y.min = 0


  ## calculate scale and int based on the aforementioned mapping relationship
  scale = (p.y.max - p.y.min)/(s.y.max - s.y.min)
  int = p.y.max - scale * s.y.max

  ## define the position of labels of independent variables
  tmp_angle = 45
  tmp_hjust = 1

  ## format of tick label of y-axis, using the actual range as reference value
  ## find reference value
  tmp.rf.p = diff(range(tmp_avg_rate$avg_dep, na.rm = TRUE))
  tmp.rf.s = diff(range(tmp_avg_rate$freq, na.rm = TRUE))
  ## find proper format
  tmp.format.p.y = find.axis.format(ifelse(tmp.rf.p == 0, mean(tmp_avg_rate$avg_dep, na.rm = TRUE), tmp.rf.p))  ## primary y axis
  tmp.format.s.y = find.axis.format(ifelse(tmp.rf.s == 0, mean(tmp_avg_rate$freq, na.rm = TRUE), tmp.rf.s))  ## secondary y axis

  ## label of axis
  tmp.yaxis.p = ifelse(!is.null(sYLab) & sYLab != sDepVar, sYLab,                        ## primary y axis
                       ifelse(is.null(sWtVar), paste('Avg', sYLab),
                              paste0(sWtVar, 'Wt Avg', sYLab)))
  tmp.yaxis.s = ifelse(is.null(sWtVar), ifelse(is.null(sY2Lab), 'Population', sY2Lab),   ## secondary y axis
                       ifelse(is.null(sY2Lab), sWtVar, sY2Lab))
  if (bShowPct) tmp.yaxis.s = paste0(tmp.yaxis.s, ' (%)') ## if distribution is desired, add '(%)' to the end of label
  tmp.xaxis = ifelse(is.null(sXLab), sIndVar, sXLab)                                     ## x axis

  ## load FRB colors
  create.validation.cols()

  ## customize the color
  if (is.null(sPredVar)) { ## if there is not predicted dependent variable
    tmp.plot.color = unname(FRB_theme_color[c('Gold', 'Green')])  ## line (avg y) - green; bar(frequency) - gold.
    names(tmp.plot.color) = c(tmp.yaxis.s, sDepVar)
  } else {
    tmp.plot.color = unname(FRB_theme_color[c('Gold', 'Green', 'Clay_Grey')]) ## actual y - gold; pred y - green; frequency - grey
    names(tmp.plot.color) = c(sDepVar, sPredVar, tmp.yaxis.s)
  }

  ## create plot
  p <- ggplot(data = tmp_avg_rate) +
    geom_bar(mapping = aes(x = sIndVar, y = freq, fill = tmp.yaxis.s), stat = 'identity', alpha = 0.7) +
    geom_point(mapping = aes(x = as.numeric(sIndVar), y = (avg_dep - int)/scale),
               color = tmp.plot.color[sDepVar], fill = tmp.plot.color[sDepVar],
               size = 3, shape = 23) +
    geom_line(mapping = aes(x = as.numeric(sIndVar), y = (avg_dep- int)/scale, color = sDepVar), size = 1) +
    scale_y_continuous(name = tmp.yaxis.s,
                       position = "right",
                       labels = tmp.format.s.y,
                       sec.axis = sec_axis(trans = ~.*scale+int,
                                           name = tmp.yaxis.p,
                                           labels = tmp.format.p.y)) +
    theme(legend.title = element_blank(),
          legend.position = 'bottom',
          axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)), ## leave some space between y2 ticks and title
          axis.text.x = element_text(angle = tmp_angle, hjust = tmp_hjust)) + ## customize the position and the title of legend
    labs(title = if (is.null(sTitle)) element_blank() else sTitle, x = tmp.xaxis)

  ## if predicted depdent variable is given, add it to the graph
  if (!is.null(sPredVar)) {
    p = p +
      geom_point(mapping = aes(x = as.numeric(sIndVar), y = avg_pred),
                 color = tmp.plot.color[sPredVar], fill = tmp.plot.color[sPredVar],
                 size = 3, shape = 17) +
      geom_line(mapping = aes(x = as.numeric(sIndVar), y = avg_pred, color = sPredVar), size = 1)
  }

  ## change the color to FRB colors
  p = p +
    scale_fill_manual(values = tmp.plot.color) +
    scale_color_manual(values = tmp.plot.color)

  ## print the graph if bPrintFig = TRUE
  if (bPrintFig) print(p)

  ## output the results
  return(p)


}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title ggplots.SVA.hist.trend: Single Variable Analysis - Historical Trend of y and x
#'
#' @description This function plots average of dependent variable (y) and average independent variable (x) over time
#'
#' @param df (required) data frame.
#' @param sDepVar (required) name of dependent variable (y) in the df; must be a string.
#' @param sIndVar (required) name of independent variable (x) in the df; must be a string.
#' @param sDateVar (required) name of date variable (x) in the df; must be a string.
#' @param sWtVar (optional) name of weights in the df; must be a string.
#' @param sTitle (optional) Title of plot; must be a string.
#' @param sYLab (optional) a label for the primary y axis, default to be the same as `sDepVar`; must be a string.
#' @param sY2Lab (optional) a label for the secondary y axis, default to be the same as `sIndVar`; must be a string.
#' @param sXLab (optional) a label for the x axis, default to be the same as `sDateVar`; must be a string.
#' @param bPrintFig (optional) Boolean to decide whether the figure should be printed, default to be TRUE.
#'
#' @return a ggplot of averaged y and average x over time
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples ##----- create data frame
#' set.seed(1234)
#' x = rnorm(400, 0, 5)
#' y = 10*x**2 + 2 + rnorm(400, 0, 1)
#' tmp.df = data.frame(x, y)
#' tmp.date = seq(from = as.Date('2010-10-01'), to = as.Date('2015-12-01'), by = 'month')
#' tmp.df$AsOfDate = sample(tmp.date, size = 400, replace = TRUE)
#' ##----- plot
#' ggplots.SVA.hist.trend(tmp.df, 'y', 'x', 'AsOfDate')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ggplots.SVA.hist.trend <- function(df, sDepVar, sIndVar, sDateVar, ## required
                                   ## optional
                                   sWtVar = NULL,
                                   sTitle = NULL, sYLab = sDepVar, sY2Lab = sIndVar, sXLab = sDateVar, bPrintFig = TRUE) {

  ## keep only necessary variables
  tmp_data = df[, c(sDepVar, sIndVar, sDateVar, sWtVar)]

  ## rename for variable for remaining quote
  names(tmp_data)[1:3] = c('sDepVar', 'sIndVar', 'sDateVar')

  ## if weight variable is not given, assign 1 to each observation for convenience
  if (is.null(sWtVar)) tmp_data$sWtVar = 1 else data.table::setnames(x = tmp_data, old = sWtVar, new  = 'sWtVar')

  ## define %>% without loading dplyr
  `%>%` <- magrittr::`%>%`

  ## calculate average y and x over time
  tmp_plot <- tmp_data %>%
    dplyr::group_by(sDateVar) %>%
    dplyr::summarise(avg_dep = weighted.mean(sDepVar, sWtVar, na.rm = TRUE),
                     avg_ind = weighted.mean(sIndVar, sWtVar, na.rm = TRUE),
                     cnt_ind = sum(!is.na(sIndVar)),
                     pct_05 = quantile(sIndVar, prob = 0.05, na.rm = TRUE),
                     pct_25 = quantile(sIndVar, prob = 0.25, na.rm = TRUE),
                     pct_75 = quantile(sIndVar, prob = 0.75, na.rm = TRUE),
                     pct_95 = quantile(sIndVar, prob = 0.95, na.rm = TRUE),
                     .groups = 'drop') %>%
    dplyr::arrange(sDateVar)


  ## create scale and int for dual y axis
  ## mapping relationship: primary = int + scale * secondary
  ## especially, primary max = int + scale * secondary max; and primary min = int + scale * secondary min
  ## primary y axis: average dependent variable
  p.y.max = max(tmp_plot$avg_dep, na.rm = TRUE)
  p.y.min = min(tmp_plot$avg_dep, na.rm = TRUE)
  ## secondary y axis: average independent variable
  s.y.max = max(tmp_plot$avg_ind, na.rm = TRUE)
  s.y.min = min(tmp_plot$avg_ind, na.rm = TRUE)


  ## calculate scale and int based on the aforementioned mapping relationship
  scale = (p.y.max - p.y.min)/(s.y.max - s.y.min)
  int = p.y.max - scale * s.y.max

  ## format of tick label of y-axis, using the actual range as reference value
  ## find reference value
  tmp.rf.p = diff(range(tmp_plot$avg_dep, na.rm = TRUE))
  tmp.rf.s = diff(range(tmp_plot$avg_ind, na.rm = TRUE))
  ## find proper format
  tmp.format.p.y = find.axis.format(ifelse(tmp.rf.p == 0, mean(tmp_plot$avg_dep, na.rm = TRUE), tmp.rf.p))  ## primary y axis
  tmp.format.s.y = find.axis.format(ifelse(tmp.rf.s == 0, mean(tmp_plot$avg_ind, na.rm = TRUE), tmp.rf.s))  ## secondary y axis

  ## label of axis
  tmp.yaxis.p = ifelse(!is.null(sYLab) & sYLab != sDepVar, sYLab,                        ## primary y axis
                       ifelse(is.null(sWtVar), paste('Avg', sYLab),
                              paste0(sWtVar, 'Wt Avg', sYLab)))
  tmp.yaxis.s = ifelse(!is.null(sY2Lab) & sY2Lab != sIndVar, sY2Lab,                     ## secondary y axis
                       ifelse(is.null(sWtVar), paste('Avg', sY2Lab),
                              paste0(sWtVar, 'Wt Avg', sY2Lab)))
  tmp.xaxis = ifelse(!is.null(sXLab), sXLab, sDateVar)                                   ## x axis


  ## load FRB colors
  create.validation.cols()

  ## customize the color
  tmp.plot.color = unname(c(FRB_theme_color[c('Gold', 'Green')], FRB_Grays[2:3]))
  names(tmp.plot.color) = c(sDepVar, sIndVar, '[P_05, P_95]', '[P_25, P_75]')

  ## create plot
  p <- ggplot(data = tmp_plot, mapping = aes(x = sDateVar)) +
    geom_ribbon(mapping = aes(ymin = pct_05, ymax = pct_95, fill = '[P_05, P_95]')) +
    geom_ribbon(mapping = aes(ymin = pct_25, ymax = pct_75, fill = '[P_25, P_75]')) +
    geom_line(mapping = aes(y = avg_ind, color = sIndVar), size = 1) +
    geom_line(mapping = aes(y = (avg_dep - int)/scale, color = sDepVar), size = 1) +
    scale_y_continuous(name = tmp.yaxis.s,
                       position = "right",
                       labels = tmp.format.s.y,
                       sec.axis = sec_axis(trans = ~.*scale+int,
                                           name = tmp.yaxis.p,
                                           labels = tmp.format.p.y)) +
    scale_color_manual(name = 'Average', values = tmp.plot.color) +
    scale_fill_manual(name = paste('Range of', sIndVar), values = tmp.plot.color) +
    theme(legend.position="bottom",
          axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)), ## leave some space between y2 ticks and title
          ) +
    labs(title = if (is.null(sTitle)) element_blank() else sTitle, x = sXLab)


  ## print the graph if bPrintFig = TRUE
  if (bPrintFig) print(p)

  ## output the results
  return(p)

}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title ggplots.backtesting.trend: Backtesting Plot - Trend of actual and prediction
#'
#' @description This function plots (averaged) actual and prediction over time
#'
#' @param dfTrain data frame - training.
#' @param dfOOS data frame - Out-of-Sample (sharing the same window as training data).
#' @param dfOOT data frame - Out-of-Time (there should not be overlap between traiing and OOT window).
#' \cr
#' \cr
#' At least one of `dfTrain`, `dfOOS`, or `dfOOT` is required.
#' @param sActual (required) name of actuals in (all of) the df; must be a string.
#' @param sPred (required) name of predictions in (all of) the df; must be a string.
#' @param sDate (required) name of date variable in (all of) the df; must be a string.
#' @param sFacet (optional) name of segment variable in (all of) the df to be used in facet_wrap(); must be a string.
#' @param sWeight (optional) name of weight variable in (all of) the df; must be a string.
#' @param sTitle (optional) title of plot; must be a string.
#' @param sYLab (optional) a label for the primary y axis, default to be the same as `sActual`; must be a string.
#' @param sY2Lab (optional) a label for the secondary y axis, default to be `Population` if `sWeight` is NULL or `sWeight` if it is provided; must be a string.
#' @param sXLab (optional) a label for the x axis, default to be the same as `sDate`; must be a string.
#' @param bPrintFig (optional) Boolean to decide whether the figure should be printed, default to be TRUE.
#' @param ... (optional) arguments to be passed to facet_wrap()
#'
#' @return a ggplot of averaged actuals and predictions over time
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples ##----- create data frame
#' set.seed(1234)
#' x = rnorm(4000, 0, 10)
#' y = c(10*x[1:1500] + 2 + rnorm(1500, 0, 10), 9*x[1501:3500] + 3 + rnorm(2000, 0, 20), 12*x[3501:4000] -4 + rnorm(500, 0, 50))
#' tmp.df = data.frame(x, y)
#' tmp.date = seq(from = as.Date('2010-10-01'), to = as.Date('2018-12-01'), by = 'month')
#' tmp.df$AsOfDate = sample(tmp.date, size = 400, replace = TRUE)
#' tmp.train = tmp.df[tmp.df$AsOfDate <= as.Date('2016-12-01'), ]
#' tmp.OOT = tmp.df[tmp.df$AsOfDate > as.Date('2016-12-01'), ]
#'
#' ## fit lm
#' tmp.fit = lm(y~x, tmp.train)
#' tmp.train$pred = tmp.fit$fitted.values
#' tmp.OOT$pred = predict(tmp.fit, newdata = tmp.OOT)
#'
#' ## create plot
#' ggplots.backtesting.trend(dfTrain = tmp.train, dfOOT = tmp.OOT, sActual ='y', sPred = 'pred', sDate = 'AsOfDate')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ggplots.backtesting.trend <- function(dfTrain = NULL, dfOOS = NULL, dfOOT = NULL,
                                      sActual, sPred, sDate, sFacet = NULL, sWeight = NULL,
                                      sTitle = NULL, sYLab = sActual, sY2Lab = sWeight, sXLab = sDate,
                                      bPrintFig = TRUE, ...) {

  ## put all the data into one list
  tmp.list.df = list(Train = dfTrain, OOS = dfOOS, OOT = dfOOT)

  ## required variables
  tmp.req.vars = c(sActual, sPred, sDate, sFacet, sWeight)



  ## Step: Input checking
  #------------------------------------------------#

  ## check if at least one of dfTrain, dfOOS, or dfOOT is provided
  if(all(sapply(tmp.list.df, is.null))) { ## if all missing
    stop('Please provide at least one of the following: dfTrain, dfOOS, or dfOOT')
  }

  ## check, and generate error message if any variable is missing in any data set
  tmp.error.msg = lapply(
    1:length(tmp.list.df),  ## loop all data
    function(tmp.idx) {
      if(!is.null(tmp.list.df[[tmp.idx]])) { ## if the data is provided

        ## further check if any required variables is not in the data frame
        tmp.miss = !tmp.req.vars %in% names(tmp.list.df[[tmp.idx]])

        if (any(tmp.miss)) { ## if any one is missing, prepare error message
          return(paste0(names(tmp.list.df)[tmp.idx], ': ', paste(tmp.req.vars[tmp.miss], collapse = ', ')))
        }
      }
    })


  ## stop if any variable is missing in any dataset
  if (!all(sapply(tmp.error.msg, is.null))) {

    ## remove NULL
    tmp.error.msg[sapply(tmp.error.msg, is.null)] <- NULL

    ## stop and output error messages
    stop(paste0('The following columns are missing\n', paste(tmp.error.msg, collapse = '\n')))
  }

  #------------------------------------------------#
  ## End of Step;



  ## combine all data together
  tmp_df_1 = dplyr::bind_rows(tmp.list.df, .id = 'sample')

  ## keep only necessary variables
  tmp_df_2 = tmp_df_1[, c(tmp.req.vars, 'sample')]

  ## rename for variable for remaining quote
  names(tmp_df_2)[1:3] = c('sActual', 'sPred', 'sDate')

  ## if facet variable is not given, assign 'all' to each observation for convenience
  if (is.null(sFacet)) tmp_df_2$sFacet = 'all' else data.table::setnames(x = tmp_df_2, old = sFacet, new  = 'sFacet')

  ## if weight variable is not given, assign 1 to each observation for convenience
  if (is.null(sWeight)) tmp_df_2$sWeight = 1 else data.table::setnames(x = tmp_df_2, old = sWeight, new  = 'sWeight')

  ## format sample to factor
  tmp_df_2$sample = factor(tmp_df_2$sample, levels = c('Train', 'OOS', 'OOT'))

  ## define %>% without loading dplyr
  `%>%` <- magrittr::`%>%`

  ## calculate the average actual and prediction over time
  tmp_plot = tmp_df_2 %>%
    dplyr::group_by(sDate, sample, sFacet) %>%
    dplyr::summarise(actual = weighted.mean(x=sActual, w = sWeight, na.rm = TRUE),
                     pred = weighted.mean(x=sPred, w = sWeight, na.rm = TRUE),
                     freq = sum(sWeight, na.rm = TRUE),
                     .groups = 'drop')

  ## load FRB colors
  create.validation.cols()


  ## label of axis
  tmp.yaxis.p = ifelse(!is.null(sYLab) & sYLab != sActual, sYLab,                        ## primary y axis
                       ifelse(is.null(sWeight), paste('Avg', sYLab),
                              paste0(sWeight, 'Wt Avg', sYLab)))
  tmp.yaxis.s = ifelse(is.null(sWeight), ifelse(is.null(sY2Lab), 'Population', sY2Lab),   ## secondary y axis
                       ifelse(is.null(sY2Lab), sWeight, sY2Lab))
  tmp.xaxis = ifelse(!is.null(sXLab), sXLab, sDate)                                       ## x axis

  ## format of tick label of y-axis, using the actual range as reference value
  ## find reference value
  tmp.rf.p = diff(range(c(tmp_plot$actual, tmp_plot$pred), na.rm = TRUE))
  tmp.rf.s = diff(range(tmp_plot$freq, na.rm = TRUE))
  ## find proper format
  tmp.format.p.y = find.axis.format(ifelse(tmp.rf.p == 0, mean(c(tmp_plot$actual, tmp_plot$pred), na.rm = TRUE), tmp.rf.p))  ## primary y axis
  tmp.format.s.y = find.axis.format(ifelse(tmp.rf.s == 0, mean(tmp_plot$freq, na.rm = TRUE), tmp.rf.s))  ## secondary y axis


  ## create scale and int for secondary y axis
  ## mapping relationship: primary = int + scale * secondary
  ## especially, primary max = int + scale * secondary max; and primary min = int + scale * secondary min
  ## primary y axis: average actual and predicted dependent variable
  p.y.max = max(tmp_plot$actual, tmp_plot$pred, na.rm = TRUE)
  p.y.min = min(tmp_plot$actual, tmp_plot$pred, na.rm = TRUE)
  ## secondary y axis: population distribution
  s.y.max = max(tmp_plot$freq, na.rm = TRUE)
  s.y.min = 0


  ## calculate scale and int based on the aforementioned mapping relationship
  scale = (p.y.max - p.y.min)/(s.y.max - s.y.min)
  int = p.y.max - scale * s.y.max

  ## check whether area plot of frequency is needed (e.g. it is not needed for time-series model)
  tmp.area.plot = ifelse(s.y.max == 1 & dplyr::n_distinct(tmp_plot$freq) == 1, FALSE, TRUE)

  ## create plot
  if (tmp.area.plot) {
    p <- ggplot(data = tmp_plot, mapping = aes(x = sDate, linetype = sample, fill = sample)) +
      geom_line(mapping = aes(y = (actual - int)/scale, color = 'actual')) +
      # geom_point(mapping = aes(y = (actual - int)/scale, color = 'actual', shape = 'actual')) +
      geom_line(mapping = aes(y = (pred - int)/scale, color = 'pred')) +
      # geom_point(mapping = aes(y = (actual - int)/scale, color = 'pred', shape = 'pred')) +
      geom_area(mapping = aes(y = freq), stat = 'identity', alpha = 0.3) +
      scale_y_continuous(name = tmp.yaxis.s,
                         position = "right",
                         labels = tmp.format.s.y,
                         sec.axis = sec_axis(trans = ~.*scale+int,
                                             name = tmp.yaxis.p,
                                             labels = tmp.format.p.y)) +
      scale_color_manual(name = sActual, values=unname(FRB_theme_color[c('Gold', 'Green')])) +
      # scale_shape_manual(name = sActual, values=c(4,6)) +
      scale_fill_manual(values=unname(FRB_Grays[c(3, 5, 5)])) +
      scale_linetype_manual(values=c('solid', 'longdash', 'longdash')) +
      theme(legend.position="bottom",
            axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)), ## leave some space between y2 ticks and title
      ) +
      labs(title = if (is.null(sTitle)) element_blank() else sTitle, x = sXLab)


  } else {

    p <- ggplot(data = tmp_plot, mapping = aes(x = sDate, linetype = sample, fill = sample)) +
      geom_line(mapping = aes(y = actual, color = 'actual')) +
      # geom_point(mapping = aes(y = actual, color = 'actual', shape = 'actual')) +
      geom_line(mapping = aes(y = pred, color = 'pred')) +
      scale_linetype_manual(values=c('solid', 'longdash', 'longdash')) +
      scale_y_continuous(name = tmp.yaxis.p, labels = tmp.format.p.y) +
      scale_color_manual(name = sActual, values=unname(FRB_theme_color[c('Gold', 'Green')])) +
      # scale_shape_manual(name = sActual, values=c(4,6)) +
      theme(legend.position="bottom") +
      labs(title = if (is.null(sTitle)) element_blank() else sTitle, x = sXLab)
  }


  ## create a data to separate training window from OOT window
  if (!is.null(dfOOT) & (!is.null(dfTrain) | !is.null(dfOOS))) {
    tmp.oot.start.dt = min(tmp_plot[tmp_plot$sample == 'OOT', ]$sDate, na.rm = TRUE)
    tmp.pos.y = ifelse(tmp.area.plot, s.y.max, p.y.max)

    ## add vertical dashed lines to the plot to separate OOT and development window
    p = p +
      geom_vline(xintercept = tmp.oot.start.dt, linetype = 'dotted', color = FRB_theme_color['Charcoal']) +
      annotate('text', label = 'OOT', x = tmp.oot.start.dt, y = tmp.pos.y,
               color = FRB_theme_color['Charcoal'], vjust = 0.3, hjust = -0.3) +
      annotate('text', label = 'Train/OOS', x = tmp.oot.start.dt, y = tmp.pos.y,
               color = FRB_theme_color['Charcoal'], vjust = 0.3, hjust = 1.1)

  }

  ## face_wrap the plot
  if (!is.null(sFacet)) {
    p = p +
      facet_wrap(~sFacet, ...)
  }

  ## if only one data set is given, no need to show sample legend
  if(sum(!sapply(tmp.list.df, is.null)) == 1) {
    p = p + guides(linetype = FALSE, fill = FALSE)
  }

  ## print the graph if bPrintFig = TRUE
  if (bPrintFig) print(p)

  ## output the results
  return(p)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title ggplots.general.lines: Plots - Single or multiple lines on a plot
#'
#' @description This function draws one or multiple lines on a plot
#'
#' @param df (required) data frame.
#' @param sXVar (required) name of variable for x axis; must be a string.
#' @param sYVar (required) name of variable for y axis; must be a string.
#' @param sByVar (optional) name of variable to differentiate lines; must be a string.
#' @param sFacet (optional) name of segment variable in (all of) the df to be used in facet_wrap(); must be a string.
#' @param sTitle (optional) Title of plot; must be a string.
#' @param sYLab (optional) a label for the y axis, default to be the same as `sYVar`; must be a string.
#' @param sXLab (optional) a label for the x axis, default to be the same as `sXVar`; must be a string.
#' @param iLegendRow (optional) number of rows of legends, default to be 1; must be a integer
#' @param cLineColors (optional) colors of line, the order should be the alphabetical order of values of `sByVar`
#' @param bAddPoints (optional) Boolean to decide whether points should be added to lines, default to be FALSE.
#' @param bPrintFig (optional) Boolean to decide whether the figure should be printed, default to be TRUE.
#' @param ... (optional) arguments to be passed to facet_wrap()
#'
#' @return a ggplot of one or multiple line
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples ##----- create data frame
#' set.seed(1234)
#' tmp.df = data.frame(
#' x = seq(1, 100),
#' y1 = rnorm(100) + 5,
#' y2 = runif(100) * 5 + 4,
#' y3 = rgamma(100, 3)
#' )
#'
#' ## create plot - one line
#' ggplots.general.lines(df = tmp.df, sXVar = 'x', sYVar = 'y1')
#'
#' ## create plot - multiple lines
#' # transform data frame from wide to long
#' tmp.df.trans = reshape2::melt(tmp.df, id.vars = 'x', measure.vars = c('y1', 'y2', 'y3'))
#' ggplots.general.lines(df = tmp.df.trans, sXVar = 'x', sYVar = 'value', sByVar = 'variable',
#' sYLab = 'y', cLineColors=c('darkgreen', 'red', 'orange'), bAddPoints = TRUE)
#'
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ggplots.general.lines <- function(df, sXVar, sYVar,  ## required
                          sByVar = NULL, sFacet = NULL,
                          sTitle = NULL, sXLab = NULL, sYLab = NULL,
                          iLegendRow = 1, cLineColors = NULL, bAddPoints = FALSE,
                          bPrintFig = TRUE, ...) {

  ## keep only necessary variables
  tmp_data = df[, c(sXVar, sYVar, sByVar, sFacet)]

  ## rename for variable for remaining quote
  names(tmp_data)[1:2] = c('sXVar', 'sYVar')

  ## if group variable (sByVar) is not given, assign'all' to each observation for convenience
  if (is.null(sByVar)) tmp_data$sByVar = 'all' else data.table::setnames(x = tmp_data, old = sByVar, new  = 'sByVar')

  ## if facet variable is not given, assign 'all' to each observation for convenience
  if (is.null(sFacet)) tmp_data$sFacet = 'all' else data.table::setnames(x = tmp_data, old = sFacet, new  = 'sFacet')

  ## define %>% without loading dplyr
  `%>%` <- magrittr::`%>%`

  ## load FRB colors if cLineColors is not given
  if (is.null(cLineColors)) {
    create.validation.cols()
    tmp.linecolor = unname(FRB_theme_color[c('Green', 'Gold', 'Clay_Brown', 'Clay_Grey', 'Clay_Green', 'Clay_Blue')])
  } else {
    tmp.linecolor = cLineColors
  }

  ## expand the color if the distinct value of sByVar is greater than the color
  tmp.n.group = dplyr::n_distinct(tmp_data$sByVar) ## calculate no. of groups
  if (tmp.n.group > length(tmp.linecolor)) {
    tmp.linecolor = create.col.palette(tmp.linecolor, dimension = tmp.n.group)
  }

  ## label of axis
  tmp.yaxis = ifelse(!is.null(sYLab), sYLab, sYVar)
  tmp.xaxis = ifelse(!is.null(sXLab), sXLab, sXVar)

  ## format of tick label of y-axis, using the actual range as reference value
  ## find reference value
  tmp.rf.y = diff(range(tmp_data$sYVar, na.rm = TRUE))  ## y axis
  # tmp.rf.x = diff(range(tmp_data$sXVar, na.rm = TRUE))  ## x axis
  ## find proper format
  tmp.format.y = find.axis.format(ifelse(tmp.rf.y == 0, mean(tmp_data$sYVar, na.rm = TRUE), tmp.rf.y))  ## y axis
  # tmp.format.x = find.axis.format(ifelse(tmp.rf.x == 0, mean(tmp_data$sXVar, na.rm = TRUE), tmp.rf.x))  ## x axis


  ## create plot
  p <- ggplot(data = tmp_data, mapping = aes(x = sXVar, y = sYVar, color = as.factor(sByVar))) +
    geom_line() +
    scale_y_continuous(name = tmp.yaxis, labels = tmp.format.y) +
    # scale_x_continuous(name = tmp.xaxis, labels = tmp.format.x) +
    scale_color_manual(name = element_blank(), values=tmp.linecolor) +
    labs(title = if (is.null(sTitle)) element_blank() else sTitle, x = tmp.xaxis) +
    guides(color = guide_legend(nrow = iLegendRow))

  ## add points if bAddPoints is TRUE
  if (bAddPoints) {

    p <- p +
      geom_point(mapping = aes(shape = as.factor(sByVar), fill = as.factor(sByVar) )) +
      guides(shape = guide_legend(title = element_blank(), nrow = iLegendRow),
             fill = guide_legend(title = element_blank(), nrow = iLegendRow))
  }

  ## face_wrap the plot
  if (!is.null(sFacet)) {
    p = p +
      facet_wrap(~sFacet, ...)

    ## uncomment to debug
    # p = p + facet_wrap(~sFacet)
  }

  ## remove legend if sByVar is not given
  if (is.null(sByVar)) {
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = 'bottom')
  }


  ## print the graph if bPrintFig = TRUE
  if (bPrintFig) print(p)

  ## output the results
  return(p)

}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;
