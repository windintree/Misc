######################################################################################################################################################
#----------------------------------------------------------------------------------------------------------------------------------------------------#
## Project: Validation Package for validation
## Script purpose: This script contains functions to create FRB colors
## Date: 2022-10-17
## Author: fchai
#----------------------------------------------------------------------------------------------------------------------------------------------------#
######################################################################################################################################################





#' @title isnull: fill missing values
#'
#' @description This function returns the specified value if `var` is NA, otherwise returns `def.var`.
#'
#' @param var (required) value/arrays to test whether is NA
#' @param def.var (required) a value to return if `var` is NA; must be the same type (e.g. string, date, numeric) as var
#'
#' @return a vector of the same type and size as `var` with NA filled with `def.var`
#'
#' @export
#'
#' @examples isnull(c(1, 2, NA), 99)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
isnull <- function(var, def.var) {

  return(ifelse(is.na(var), def.var, var))
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @name Round2
#' @aliases round2
#' @title round2: round the number
#'
#' @description `round2` rounds number to the specified number of decimal places
#' Note that `round` function in R rounds 0.5 to the nearest even number (i.e. 0), rather than nearest number (i.e. 1)
#'
#' @param x (required) a numeric vector
#' @param n (optional) an integer indicating the number of digits after decimal places, default to be 0
#'
#' @return a rounded numeric vector
#'
#' @export
#'
#' @examples round2(c(1.35, 2, 0.5), 1)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
round2 <- function(x, n = 0) {
  z = trunc(abs(x)*10^n + 0.5)
  z = z/10^n
  return(z*sign(x))
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;




#' @name Round2
#' @aliases floor2
#' @title Round2: round the number
#'
#' @description `floor2` returns a numeric vector containing the largest integers not greater than the corresponding elements of x.
#'
#' @export
#'
#' @examples floor2(c(1.35, 2, 0.5), 1)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
floor2 <- function(x, n = 0) {
  # Description of input:
  # x: numeric, number to be rounded
  # n: integer, number of digits to be kept after decimal place, default to be 0

  z = round(x - 5*10^(-n-1), n)
  return(z)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title AddSheet: add excel spread sheet
#'
#' @description This function adds spread sheet to existing workbook.
#'
#' @param x (required) Object to be written.
#' @param sFile (required) full name (including path) of a Workbook object containing a worksheet; must be a string.
#' @param sSheetName (required) The worksheet to write to; must be a string.
#' @param bOverwirte (optional) Boolean variable to determine whether existing worksheet should be overwritten, default to be `TRUE`.
#'
#' @return no value will be returned
#'
#' @export
#'
#' @examples AddSheet(x = c(1,2,3), '/mnt/qnt_mmrstudio/sandbox/test.xlsx', 'test')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
AddSheet <- function(x, sFile, sSheetName, bOverwirte = FALSE) {

  ## Load the workbook
  tmp_excel = openxlsx::loadWorkbook(sFile)

  ## Check if the there is a sheet in the workbook that has the same name
  if(sSheetName %in% names(tmp_excel)) {

    if (bOverwirte) { ## if overwrite of existing worksheet is true, remove the worksheet before adding it
      openxlsx::removeWorksheet(tmp_excel, sSheetName)
    } else {
      stop("A worksheet by that name already exists.")
    }
  }

  ## Add another sheet
  openxlsx::addWorksheet(tmp_excel, sSheetName)

  ## Write the data to the sheet
  openxlsx::writeData(tmp_excel, sheet = sSheetName, x = x)

  ## Save the file
  openxlsx::saveWorkbook(tmp_excel, sFile, overwrite = TRUE)

}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @name ListEmptyToObj
#' @aliases ListEmptyToObj
#' @title ListEmptyToObj: replace empty element in a list with an object
#'
#' @description `ListEmptyToObj` replaces empty list to an object
#'
#' @param x.list (required) a list
#' @param replace.val (optional) a value to replace the empty element, default to be `NULL` for `ListEmptyToObj` and `NA` for `ListNULLToObj`
#'
#' @return a list
#'
#' @export
#'
#' @examples ## create list
#' tmp.list = list(a = c(1, 2, 3), b = list(b1 = NA, b2 = list()), c = NULL)
#' tmp.list # show the list
#'
#' ## replace empty element with NULL
#' ListEmptyToObj(tmp.list)
#' ListEmptyToObj(tmp.list, NA) ## replace empty element to NA
#' ListNULLToObj(tmp.list)  ## replace NULL to NA
#'
#' ## replace empty element with an array
#' ListEmptyToObj(tmp.list, replace.val = c('a', 'b', 'c'))
#' ListNULLToObj(tmp.list, replace.val = c('a', 'b', 'c'))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ListEmptyToObj <- function(x.list, replace.val = NULL){

  lapply(x.list,
         function(x) {
           if(is.list(x) && length(x)==0) replace.val else if (is.list(x)) ListEmptyToObj(x, replace.val) else x})
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @name ListEmptyToObj
#' @aliases ListNULLToObj
#' @title ListEmptyToObj: replace empty element in a nested list with an object
#'
#' @description `ListNULLToObj` replaces NULL in nested list to an object
#'
#' @export
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
ListNULLToObj <- function(x.list, replace.val = NA){

  lapply(x.list,
         function(x) {
           if(!is.list(x) & is.null(x)) replace.val else if (is.list(x)) ListNULLToObj(x) else x})
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title get.repo.info: extract basic information of repo
#'
#' @description This function extract basic information of repo and store them in a data frame
#'
#'
#' @export
#'
#' @examples extract.repo.info()
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
extract.repo.info <- function() {

  ## extract basic information of repo
  tmp_repo = system('git remote show origin', intern = TRUE)

  ## compile all information
  tmp_info = data.frame(stringsAsFactors = FALSE,
                        SSH_Path_Fetch = tmp_repo[stringr::str_detect(tmp_repo, 'Fetch URL')],
                        SSH_Path_Push = tmp_repo[stringr::str_detect(tmp_repo, 'Push  URL')],
                        Branch_HEAD = tmp_repo[stringr::str_detect(tmp_repo, 'HEAD branch')],
                        Branch_Current = system("git branch | grep '*'", intern = TRUE))

  ## define %>% without loading dplyr
  `%>%` <- magrittr::`%>%`

  ## clean the data
  git_info = tmp_info %>%
    dplyr::mutate(
      SSH_Path_Fetch = trimws(stringr::str_remove(SSH_Path_Fetch, 'Fetch URL\\:')),
      SSH_Path_Push = trimws(stringr::str_remove(SSH_Path_Push, 'Push  URL\\:')),
      Branch_HEAD = trimws(stringr::str_remove(Branch_HEAD, 'HEAD branch\\:')),
      Branch_Current = trimws(stringr::str_remove(Branch_Current, '^\\*'))
    ) %>%
    dplyr::transmute(
      SSH_Path_Fetch,
      SSH_Path_Push,
      Repo_Fetch = stringr::word(SSH_Path_Fetch, -1, sep = ':'),
      Repo_Push = stringr::word(SSH_Path_Push, -1, sep = ':'),
      Branch_HEAD,
      Branch_Current
    )

  ## output the results
  return(git_info)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;




#' @title data.summary: summarize data
#'
#' @description This function summarizes the data to provide an overview.
#'
#' @param df (required) a data frame
#' @param sVarList (optional) string vector of variable names to be summarized, default to be all columns in the data
#'
#' @return a data frame includes key information of all tested variables, such as missing rate, mean, etc.
#'
#' @export
#'
#' @examples ## create data frame
#' df = data.frame(x = rnorm(10), y = factor(1:10), z = letters[11:20], stringsAsFactors = FALSE)
#' data.summary(df)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
data.summary <- function(df, sVarList = names(df)) {

  ## keep the variables in interest
  tmp_dsa_1 = df[, sVarList]

  ## Create a data frame to store assessment results
  DQ_Results = data.frame( var_name = names(tmp_dsa_1),
                           stringsAsFactors = FALSE)

  ## Display the format of each variable
  DQ_Results$Format = sapply(tmp_dsa_1, class)

  ## Summarize Data
  tmp_dsa_description = Hmisc::describe(tmp_dsa_1)

  ## Tidy output and store in a data frame
  for (i in tmp_dsa_description) {

    ## Reformat the values to ensure consistency
    tmp_dsa_name = as.data.frame(as.character(i$descript), stringsAsFactors = FALSE)
    tmp_dsa_stat = as.data.frame(t(sapply(i$counts, as.numeric)))
    tmp_dsa_extremes = as.data.frame(t(sapply(i$extremes, as.character)), stringsAsFactors = FALSE)

    ## Rename the variables
    names(tmp_dsa_stat) = gsub("\\.", "P_", names(tmp_dsa_stat))  ## quantiles
    names(tmp_dsa_name) = 'var_name'                              ## Variable names
    names(tmp_dsa_stat)[1] = 'non_missing'                        ## No. of non-missing observations

    ## Combine different statistics
    tmp_dsa_summary = cbind(tmp_dsa_name, tmp_dsa_stat, tmp_dsa_extremes)

    ## Combine results of different variables
    if (i$descript == names(tmp_dsa_1)[1]) {
      tmp_dsa_summary_all = tmp_dsa_summary
    } else {
      tmp_dsa_summary_all = dplyr::bind_rows(tmp_dsa_summary_all, tmp_dsa_summary)
    }
  }

  ## Calculate missing rate
  tmp_dsa_summary_all$total_n = tmp_dsa_summary_all$missing + tmp_dsa_summary_all$non_missing
  tmp_dsa_summary_all$missing_rate = paste0(round(tmp_dsa_summary_all$missing/(tmp_dsa_summary_all$total_n)*100, 2), '%', sep='')
  tmp_dsa_summary_all$uniqueness = (tmp_dsa_summary_all$total_n == tmp_dsa_summary_all$distinct)

  ## Merge the summary into results
  DQ_Results = merge(DQ_Results, tmp_dsa_summary_all, by='var_name', all.x = TRUE)

  ## Calculate standard deviation
  tmp_dsa_2 = Filter(is.numeric, tmp_dsa_1)   ## Keep only numeric columns
  if (dim(tmp_dsa_2)[2] > 0) {
    tmp_dsa_std = data.frame( var_name = names(tmp_dsa_2), stringsAsFactors = FALSE)
    tmp_dsa_std$std = sapply(tmp_dsa_2, sd, na.rm = TRUE)

    ## Merge the std into results
    DQ_Results = merge(DQ_Results, tmp_dsa_std, by='var_name', all.x = TRUE)
  } else DQ_Results$std <- NA

  ## define %>% without loading dplyr
  `%>%` <- magrittr::`%>%`

  ## reorder columns
  DQ_Results <- DQ_Results %>%
    dplyr::select(var_name, Format, total_n, non_missing, missing, missing_rate,
                  distinct, uniqueness, Mean, std,
                  dplyr::everything())

  ## output the results
  return(DQ_Results)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;
