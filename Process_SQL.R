######################################################################################################################################################
#----------------------------------------------------------------------------------------------------------------------------------------------------#
## Project: Validation Package for validation
## Script purpose: This script contains functions to process SQL codes
## Date: 2022-10-17
## Author: fchai
#----------------------------------------------------------------------------------------------------------------------------------------------------#
######################################################################################################################################################





#' @title read.query: read query from .sql
#'
#' @description This function reads queries from .sql into a string
#'
#'
#' @param sFilePath full path of .sql file; must be a string
#' @param sFileEncode (optional) file format for .sql, default to be `ANSI_X3.4-1986`; must be the same as .sql format.
#'
#' @return a string that contains all the query in the .sql file
#'
#' @export
#'
#' @examples ## read queries from a given sql file
#' read.query('/SQL/test.sql', sFileEncode = 'UTF-8')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
read.query <- function(sFilePath, sFileEncode = 'ANSI_X3.4-1986'){

  con = file(sFilePath, "rb", encoding=sFileEncode)
  sql.string <- ""
  
  while (TRUE){
    
    ## read each line
    line <- readLines(con, n = 1, encoding = sFileEncode, warn = FALSE)
    
    if ( length(line) == 0 ){
      break
    }
    
    ## replace tab with space
    line <- gsub("\\t", " ", line)
    
    ## in case \xa0 is not recognized as space, replace it with space
    line <- gsub("\xa0", " ", line)  
    line <- gsub("\\s", " ", line)  ## additional cautions
    
    ## if there is comments start with "--" in the code, replace it with /*  */
    tmp_line = gsub("\\'[^']*\\'", "", line)  ## ignore -- in the string quoted by ''
    
    if(grepl("--",tmp_line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }
    
    sql.string <- paste(sql.string, line)
  }
  
  close(con)
  return(sql.string)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title extract.sql.table.one: extract input and output tables from a .sql file
#'
#' @description This function extracts input and output tables from a .sql file
#'
#'
#' @param sFilePath full path of .sql file; must be a string
#' @param sFileEncode (optional) file format for .sql, default to be `ANSI_X3.4-1986`; must be the same as .sql format.
#' @param aKeywordInput (optional) array of keywords in query that are immediately followed by input tables.
#' @param aKeywordOutput (optional) array of keywords in query that are immediately followed by output tables.
#' @param aKeywordOthers (optional) other keywords in query that are not immediately followed by either input or output tables.
#'
#' @return a data frame contains all input and output tables used in the .sql file
#'
#' @export
#'
#' @examples ## read queries from a given sql file
#' extract.sql.table.one('/SQL/test.sql', sFileEncode = 'UTF-8')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
extract.sql.table.one <- function(sFilePath,
                                  sFileEncode ='ANSI_X3.4-1986',
                                  aKeywordInput = c("from", "join"),
                                  aKeywordOutput = c('into', 'create table', 'create view', 'with', 'update'),
                                  aKeywordOthers = c("use", "set", "select", "on", "where", "case when", "then", "else", "end", "begin",
                                                     "group by", "order by", "having by", "cross apply", "pivot", "unpivot")) {

  # require(dplyr)
  # require(tidyr)


  ## change all to capital
  aKeywordInput = toupper(aKeywordInput)
  aKeywordOutput = toupper(aKeywordOutput)
  aKeywordOthers = toupper(aKeywordOthers)
  Keywords_All = unique(c(aKeywordInput, aKeywordOutput, aKeywordOthers))
  
  ## Read the SQL query into string
  tmp_query = read.query(sFilePath, sFileEncode)
  
  ## first, remove all the string
  tmp_query_rms = gsub("\\'[^']*\\'", replacement = "", tmp_query)
  
  ## Remove comments
  tmp_rm_comment =toupper(gsub(pattern ="/\\*((?!/\\*).)*?\\*/", replacement = "", x = tmp_query_rms, perl = TRUE))  ## first, if there is not /* within /* */, remove them
  while (grepl("/\\*((?!/\\*).)*?\\*/", tmp_rm_comment, perl = TRUE)) {  ## in case whether there is comment within comment (e.g. "/* a /* */ */"), remove one layer at a time
    tmp_rm_comment = gsub(pattern ="/\\*((?!/\\*).)*?\\*/", replacement = "", x = tmp_rm_comment, perl = TRUE)
  }
  # tmp_rm_comment =toupper(gsub(pattern = "/\\*.*?\\*/", replacement = "",x = tmp_rm_comment))  ## finally, remove everything between /* */
  
  ## define %>% without loading dplyr
  `%>%` <- magrittr::`%>%`
  
  ## Split the query by keywords and remove the leading/trailing space
  tmp_split_sql = data.frame(
    stringsAsFactors = FALSE,
    splitted_string = stringr::str_split(string = tmp_rm_comment,
                                         pattern = paste0('(?i)\\b(', ## word separator
                                                          paste(paste0('(', Keywords_All, ')'), collapse = '|'), ## e.g. (USE)|(FROM)|...
                                                          ')\\b'))[[1]],
    keyword = c(NA, stringr::str_extract_all(string = tmp_rm_comment,
                                             pattern = paste0('(?i)\\b(', ## word separator
                                                              paste(paste0('(', Keywords_All, ')'), collapse = '|'), ## e.g. (USE)|(FROM)|...
                                                              ')\\b'))[[1]])
  ) %>%
    dplyr::mutate(splitted_string = trimws(toupper(splitted_string)),
                  keyword = trimws(toupper(keyword))) %>%
    dplyr::filter(!is.na(keyword))
  
  
  ## Find the pre-determined database if any
  tmp_used_db = tmp_split_sql %>%  ## find the string right after use
    dplyr::filter(keyword == 'USE') %>%
    dplyr::mutate(used_db = stringr::word(splitted_string, 1, sep = '\\s'))
  used_db = unique(tmp_used_db$used_db) ## double checking - there should only be one in one script
  used_db = stringr::str_replace_all(string=used_db, pattern='\\[|\\]', replacement = '')  ## remove []
  
  
  ## Find and classify the string that contains the name of input/output tables
  tmp_InputOutput_Split = tmp_split_sql %>%
    ## only keep keywords relevant to input/output table
    dplyr::filter(keyword %in% c(aKeywordInput, aKeywordOutput)) %>%
    ## mark input/output
    dplyr::mutate(category = ifelse(keyword %in% aKeywordInput, 'Input', 'Output')) %>%
    dplyr::filter(trimws(splitted_string) != '(') %>%
    dplyr::filter(trimws(substr(splitted_string, 1, 1)) != '@')
  
  
  ## sometimes, the tables were joined in the old way, e.g. "select from A, B"
  ## need to separate them,
  tmp_comma_split = tmp_InputOutput_Split %>%
    dplyr::mutate(tmp.split = stringr::str_extract(splitted_string, '^[^(]*\\(')) %>% ## ignore contect in ()
    dplyr::mutate(tmp.split = ifelse(is.na(tmp.split), splitted_string, tmp.split)) %>% ## ignore contect in ()
    dplyr::mutate(tmp.split = strsplit(x = tmp.split, split = ',')) %>% ## split the table by ","
    tidyr::unnest(tmp.split) %>%
    ## remove leading and trailing spaces
    dplyr::mutate(tmp.split = trimws(tmp.split)) %>%
    ## Extract the table names %>%
    dplyr::mutate(table = trimws(stringr::word(tmp.split, 1, sep = '[\\s);]'))) %>%
    ## remove []
    dplyr::mutate(table = stringr::str_replace_all(string=table, pattern='\\[|\\]', replacement = ''))
  
  ## find temporary table symbol
  ## those table either starts with # or created by with
  tmp_temporary_table = tmp_comma_split %>%
    dplyr::filter((keyword == 'WITH'| substr(table, 1, 1) == '#') & category == 'Output')
  
  ## extract temporary view in WITH CLAUSE
  Flag_WithClause = stringr::str_detect(tmp_rm_comment, '(?i)\\b(WITH.*AS)\\b') ## check whether there is with clause
  if (Flag_WithClause) {
    ## extract texts that has the following pattern: "), ("
    tmp_extract_with = stringr::str_extract_all(string = tmp_rm_comment, pattern = '(?i)\\)\\s*,[^\\(]*\\(')[[1]]
    ## double confirm the string ends with "AS ("
    tmp_extract_with = tmp_extract_with[stringr::str_detect(tmp_extract_with, "AS\\s*\\($")]
    ## if Yes, continue
    if (length(tmp_extract_with) != 0) {
      ## remove "AS ("from the extracted text
      tmp_with_table = trimws(stringr::str_remove(tmp_extract_with, pattern = "AS\\s*\\($"))
      ## extract table, which is after ,
      tmp_with_table = trimws(stringr::word(tmp_with_table, start = -1, sep = ','))
      
      ## add them to tmp_temporary_table
      tmp_temporary_table = dplyr::bind_rows(
        tmp_temporary_table,
        data.frame(stringsAsFactors = FALSE,
                   splitted_string = tmp_extract_with,
                   keyword = 'WITH',
                   category = 'Output',
                   table = trimws(tmp_with_table)) %>%
          ## remove []
          dplyr::mutate(table = stringr::str_replace_all(string=table, pattern='\\[|\\]', replacement = ''))
      )
    }
    
  }
  
  ## remove temporary table from the lists
  tmp_rm_tmp = tmp_comma_split %>%
    dplyr::filter(!table %in% tmp_temporary_table$table) %>%
    dplyr::select(category, table) %>%
    dplyr::mutate(tmp.count = stringr::str_count(table, '\\.')) %>%
    dplyr::mutate(table = ifelse(tmp.count == 1, paste(used_db, table, sep ='.'),
                                 ifelse(tmp.count == 0, paste(used_db, table, sep ='..'), table))) %>%
    dplyr::select(-tmp.count) %>%
    unique()

  ## output the results
  return(tmp_rm_tmp)
}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;





#' @title extract.sql.table.all: extract input and output tables from all .sql file in a folder
#'
#' @description This function extracts input and output tables from all .sql files in the folder
#'
#'
#' @param sFolderName (required) path of folder where .sql files are saved; must be a string.
#' @param bRecursive (optional) Boolean variable to determine whether the listing should be recurse into directories, default to be `FALSE`.
#' @param bSchemaInName (optional) Boolean variable to determine whether sql file name contains schema, default to be `FALSE`.
#' @param ... ... (optional) arguments to pass to `extract.sql.table.one`
#'
#' @return no value will be returned but a series of global variables will be generated to store paths
#'
#' @export
#'
#' @examples ## extract input and output tables from queries
#' extract.sql.table.all(sFolderName = '/SQL')
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
extract.sql.table.all <- function(sFolderName, bRecursive = FALSE, bSchemaInName = FALSE, ...) {

  ## get the list of sql queries in the folder
  list.sql.files <- list.files(sFolderName, pattern="*.sql", full.names=TRUE, recursive = bRecursive)

  ## extract input and output from the queries
  list_tables <- lapply(
    list.sql.files,
    function(sSQL) {

      ## debug only
      # sSQL = list.sql.files[1]

      ## get the input and output from the query
      # tmp_sql_data = extract.sql.table.one(sFilePath = sSQL) # debug only
      tmp_sql_data = extract.sql.table.one(sFilePath = sSQL, ...)

      ## add query file to the data
      tmp_sql_data$SQL_file = stringr::word(sSQL, start = -1, sep = '/')

      ## use schema from sql file as default schema if included in the file name
      if (bSchemaInName) {

        ## find schema from file name
        tmp_schema = toupper(stringr::word(stringr::word(sSQL, start = -1, sep = '/'), 1, sep = '\\.'))

        ## check if there is any missing schema
        tmp_sql_data = tmp_sql_data %>%
          dplyr::mutate(ori_table = table) %>%
          dplyr::mutate(table = stringr::str_replace(ori_table, stringr::fixed('..'), stringr::fixed(paste0('.', tmp_schema, '.'))))

      }

      ## output results
      return(tmp_sql_data)


    })

  ## combine the result into one data frame
  sql_data = dplyr::bind_rows(list_tables)

  ## note that SQL does not differentiate capital and small cases,
  ## change them to capital to avoid duplication
  sql_data$table_full = toupper(sql_data$table)

  ## distinguish between terminal/interim tables
  ## if an input table is an output of a query, then it is considered as an interim table
  ## otherwise, it is a terminal table
  List_Output = sql_data %>%
    dplyr::filter(category == 'Output') ## 1) find the output tables

  ## flag whether the input table is a terminal table.
  sql_data = sql_data %>%
    ## To be a terminal table,
    dplyr::mutate(Flag_Terminal = ifelse(category == 'Input' &  ## first, it is an input table
                                    !table_full %in% unique(List_Output$table_full),  ## second, it is not an output
                                  1, 0))
  ## output the results
  return(sql_data)

}
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
## End of Function;

