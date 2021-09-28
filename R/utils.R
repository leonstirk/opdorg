#' Create tibble from the census csv frames
#' @param dflist list
#' @param agg_lv character
#' @param sp_unit_ids character
#' @return tibble
#' @export
make2013CensusTibble <- function(dflist, agg_lv = 'mb', sp_unit_ids = NA) {

  ## Shift column names from first row to column headers
  for(i in 1:length(dflist)) {
    names(dflist[[i]]) <- dflist[[i]][1,]
    dflist[[i]] <- dflist[[i]][-1,]
    rownames(dflist[[i]]) <- NULL
  }

  ## get only rows that correspond to the specified aggregation level
  if(agg_lv %in% c('mb', 'MB', 'Mb')) {
    dflist <- lapply(dflist, function(x) {
      dplyr::filter(x, Description == '' & unlist(lapply(strsplit(Area_Code_and_Description, ' '), function (x) { x[1] })) == "MB")
    })
  } else if (agg_lv %in% c('AU', 'au', 'Au')) {

  } else if (agg_lv %in% c('Ward', 'ward')) {

  } else if (agg_lv %in% c('TA', 'ta', 'Ta')) {

  } else if (agg_lv %in% c('Region', 'region')) {

  }

  ## Get specified spatial unit rows only
  if(length(sp_unit_ids) > 1 & !anyNA(sp_unit_ids)){
    dflist <- lapply(dflist, function(x) { dplyr::filter(x, Code %in% sp_unit_ids) })
  }

  ## Clean up dflist names
  for(i in 1:length(dflist)) {
    names(dflist[[i]]) <- names(dflist[[i]]) %>% clean2013colnames
  }

  ## drop the fist column "area code and description"
  dflist <- lapply(dflist, function(x) {
    x[2:ncol(x)]
  })

  census_data <- rlist::list.cbind(lapply(dflist, function(df) {
    years <- df %>% names %>% stringr::str_extract('^[0-9]{4}+_') %>% stringr::str_replace_all('_$', '') %>% as.factor %>% levels()

    ylist <- lapply(years, function(y) {
      tmp <- df[stringr::str_which(names(df), y)]
      names(tmp) <- tmp %>% names %>% stringr::str_replace_all('^[0-9]{4}+_', '')
      tmp <- cbind(df[1], tmp)
      tmp$year <- y
      tmp <- tmp %>% dplyr::as_tibble()
      return(tmp)
    })

    ref <- ylist %>% lapply(names) %>% lapply(length) %>% unlist
    ref <- which(ref == min(ref))[1]

    ylist <- ylist %>% lapply(., function(y) { y %>% dplyr::select((ylist[[ref]] %>% names)) })

    return(rlist::list.rbind(ylist))
  }))

  census_data <- census_data %>% dplyr::select(unique(names(census_data)))
  census_data[-c(1)] <- census_data[-c(1)] %>% lapply(as.numeric)

  ## All values < -900 to NA
  for(i in 1:length(census_data)) { census_data[which(census_data[i] < -900),i] <- NA }

  ## ## meshblock colname
  ## names(census_data)[1] <- 'meshblock'

  census_data <- census_data %>% dplyr::as_tibble()
  census_data$year <- census_data$year %>% as.factor()

  return(census_data)
}

#' Create tibble from the census csv frames
#' @param dflist list
#' @param looklist list
#' @param sp_unit_ids character
#' @param keep_2018 logical
#' @return tibble
#' @export
make2018CensusTibble <- function(dflist, looklist, sp_unit_ids = NA, keep_2018 = F) {

  ## Shift column names from first row to column headers
  for(i in 1:length(dflist)) {
    names(dflist[[i]]) <- dflist[[i]][1,]
    dflist[[i]] <- dflist[[i]][-1,]
    rownames(dflist[[i]]) <- NULL
  }

  ## Get specified spatial unit rows only
  if(!is.na(sp_unit_ids)){
    dflist <- lapply(dflist, function(x) { dplyr::filter(x, SA12018_V1_00 %in% sp_unit_ids) })
  }

  for(i in 1:length(looklist)) {
    names(looklist[[i]]) <- looklist[[i]][1,]
    names(looklist[[i]]) <- c('full', 'match')
    looklist[[i]] <- looklist[[i]][-1,]
    rownames(looklist[[i]]) <- NULL
  }

  dnames <- dflist %>% lapply(., function(df) { data.frame(match = df %>% names %>% stringr::str_replace_all('C[0-9]{2}_', '') %>% unique()) })

  dnames <- lapply(1:length(dflist), function(i) {
    looklist[[i]]$match <- looklist[[i]]$match %>% stringr::str_replace_all('C[0-9]{2}_', '')
    looklist[[i]]$full <- looklist[[i]]$full %>% stringr::str_replace_all('Census_[0-9]{4}_', '')

    conv <- dplyr::left_join(dnames[[i]], looklist[[i]], by = 'match')

    tmp <- data.frame(years = names(dflist[[i]]) %>% stringr::str_extract('C[0-9]{2}_') %>% stringr::str_replace_all('^C', 'Census_20'), match = names(dflist[[i]]) %>% str_replace_all('C[0-9]{2}_', ''))

    tmp$match <- tmp$match %>% stringr::str_replace_all('Brd_UK_Ireland', 'Brd_UK_and_Ireland')
    tmp$match <- tmp$match %>% stringr::str_replace_all('Brd_N_America', 'Brd_North_America')
    tmp$match <- tmp$match %>% stringr::str_replace_all('Brd_T_Stated', 'Brd_Total_stated')
    tmp$match <- tmp$match %>% stringr::str_replace_all('Arrival_Total_OB', 'ArrivalNZ_Total_OB')

    tmp <- dplyr::left_join(tmp, conv, by = 'match')
    tmp$years_full <- tmp$full
    tmp$years_full[!is.na(tmp$full)] <- paste(tmp$years[which(!is.na(tmp$full))], tmp$full[which(!is.na(tmp$full))], sep = '')
    tmp$years_full[which(is.na(tmp$years))] <- tmp$full[which(is.na(tmp$years))]

    return(tmp$years_full)
  })

  dflist <- lapply(1:length(dflist), function(i) {
    names(dflist[[i]]) <- dnames[[i]]
    return(dflist[[i]])
  })

  ## Clean up dflist names
  for(i in 1:length(dflist)) {
    names(dflist[[i]]) <- names(dflist[[i]]) %>% clean2018colnames
  }

  dflist <- lapply(dflist, function(df) {
    df[which(!is.na(names(df)))]
  })

  census_data <- rlist::list.cbind(lapply(dflist, function(df) {
    years <- df %>% names %>% stringr::str_extract('^[0-9]{4}+_') %>% stringr::str_replace_all('_$', '') %>% as.factor %>% levels()

    ylist <- lapply(years, function(y) {
      tmp <- df[stringr::str_which(names(df), y)]
      names(tmp) <- tmp %>% names %>% stringr::str_replace_all('^[0-9]{4}+_', '')
      tmp <- cbind(df[1], tmp)
      tmp$year <- y
      tmp <- tmp %>% dplyr::as_tibble()
      return(tmp)
    })

    if(!keep_2018) {
      ## Trim out the variables only collected in 2018
      ref <- ylist %>% lapply(names) %>% lapply(length) %>% unlist
      ref <- which(ref == min(ref))[1]
      ylist <- ylist %>% lapply(., function(y) { y %>% dplyr::select((ylist[[ref]] %>% names)) })
    }

    return(dplyr::bind_rows(ylist))
  }))

  census_data <- census_data %>% dplyr::select(unique(names(census_data)))
  census_data[-c(1)] <- census_data[-c(1)] %>% lapply(as.numeric)

  ## All values < -900 to NA
  for(i in 1:length(census_data)) { census_data[which(census_data[i] < -900),i] <- NA }

  ## Statistical area colname
  names(census_data)[1] <- 'sa1'
  census_data <- census_data %>% dplyr::as_tibble()
  census_data <- census_data %>% dplyr::select(sa1, year, names(census_data)[!names(census_data) %in% c('sa1', 'year')])
  census_data$year <- census_data$year %>% as.factor()

  return(census_data)
}

####################################################################################################

#' Clean name strings
#' @param colnames character
#' @return character
clean2013colnames <- function(colnames){
  sapply(colnames, function(string) {
    ## Lowercase
    temp <- tolower(string)
    ## digits between brackets (including brackets)
    temp <- stringr::str_replace_all(temp, "\\([\\d\\$]+\\)", "")
    ## get rid of this weird thing
    temp <- stringr::str_replace_all(temp, "\\x96", "to")
    ## replace / with _
    temp <- stringr::str_replace_all(temp, '/', '_')

    ## Remove everything that is not a number or letter
    temp <- stringr::str_replace_all(temp, "[^0-9a-zA-Z\\s_]", "")

    ## Replace '_' with ' '
    temp <- stringr::str_replace_all(temp, "[_]", " ")

    ## Shrink down to just one white space
    temp <- stringr::str_replace_all(temp, "[\\s]+", " ")

    ## Replace 'occupied private dwelling' with 'OPD'
    temp <- stringr::str_replace_all(temp, "occupied private dwelling", "OPD")
    ## Replace 'private occupied dwelling' with 'OPD'
    temp <- stringr::str_replace_all(temp, "private occupied dwelling", "OPD")
    ## Replace 'census usually resident population' with 'CURP'
    temp <- stringr::str_replace_all(temp, "census usually resident population", "CURP")
    ## Replace 'aged 15 years and over' with 'aged 15+'
    temp <- stringr::str_replace_all(temp, "aged 15 years and over", "aged 15+")
    ## Remove the word 'census'
    temp <- stringr::str_replace_all(temp, '(^\\d{4})\\scensus', '\\1')
    ## Get rid of trailing "" if necessary
    indexes <- which(temp == "")
    if(length(indexes) > 0){
      temp <- temp[-indexes]
    }
    temp <- tolower(temp)
    temp <- snakecase::to_any_case(temp, 'snake')
    return(temp)
  })
}

#' Clean name strings
#' @param colnames character
#' @return character
clean2018colnames <- function(colnames){
  sapply(colnames, function(string) {
    ## Lowercase
    temp <- tolower(string)
    ## Replace '_' with ' '
    temp <- stringr::str_replace_all(temp,"[_]", " ")
    ## Remove everything that is not a number or letter
    temp <- stringr::str_replace_all(temp,"[^0-9a-zA-Z\\s_]", "")
    ## Replace 'census usually resident population' with 'CURP'
    temp <- stringr::str_replace_all(temp, "curp", "CURP")
    ## Remove the word 'census'
    temp <- stringr::str_replace_all(temp, '^census\\s(\\d{4})', '\\1')
    ## Sort this shit out
    temp <- stringr::str_replace_all(temp, '15years', '15 years')
    ## Remove random numbers
    temp <- stringr::str_replace_all(temp, '(?<!to\\s|\\d|\\sa|under\\s|^)\\d+(?!\\sto\\s|\\d|\\sand\\sover|\\syear|\\sor\\s|\\scertif|\\sdiplo|\\squalif)', '')
    ## Shrink down to just one white space
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    ## Replace 'occupied private dwelling' with 'OPD'
    temp <- stringr::str_replace_all(temp, "occupied private dwelling", "OPD")
    ## Replace 'private occupied dwelling' with 'OPD'
    temp <- stringr::str_replace_all(temp, "private occupied dwelling", "OPD")
    ## Correct for removal of macronated a
    temp <- stringr::str_replace_all(temp, ' mori ', ' maori ')
    ## 'nec' to 'not elsewhere classified'
    temp <- stringr::str_replace_all(temp, ' nec ', ' not elsewhere classified ')
    ## no trailing spaces
    temp <- stringr::str_replace_all(temp, '\\s$', '')

    ## digits between brackets (including brackets)
    ##temp <- stringr::str_replace_all(temp, "\\([\\d\\$]+\\)", "")
    ## get rid of this weird thing
    ##temp <- stringr::str_replace_all(temp, "\\x96", "to")

    ## Replace 'aged 15 years and over' with 'aged 15+'
    ##temp <- stringr::str_replace_all(temp, "aged 15 years and over", "aged 15+")

    ## Get rid of trailing "" if necessary
    ##indexes <- which(temp == "")
    ##if(length(indexes) > 0){
    ##  temp <- temp[-indexes]
    ##}

    temp <- tolower(temp)
    temp <- snakecase::to_any_case(temp, 'snake')
    return(temp)
  })
}

