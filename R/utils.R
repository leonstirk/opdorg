#' Create dflist of the census csv frames
#' @param path string
#' @param files character
#' @param mbs character
#' @return list
#' @export
makeDflist2013 <- function(path, files, mbs) {
  ## List of dataframes. One dataframe for each census section (dwelling, household, family, individual_1, individual_2, etc.)
  dflist <- lapply(1:length(files), function (i) { read.csv(paste(path, files[i], sep = ''), header = F, encoding = 'UTF-8') })

  ## Shift column names from first row to column headers
  for(i in 1:length(files)) {
    names(dflist[[i]]) <- dflist[[i]][1,]
    dflist[[i]] <- dflist[[i]][-1,]
    rownames(dflist[[i]]) <- NULL
  }

  ## get MB rows only
  dflist <- lapply(dflist, function(x) {
    filter(x, Description == '' & unlist(lapply(strsplit(Area_Code_and_Description, ' '), function (x) { x[1] })) == "MB")
  })

  ## Get MB rows in the relevant spatial dataframe
  dflist <- lapply(dflist, function(x) { filter(x, Code %in% mbs) })

  ## Clean up dflist names
  for(i in 1:length(dflist)) {
    names(dflist[[i]]) <- names(dflist[[i]]) %>% cleanStringVec2013
  }

  ## drop the fist column "area code and description"
  dflist <- lapply(dflist, function(x) {
    x[2:ncol(x)]
  })

  return(dflist)
}

#' Create 'full' census tibble from dflist (from makeDflist)
#' @param dflist list
#' @return tibble
#' @export
makeCensusTibble <- function(dflist) {
  census_data <- rlist::list.cbind(lapply(dflist, function(df) {
    years <- df %>% names %>% stringr::str_extract('^[0-9]{4}+\\s') %>% as.factor %>% levels()

    ylist <- lapply(years, function(y) {
      tmp <- df[stringr::str_which(names(df), y)]
      names(tmp) <- tmp %>% names %>% stringr::str_replace_all('^[0-9]{4}+\\s', '')
      tmp <- cbind(df[1], tmp)
      tmp$year <- y
      tmp <- tmp %>% as_tibble
      return(tmp)
    })

    ref <- ylist %>% lapply(names) %>% lapply(length) %>% unlist
    ref <- which(ref == min(ref))[1]

    ylist <- ylist %>% lapply(., function(y) { y %>% select((ylist[[ref]] %>% names)) })

    return(rlist::list.rbind(ylist))
  }))

  census_data <- census_data %>% select(unique(names(census_data)))
  census_data[-c(1)] <- census_data[-c(1)] %>% lapply(as.numeric)

  ## All values < -900 to NA
  for(i in 1:length(census_data)) { census_data[which(census_data[i] < -900),i] <- NA }

  ## meshblock colname
  names(census_data)[1] <- 'meshblock'

  census_data <- census_data %>% as_tibble()
  return(census_data)
}

#' Clean name strings
#' @param stringVec character
#' @return character
cleanStringVec2013 <- function(stringVec){
  sapply(stringVec, function(string) {
    ## Lowercase
    temp <- tolower(string)
    ## digits between brackets (including brackets)
    temp <- stringr::str_replace_all(temp, "\\([\\d\\$]+\\)", "")
    ## get rid of this weird thing
    temp <- stringr::str_replace_all(temp, "\\x96", "to")
    ## Remove everything that is not a number or letter
    temp <- stringr::str_replace_all(temp,"[^0-9a-zA-Z\\s_]", "")
    ## Shrink down to just one white space
    temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
    ## Replace '_' with ' '
    temp <- stringr::str_replace_all(temp,"[_]", " ")
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
    return(temp)
  })
}

#' Create dflist of the census csv frames
#' @param path character
#' @param files character
#' @param lookups character
#' @param mbs character
#' @return list
#' @export
#'
makeDflist2018 <- function(path, files, lookups, sas) {
  ## List of dataframes. One dataframe for each census section (dwelling, household, family, individual_1, individual_2, etc.)
  dflist <- lapply(1:length(files), function (i) { read.csv(paste(path, files[i], sep = ''), header = F, encoding = 'UTF-8') })

  ## Shift column names from first row to column headers
  for(i in 1:length(files)) {
    names(dflist[[i]]) <- dflist[[i]][1,]
    dflist[[i]] <- dflist[[i]][-1,]
    rownames(dflist[[i]]) <- NULL
  }

  ## Get SA rows in the relevant spatial dataframe
  dflist <- lapply(dflist, function(x) { filter(x, SA12018_V1_00 %in% sas) })

  looklist <- lapply(1:length(lookups), function (i) { read.csv(paste(path, lookups[i], sep = ''), header = F, encoding = 'UTF-8') })

  for(i in 1:length(looklist)) {
    names(looklist[[i]]) <- looklist[[i]][1,]
    names(looklist[[i]]) <- c('full', 'match')
    looklist[[i]] <- looklist[[i]][-1,]
    rownames(looklist[[i]]) <- NULL
  }

  dnames <- dflist %>% lapply(., function(df) { data.frame(match = df %>% names %>% str_replace_all('C[0-9]{2}_', '') %>% unique()) })

  dnames <- lapply(1:length(dflist), function(i) {
    looklist[[i]]$match <- looklist[[i]]$match %>% str_replace_all('C[0-9]{2}_', '')
    looklist[[i]]$full <- looklist[[i]]$full %>% str_replace_all('Census_[0-9]{4}_', '')

    conv <- left_join(dnames[[i]], looklist[[i]], by = 'match')

    tmp <- data.frame(years = names(dflist[[i]]) %>% str_extract('C[0-9]{2}_') %>% str_replace_all('^C', 'Census_20'), match = names(dflist[[i]]) %>% str_replace_all('C[0-9]{2}_', ''))

    tmp$match <- tmp$match %>% str_replace_all('Brd_UK_Ireland', 'Brd_UK_and_Ireland')
    tmp$match <- tmp$match %>% str_replace_all('Brd_N_America', 'Brd_North_America')
    tmp$match <- tmp$match %>% str_replace_all('Brd_T_Stated', 'Brd_Total_stated')
    tmp$match <- tmp$match %>% str_replace_all('Arrival_Total_OB', 'ArrivalNZ_Total_OB')

    tmp <- left_join(tmp, conv, by = 'match')
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
    names(dflist[[i]]) <- names(dflist[[i]]) %>% cleanStringVec2018
  }

  dflist <- lapply(dflist, function(df) {
    df[which(!is.na(names(df)))]
  })

  return(dflist)
}

#' Create 'full' census tibble from dflist (from makeDflist)
#' @param dflist list
#' @return tibble
#' @export
makeCensusTibble2018 <- function(dflist) {
  census_data <- rlist::list.cbind(lapply(dflist, function(df) {
    years <- df %>% names %>% stringr::str_extract('^[0-9]{4}+\\s') %>% as.factor %>% levels()

    ylist <- lapply(years, function(y) {
      tmp <- df[stringr::str_which(names(df), y)]
      names(tmp) <- tmp %>% names %>% stringr::str_replace_all('^[0-9]{4}+\\s', '')
      tmp <- cbind(df[1], tmp)
      tmp$year <- y
      tmp <- tmp %>% as_tibble
      return(tmp)
    })

    ref <- ylist %>% lapply(names) %>% lapply(length) %>% unlist
    ref <- which(ref == min(ref))[1]

    ylist <- ylist %>% lapply(., function(y) { y %>% select((ylist[[ref]] %>% names)) })

    return(rlist::list.rbind(ylist))
  }))

  census_data <- census_data %>% select(unique(names(census_data)))
  census_data[-c(1)] <- census_data[-c(1)] %>% lapply(as.numeric)

  ## All values < -900 to NA
  for(i in 1:length(census_data)) { census_data[which(census_data[i] < -900),i] <- NA }

  ## Statistical area colname
  names(census_data)[1] <- 'sa1'

  census_data <- census_data %>% as_tibble()
  return(census_data)
}

#' Clean name strings
#' @param stringVec character
#' @return character
cleanStringVec2018 <- function(stringVec){
  sapply(stringVec, function(string) {
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

    return(temp)
  })
}

