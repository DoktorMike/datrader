#' Download a list of instrumentr from Yahoo
#'
#' Download a list of instruments from Yahoo into the specified path which is kept as CSV files
#'
#' Nothing
#' @param instruments the character vector of instruments to download
#' @param path the path where to store the final CSV files
#' @param startDate the starting date to start the query from
#' @importFrom quantmod getSymbols
#' @importFrom zoo write.zoo
#' @importFrom dplyr progress_estimated
#' @return TRUE if we successfully downloaded all instruments. FALSE otherwise
#' @export
#'
#' @examples
#' omxs30Symbols<-c('ABB.ST', 'ALFA.ST', 'ASSA-B.ST', 'AZN.ST', 'ATCO-A.ST',
#' 'ATCO-B.ST', 'BOL.ST', 'ELUX-B.ST', 'ERIC-B.ST', 'GETI-B.ST', 'HMB.ST',
#' 'INVE-B.ST', 'LUPE.ST', 'MTG-B.ST', 'NDA-SEK.ST', 'SAND.ST',
#' 'SCA-B.ST', 'SCV-B.ST', 'SEB-A.ST', 'SECU-B.ST', 'SKA-B.ST', 'SKF-B.ST',
#' 'SSAB-A.ST', 'SHB-A.ST', 'SWED-A.ST', 'SWMA.ST', 'TEL2-B.ST', 'TLSN.ST', 'VOLV-B.ST')
#' \dontrun{downloadInstruments(c('ABB.ST'), '/somewhere/trading')}
downloadInstruments<-function(instruments, path, startDate="2000-01-01"){
  if(!dir.exists(paths = path)) dir.create(path = path, recursive = T)
  p <- progress_estimated(length(instruments))
  for (i in instruments){
    p$tick()$print()
    tryCatch({
      tmpdf<-getSymbols(Symbols = i, src = "yahoo", from = startDate, auto.assign = FALSE);
      colnames(tmpdf)<- c("open","high","low","close","volume","adj.");
      # print(paste(path,"/", i,".csv",sep=""));
      write.zoo(tmpdf, paste(path,"/", i,".csv",sep=""), sep=",",row.names=FALSE);
    }, error = function(e) {cat("Error with instrument: ", i); e})
  }
}

#' Update existing database of instruments
#'
#' Update the existing database of instruments in the specified directory which keep all the data as CSV files.
#'
#' Nothing.
#'
#' @param path the path where all the CSV files are kept
#' @param startDate the startDate from when to update with new data where the default is given as today's date - 60 days
#'
#' @importFrom xts xts
#' @importFrom zoo write.zoo
#' @importFrom zoo index
#' @importFrom utils read.csv
#' @return Nothing
#' @export
#'
#' @examples
#' \dontrun{updateInstruments("/somewhere/trading")}
updateInstruments<-function(path, startDate=Sys.Date()-60){
  theFiles <- list.files(path=path, pattern=".csv")
  selCols <- c("open","high","low","close","volume","adj.")
  for (i in theFiles){
    tryCatch({
      data <- read.csv(paste(path,"/",i,sep=""))
      data <- xts(data[,selCols], order.by = as.Date(data[,"Index"],format="%Y-%m-%d"))
      lastHistoricalDate <- index(data[nrow(data),])

      recent <- getSymbols(Symbols = substr(i,1,nchar(i)-4), src = "yahoo", from = startDate, auto.assign = FALSE)
      colnames(recent) <- selCols

      pos <- match(as.Date(lastHistoricalDate, format="%Y-%m-%d"), index(recent))
      if (!is.na(pos)){
        if (pos == nrow(recent)) print("File already up-to-date")
        else if (pos < nrow(recent)){
          dt <- rbind(data,recent[(pos+1):nrow(recent),])
          write.zoo(dt, paste(path, "/", i, sep=""), sep=",", row.names=FALSE)
        }
      } else print("Error: dates do not match")
    }, error = function(e) {cat("Problem with instrument: ", i); e})
  }
}


#' Extract the earliest last observed date in all stored instruments
#'
#' Each instrument has it's own date range and this function runs through all
#' of them identifying the earliest last observed date. It is intended to help
#' you identify from which date you need to update your database.
#'
#' @param path the path where your data is stored as csv's
#'
#' @return the earliest observed last date
#' @export
#'
#' @examples
#' \dontrun{updateInstruments("/somewhere/trading")}
findLastDateInInstruments<-function(path){
  theFiles <- list.files(path=path, pattern=".csv")
  selCols <- c("open","high","low","close","volume","adj.")
  oldestLastHistoricalDate <- Sys.Date()
  p <- progress_estimated(length(theFiles))
  for (i in theFiles){
    p$tick()$print()
    tryCatch({
      fname <- paste(path,"/",i,sep="")
      command <- paste("wc -l", fname, "| awk '{print $1}'")
      nlines <- as.integer(system(command, intern = TRUE))
      data <- read.csv(fname)
      data <- xts(data[,selCols], order.by = as.Date(data[,"Index"],format="%Y-%m-%d"))
      lastHistoricalDate <- index(data[nrow(data),])
      if(lastHistoricalDate < oldestLastHistoricalDate)
        oldestLastHistoricalDate <- lastHistoricalDate
    }, error = function(e) {cat("Problem with instrument: ", i); e})
  }
  oldestLastHistoricalDate
}

#' Load existing instruments from the database into the R environment
#'
#' @param path the path where all the instruments data are stored as individual CSV files
#' @importFrom dplyr progress_estimated
#' @importFrom xts xts
#' @return the list of xts time series for each instrument
#' @export
#'
#' @examples
#' a<-1
loadExistingInstruments<-function(path){
  if(!dir.exists(paths = path)) stop("That directory does not exist!")
  theFiles <- list.files(path=path, pattern=".csv")
  selCols <- c("open","high","low","close","volume","adj.")
  instrumentslist<-list()
  p <- progress_estimated(length(theFiles))
  for (i in theFiles){
    p$tick()$print()
    tryCatch({
      data <- readInstrument(paste(path,"/",i,sep=""))
      data <- xts(data[,selCols], order.by = as.Date(data[,"Index"],format="%Y-%m-%d"));
      instrumentslist[[substr(i,1,nchar(i)-4)]]<-data
    }, error = function(e) {cat("Error with instrument: ", i); e})
  }
  instrumentslist
}


#' Read an instrument from a CSV formatted file
#'
#' The format of the file read mus look like in the details section.
#' This is also how your resulting tibble will be organized. No other
#' formats are supported.
#'
#' "Index","open","high","low","close","volume","adj."
#' "1999-11-18",32.546494,35.765381,28.612303,31.473534,62546300,27.494957
#' "1999-11-19",30.71352,30.758226,28.478184,28.880543,15234100,25.229753
#' "1999-11-22",29.551144,31.473534,28.657009,31.473534,6577800,27.494957
#' "1999-11-23",30.400572,31.205294,28.612303,28.612303,5975600,24.995413
#' "1999-11-24",28.701717,29.998211,28.612303,29.372318,4843200,25.659359
#'
#' @param fname the csv file to load
#'
#' @return a tibble representing the data
#' @importFrom readr read_csv cols col_double col_integer col_date
#' @export
#'
#' @examples
#' \dontrun{readInstrument("/somewhere/trading/AA.csv")}
readInstrument <- function(fname) {
  data <- readr::read_csv(fname,
                          col_types = readr::cols(Index = readr::col_date(format = "%Y-%m-%d"),
                                           open = readr::col_double(),
                                           high = readr::col_double(),
                                           low = readr::col_double(),
                                           close = readr::col_double(),
                                           volume = readr::col_integer(),
                                           adj. = readr::col_double()))
  data
}
