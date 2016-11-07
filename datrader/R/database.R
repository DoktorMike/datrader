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
      data <- read.csv(paste(path,"/",i,sep=""));
      data <- xts(data[,selCols], order.by = as.Date(data[,"Index"],format="%Y-%m-%d"));
      instrumentslist[[substr(i,1,nchar(i)-4)]]<-data
    }, error = function(e) {cat("Error with instrument: ", i); e})
  }
  instrumentslist
}
