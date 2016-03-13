a<-1


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
#' @return TRUE if we successfully downloaded all instruments. FALSE otherwise
#' @export
#'
#' @examples
#' a<-1
downloadInstruments<-function(instruments, path, startDate){
  for (i in instruments){
    print(i)
    tmpdf<-getSymbols(Symbols = i, src = "yahoo", from = startDate, auto.assign = FALSE)
    colnames(tmpdf)<- c("open","high","low","close","volume","adj.")
    write.zoo(tmpdf, paste(path,i,".csv",sep=""),sep=",",row.names=FALSE)
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
#' @return Nothing
#' @export
#'
#' @examples
updateInstruments<-function(path, startDate=Sys.Date()-60){
  theFiles <- list.files(path=path, pattern=".csv")
  for (i in theFiles){
    data <- read.csv(paste(path,i,sep=""))
    data <- xts(data[,c("open","high","low","close","volume","adj.")],
               order.by = as.Date(data[,"Index"],format="%Y-%m-%d"))
    lastHistoricalDate<- index(data[nrow(data),])

    recent <- getSymbols(Symbols = substr(i,1,nchar(i)-4), src = "yahoo", from = startDate, auto.assign = FALSE)
    colnames(recent) <- c("open","high","low","close","volume","adj.")

    pos <- match(as.Date(lastHistoricalDate, format="%Y-%m-%d"), index(recent))
    if (!is.na(pos)){
      if (pos == nrow(recent)) print("File already up-to-date")
      if (pos < nrow(recent)){
        dt<- NULL
        dt<- rbind(data,recent[(pos+1):nrow(recent),])
        write.zoo(dt,paste(path,i,sep=""),sep=",",row.names=FALSE)
      }
    }
    if (is.na(pos)) print("Error: dates do not match")
  }
}