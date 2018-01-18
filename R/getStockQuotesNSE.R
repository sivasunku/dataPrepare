#' getStockQuotesNSE
#' 
#' This gets the daily data from the bhavcopy csv into a data frame(OHLC)
#' 
#' @author Siva Sunku
#' @keywords getStockQuotes
#' @note 
#' This will return an OHLC object with all the details for a given stock.
#' It will read the fileName(BHAVcopy archive), and return the data specific to that particular stock as a OHLC dataframe.
#' 
#' @details Copies the bhavcopies from nse and keeps the csv (yearlywise) in the given directory.
#' 
#' @param  startDate - Date in YMD format.
#' @param  endDate   - Date in YMD format.
#' @param  fileName  - File name where the quotes(bhavcopy) are stored
#' @param  objectName - if the data is stored as rda. Either filename or objectname should be there, not both and neither none.
#' @param  stock     - Equity data to be downloaded
#' @rdname getStockQuotesNSE
#' @return Returns a dataframe with all the OHLC data
#' @export
#' 
getStockQuotesNSE <- function(
                       stock     = NULL,
                       fileName  = NULL,
                       objectName = NULL) {
  temp <- NULL
  #Housekeeping
  if (is.null(fileName)  && is.null(objectName)){
    stop("In getStockQuotesNSE : both filename and objectname are null")
  }
  if (!is.null(fileName) && (!file.exists(fileName)) ){
    stop("In getStockQuotesNSE : filename doesn't exist")
  }
  
  
  #if file name is given make object out of filename
  if ( !is.null(fileName) ){
    temp <- read.table(fileName,sep="," ,header = TRUE,stringsAsFactors = FALSE,fill = TRUE)
  }
  #If object is given, move object to tempVariable
  if ( !is.null(objectName) ){
    temp <- objectName  
  }

  #get only required fields
  dropcols <- c("SYMBOL","SERIES")
  temp <- temp %>% 
          select("SYMBOL","SERIES","OPEN","HIGH","LOW","CLOSE","TOTTRDQTY","TIMESTAMP") %>%
          filter(SERIES == "EQ", SYMBOL == stock) %>%
          select(-one_of(dropcols))
  
  #Make it as an xts object
  temp <- as.xts(
             read.zoo(temp,
                      FUN=function(x) as.POSIXlt(strptime(x,"%Y-%m-%d")),
                      index.column = 6
                      )
          )
  
  colnames(temp) <- c("Open","High","Low","Close","Volume")

 return(temp)
}