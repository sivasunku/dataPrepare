#' getBhavcopy
#' 
#' This gets the bhavcopy from nse-india for given days. It will by default, do not download Sun/Sat.
#' 
#' 
#' @author Siva Sunku
#' @keywords bhavcopy
#' @note
#' 
#' This will create a trade object
#' @details  trades - Returns the empty trades data frame with given 'n' objects
#' @param  n - no. of rows
#' @rdname trades
#' @return blank trades dataframe
#' @export

getBhavcopy <- function(startDate = as.Date("2010-01-01",order="ymd"),
                        endDate   = as.Date("2010-01-10",order="ymd"),
                        fileName  = bhavcopy1
                        ) {
  
  temp <- try( as.Date( startDate, format= "%Y-%m-%d" ) )
  if( class( temp ) == "try-error" || is.na( d ) ) {
    stop("In getBhavcopy : startDate is not valid")
    
  }
#Define Working Directory, where files would be saved
setwd('E:/R/bhavcopy/')

#Define start and end dates, and convert them into date format
startDate = as.Date("2018-01-01", order="ymd")
endDate = as.Date("2018-01-08", order="ymd")

#work with date, month, year for which data has to be extracted
myDate = startDate
zippedFile <- tempfile()

while (myDate <= endDate){
  filenameDate = paste(as.character(myDate, "%y%m%d"), ".csv", sep = "")
  monthfilename=paste(as.character(myDate, "%y%m"),".csv", sep = "")
  downloadfilename=paste("cm", toupper(as.character(myDate, "%d%b%Y")), "bhav.csv", sep = "")
  temp =""
  
  #Generate URL
  myURL = paste("http://nse-india.com/content/historical/EQUITIES/",
                as.character(myDate, "%Y"), "/", 
                toupper(as.character(myDate, "%b")),"/",
                downloadfilename, ".zip", sep = "")
  
  #Sample url - https://www.nse-india.com/content/historical/EQUITIES/2018/JAN/cm01JAN2018bhav.csv.zip
  
  #retrieve Zipped file
  tryCatch({
    #Download Zipped File
    download.file(myURL,zippedFile, quiet=TRUE, mode="wb")
    
    #Unzip file and save it in temp 
    temp <- read.csv(unzip(zippedFile), sep = ",") 
    
    #Rename Columns Volume and Date
    colnames(temp)[9] <- "VOLUME"
    colnames(temp)[11] <- "DATE"
    
    #Define Date format
    temp$DATE <- as.Date(temp$DATE, format="%d-%b-%Y")
    
    #Reorder Columns and Select relevant columns
    #temp<-subset(temp,select=c("DATE","SYMBOL","OPEN","HIGH","LOW","CLOSE","LAST","VOLUME"))
    
    #Write the BHAVCOPY csv - datewise
    write.csv(temp,file=filenameDate,row.names = FALSE)
    
    #Write the csv in Monthly file
    if (file.exists(monthfilename))
    {
      write.table(temp,file=monthfilename,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
    }else
    {
      write.table(temp,file=monthfilename,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
    }
    
    #Write the file Symbol wise
    
    
    #Print Progress
    #print(paste (myDate, "-Done!", endDate-myDate, "left"))
  }, error=function(err){
    print(paste(myDate, "-No Record"))
  }
  )
  myDate <- myDate+1
  #print(paste(myDate, "Next Record"))
}

#Delete temp file - Bhavcopy
junk <- dir(pattern="cm")
file.remove(junk)

get.trades <- function( pf = "default"){
  if ( !is.valid.portfolio(pf) ) {
    stop("add.trades.position - portfolio is not a valid portfolio. Create one using portfolio before using the same.")
  }
  ipf <- get(pf,envir = .rules)
  return( ipf$trades[1:(ipf$tradeRow-1),] )
}


#' @details as.xts.trades - Convert the trades from dataframe to xts. Since xts is a strict matrix, all string columns are removed.
#' Index of the returned xts object is open date on which trade is initially opened.
#' @param t - trades
#' @return returns the trades/trxns data frame
#' @rdname trades
#' @export
as.xts.trades <- function(t){
  if ( !is.trades(t) ) {
    stop("as.xts.trades - t is not a trades object.")
  }
  t$symbols <- NULL
  t$direction <- ifelse(t$direction=="LONG",1,-1)
  t$exitTime <- NULL
  t$closeReason <- NULL
  t$openReason <- NULL
  
  xts(t[,-2],order.by = t[,2])
}

