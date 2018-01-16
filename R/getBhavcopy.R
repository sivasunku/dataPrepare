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
#' @param  startDate - Date in %Y-%m-%d format.
#' @param  endDate   - Date in %Y-%m-%d format.
#' @param  fileName  - Name of the file where data to be stored with complete path.
#' @param  weekends  - default, it won't pull data for weekends. If needed enable the flag. Sometime Muhurat trade can be on weekends.
#' @rdname getBhavcopy
#' @return Returns a dataframe with all the accumulated data.
#' @export

getBhavcopy <- function(startDate = as.Date("2010-01-01",order="ymd"),
                        endDate   = as.Date("2010-01-10",order="ymd"),
                        fileName  = NULL,
                        weekends  = FALSE,
                        freq      = TRUE
                        ) {
  
  temp <- try( as.Date( startDate, format= "%Y-%m-%d" ) )
  if( class( temp ) == "try-error" || is.na( temp ) ) {
    stop("In getBhavcopy : startDate is not valid")
  }
  
  temp <- try( as.Date( endDate, format= "%Y-%m-%d" ) )
  if( class( temp ) == "try-error" || is.na( temp ) ) {
    stop("In getBhavcopy : endDate is not valid")
  }
  currDir <- getwd()
  setwd(tempdir())
  currDate = startDate
  zippedFile <- tempfile()
  
  while (currDate <= endDate){
    filenameDate = paste(as.character(currDate, "%y%m%d"), ".csv", sep = "")
    yearfilename=paste(as.character(currDate, "%y"),".csv", sep = "")
    downloadfilename=paste("cm", toupper(as.character(currDate, "%d%b%Y")), "bhav.csv", sep = "")
    temp =""
    
    #Sample url - https://www.nse-india.com/content/historical/EQUITIES/2018/JAN/cm01JAN2018bhav.csv.zip
    myURL = paste("http://nse-india.com/content/historical/EQUITIES/",
                  as.character(currDate, "%Y"), "/", 
                  toupper(as.character(currDate, "%b")),"/",
                  downloadfilename, ".zip", sep = "")
    tryCatch ( {
      #Download  & unzip file 
      download.file(myURL,zippedFile, quiet=TRUE, mode="wb")
      temp <- read.csv(unzip(zippedFile), sep = ",") 
      temp$TIMESTAMP <- as.Date(temp$TIMESTAMP, format="%d-%b-%Y")
      
      #Write the BHAVCOPY csv - datewise
      write.csv(temp,file=filenameDate,row.names = FALSE)
        
      #Write the csv in Monthly file
      if (file.exists(yearfilename)) {
        write.table(temp,file=yearfilename,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
      } else {
        write.table(temp,file=yearfilename,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
      }
    },
    error=function(err){
      print(paste(currDate, "-No Record"))
    }
  )
  
  currDate <- currDate+1
  #Do not try to get data for weekends.
  if (weekends == FALSE){
    while( (wday(currDate) == 1) || (wday(currDate == 7)) ){
        currDate <- currDate + 1
  }
  }
 } #end of while loop
  
  
 #Delete temp file - Bhavcopy
 junk <- dir(pattern="cm")
 file.remove(junk)
 return(yearfilename)
} #end of function
  