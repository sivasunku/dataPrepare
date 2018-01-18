#' getBhavcopy
#' 
#' This gets the bhavcopy from nse-india for given days and puts the data into the file given in params.
#' 
#' @author Siva Sunku
#' @keywords bhavcopy
#' @note 
#' This will create a trade object
#' 
#' @details Copies the bhavcopies from nse and keeps the csv (yearlywise) in the given directory.
#' 
#' @param  startDate - Date in YMD format.
#' @param  endDate   - Date in YMD format.
#' @param  dirName   - default - current working directory. Name of the directory where files should be downloaded.
#' @param  weekends  - default, it won't pull data for weekends. If needed enable the flag. Sometime Muhurat trade can be on weekends.
#' @param  dayFile   - default - TRUE. If yearlyfile is true, if you wish you can remove all daily files. It is valid only when yearlyFile is True.
#' @param  monFile   - default - FALES. If monthly file to be created.
#' @param  YearlyFile - default - FALSE. If given, a consolidated file with all the data yearwise is kept.
#' @param  rdaObj    - If rdaObj is given, complete data is created as rda file, with bhavcopy as objectname(dataframe)

#' @rdname getBhavcopy
#' @return Returns a dataframe with all the accumulated data.
#' @export
getBhavcopy <- function(startDate = as.Date("2010-01-01",order="ymd"),
                        endDate   = as.Date("2010-01-10",order="ymd"),
                        dirName    = NULL,
                        weekends   = FALSE,
                        yearlyFile = TRUE,
                        dayFile    = TRUE,
                        monFile    = TRUE,
                        rdaObj    = NULL
                        ) {
  
  temp <- try( as.Date( startDate, format= "%Y-%m-%d" ) )
  if( class( temp ) == "try-error" || is.na( temp ) ) {
    stop("In getBhavcopy : startDate is not valid")
  }
  
  temp <- try( as.Date( endDate, format= "%Y-%m-%d" ) )
  if( class( temp ) == "try-error" || is.na( temp ) ) {
    stop("In getBhavcopy : endDate is not valid")
  }
  
  if (startDate > endDate){
    stop("In getBhavcopy : startDate is greater than endDate")
  }
  
  if (is.null(dirName)){dirName <- getwd()}
  if (!dir.exists(dirName)){
    stop("In getBhavcopy: directory name given in dirName doesn't exist")
  }
  
  
  currDir <- getwd()

  #Create a directory named dataDownload in the specified path & clean it for fresh download.  
  setwd(dirName)
  dir.create("dataDownload",showWarnings = FALSE)
  setwd("dataDownload")
  junk <- dir()
  file.remove(junk)
  
  currDate = as.Date(startDate)
  zippedFile <- tempfile()
  
  while (currDate <= endDate){
    filenameDate = paste("dailybhavcopy",as.character(currDate, "%y%m%d"), ".csv", sep = "")
    yearfilename=paste("bhavcopy",as.character(currDate, "%Y"),".csv", sep = "")
    monfilename=paste("bhavcopy",as.character(currDate, "%Y-%m"),".csv", sep = "")
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

      #Write the BHAVCOPY csv - per day one
      if ( dayFile == TRUE ){
        write.csv(temp,file=filenameDate,row.names = FALSE)
      }
      #Write the csv in monthly file
      if (monFile) {
        if (file.exists(monfilename)) {
          write.table(temp,file=monfilename,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
        } else {
          write.table(temp,file=monfilename,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
        }
      }
      #Write the csv in Yearly file
      if (yearlyFile) {
        if (file.exists(yearfilename)) {
          write.table(temp,file=yearfilename,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
        } else {
          write.table(temp,file=yearfilename,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
        }
      }
      
      if(!is.null(rdaObj)){
        if (file.exists(rdaObj)) {
          write.table(temp,file=rdaObj,sep=",", eol="\n", row.names = FALSE, col.names = FALSE, append=TRUE)
        } else {
          write.table(temp,file=rdaObj,sep=",", eol="\n", row.names = FALSE, col.names = TRUE, append=FALSE)
        }
      }
    },
    error=function(err){
      print(sprintf("For Date: %s , error Message : %s", currDate,err) )
    }
  )
  
  currDate <- currDate+1
  #Do not try to get data for weekends.
  if (weekends == FALSE){
    while( (wday(currDate) == 1) || (wday(currDate) == 7) ){
        currDate <- currDate + 1
    }
  }
 } #end of while loop
  
 
 #Delete temp file - Bhavcopy
 junk <- dir(pattern="cm")
 file.remove(junk)

 if (!is.null(rdaObj)){
   rdaFileName <- rdaObj
   temp <- read.table(rdaFileName,sep="," ,header = TRUE,stringsAsFactors = FALSE,fill = TRUE)
   assign(rdaObj,temp)
   save(list = rdaObj,file = paste(rdaFileName,".rdata",sep=""))
   file.remove(rdaFileName)
 }
 
 setwd(currDir)
 return(paste(currDir,"/",yearfilename,sep=""))
} #end of function
