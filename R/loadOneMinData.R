#' makeOneMinDataObj
#' 
#' This makes the onemindata obtained from google drive into a data table
#' 
#' @author Siva Sunku
#' @keywords 
#' @details loads the data in a file(oneminute google drive data into a table). 
#'     It reads all the files in a directory.
#'        If file is a zip file, it needs to be processed ==== TBD === 
#'        Reads the text file and formats it into a data table with (Symbol, date, time , OHLC, volume)
#'        Returns the object with all the data read.
#' 
#' @param dirName - input Directory name
#' @param echoFlag - flag to display the file being processed 
#' @export
makeOneMinDataObj <- function(dirName = ".",echoFlag = FALSE){
  
  if (!dir.exists(dirName)){
   stop("In makeOneMinDataObj : dirName doesnot exist") 
  }
  files <- list.files(path = dirName,recursive = TRUE,pattern = "*.txt",full.names = TRUE)
  i <- 1
  d <- as.list(seq_len(length(files)))
  for ( f in files){
    temp           <- read.table(f,sep="," ,header = TRUE,stringsAsFactors = FALSE,fill = TRUE)
    if (ncol(temp) <= 8) {temp$Vol <- NA}
    if (ncol(temp) == 9) {temp[,9] <- NULL}
    print(sprintf("File: %s, cols %s",ncol(temp),f))
    colnames(temp) <- c("SYMBOL","DATE","TIME","OPEN","HIGH","LOW","CLOSE","VOLUME")
    d[[i]] <- temp
    i <- i+1

    # if (exists("cData")){
    #   cData <- rbind(cData,temp)
    # } else {
    #   cData <- temp
    # }
    
    if (echoFlag){
      print(sprintf("Completed : %s",f))
    }
  } #End of for loop
  cData <- do.call(rbind,d)
 return(cData)
}

#' getOneMinQuotes
#' @details This gets the quotes of a particular stock from the big one min data object. This returns the object as xts for that particular stock.
#' @param objectName - objectname from which quotes to be read
#' @param stock - Stock name to be filtered. Default ALL stocks
#' @param fut - default NULL. If fut prices are needed F1/F2 to be passed. currently only F1/F2 are available
#' @param trim - If True, only prices till 15:30 hours are returned. Default NULL.
#' @export
getOneMinQuotes <- function(objectName,
                     stock     = NULL,
                     fut = NULL,
                     trim = "T23:55") {
  if (is.xts(objectName)){
    stop("In getOneMinQuotes : objectNAme is an xts. Only data frame is needed.")
  }
  
  #If futures append _F1 to stock name
  if (!is.null(fut)) {
    stock <- paste(stock,fut,sep="")
  }
  
  dropcols <- c("SYMBOL")
  if (!is.null(stock)){
    temp <- objectName %>% 
      filter(SYMBOL == stock) %>%
      select(-one_of(dropcols))
  } else {
    temp <- objectName %>% 
      select(-one_of(dropcols))
  }
  
  #Make it as an xts object
  temp <- as.xts(
    read.zoo(temp,
             FUN=function(x) strptime(paste(x[,1],x[,2]),"%Y%m%d %H:%M"),
             index.column = c(1,2)
            )
    )
  
  if ( !is.OHLC(temp) ){
    warning("object doesn't contain OHLC")
  }
  
  #Return only data till 15:30 if trim is given.
  t <- paste("T00:00/",trim,sep="")
  return (temp[t] )
}
