if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

cleanData <- function(data, entry="entry") {
  if (!any(class(data)=="data.frame")){
    stop("Please input a data.frame or tbl object.")
  }
  if (is.null(data[[entry]])){
    stop("Must enter a column name for wordforms ('entry' by default).")
  }
  
  #if (class(data)[1]=="tbl_df") {data <- as.data.frame(data)}
  
  data[[entry]] <- gsub("__[[:alnum:]]+", "", data[[entry]])
  data[[entry]] <- gsub("\\(.+\\)", "", data[[entry]])
  data[[entry]] <- gsub(" ", "", data[[entry]])
  duplicated <- which(duplicated(data[[entry]]))
  if(length(duplicated)>0) data <- data[-duplicated,]
  
  return(data)
}

extractPOS <- function(data, POS=NULL) {
  if (!any(class(data)=="data.frame")){
    stop("Please input a data.frame or tbl object.")
  }
  if (is.null(POS)){
  stop("Please enter a value for POS.")
  }
  
  if (!is.null(data[[POS]])){
    options <- as.factor(data[[POS]])
    options <- paste(levels(options)[1:5],collapse=" ")
    print(paste0("Some of your POS codes are: ", options))
    whatPOS <- readline(prompt = "Enter a POS code? ")
    data <- data[grep(whatPOS,data[[POS]]),]
  }
  
  return(data)
}
