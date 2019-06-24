if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

cleanData <- function(data, entry="entry", filter = "shp") {
  # 'filter' argument 
  # Remove various non-phonetic (S)ymbols (e.g., "그__01" becomes "그"; "우리 학교" becomes "우리학교")
  # Remove (H)omphones
  # Remove all words except those of specific (P)art of speech

  if (!any(class(data)=="data.frame")){
    stop("Please input a data.frame or tbl object.")
  }
  if (is.null(data[[entry]])){
    stop("Must enter a column name for wordforms ('entry' by default).")
  }
  
  #if (class(data)[1]=="tbl_df") {data <- as.data.frame(data)}
  
  filter <- tolower(filter)
  
  if(grepl("p",filter)){
    options <- colnames(data)
    print(paste0("Your 'corpus' has the following columns: ", paste0(options, collapse = " ")))
    whatPOS <- readline(prompt = "Which of them represents POS? ")
    data <- extractPOS(data, POS = whatPOS)
  }
  
  if(grepl("s",filter)|grepl("h",filter)){
    data[[entry]] <- gsub("__[[:alnum:]]+", "", data[[entry]])
    data[[entry]] <- gsub("\\(.+\\)", "", data[[entry]])
    data[[entry]] <- gsub(" ", "", data[[entry]])
  }
  
  if(grepl("h",filter)){
    duplicated <- which(duplicated(data[[entry]]))
    if(length(duplicated)>0) data <- data[-duplicated,]
  }
  
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
