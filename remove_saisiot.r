if (!require(pbapply)) install.packages("pbapply")
library(pbapply)

removeSaisiot <- function(data, entry = "entry", id = 1){
  if (class(data)[1]!="character") {
    if (any(class(data)=="data.frame")){
      if (is.null(data[[entry]])){
        stop("Must enter a column name for wordforms ('entry' by default).")
      }
      list.data <- as.list(data[[entry]])
      result <- unlist(pbsapply(list.data, removeSaisiot))
      result <- as.data.frame(matrix(result, ncol=4, byrow=T), stringsAsFactors=F)
      colnames(result) <- c("id","original","no_saisiot","user_choice")
      result$id <- 1:nrow(data)
      result <- result[which(!is.na(result$original)), ]
      return(result)
    } else stop("Please input a character, data.frame or tbl object.")
  }
  source(".\\hangul_converter.r", encoding = "UTF-8")
  jamo <- toJamo(data, removeEmptyOnset = F)
  if(length(grep("ㅅ",jamo))>0){
    
    split_jamo <- unlist(strsplit(jamo,""))
    CV <- CV_mark(jamo)
    split_CV <- unlist(strsplit(CV,""))
    siot_location <- grep("ㅅ",split_jamo[2:length(split_jamo)])
    siot_location <- siot_location + 1
    saisiot_criteria <- c("V","C","C")
    for (i in rev(siot_location)){
      if(i < length(split_jamo)){
        check_saisiot <- split_CV[(i-1):(i+1)]
        if (all(check_saisiot == saisiot_criteria)) {
          split_CV <- c(split_CV[1:(i-1)], split_CV[(i+1):length(split_CV)])
          split_jamo <- c(split_jamo[1:(i-1)], split_jamo[(i+1):length(split_jamo)])
        }
      }
    }
    no_saisiot <- paste0(split_jamo,collapse="")
  } else no_saisiot <- jamo
  
  if(jamo != no_saisiot){
    no_saisiot <- toHangul(no_saisiot, emptyOnset = T)
    output <- cbind(id,data,no_saisiot,"")
    output <- as.data.frame(matrix(output, ncol=4, byrow=T), stringsAsFactors=F)
    colnames(output) <- c("id","original","no_saisiot","user_choice")
  } else {
    output <- data.frame(id = NA, original = NA, no_saisiot = NA, user_choice = NA)
  }
    return(output)
  
}

updateSaisiot <- function(x, y, entry = "entry"){
  if (any(class(x)=="data.frame")){
    if (is.null(x[[entry]])){
      stop("Must enter a column name for wordforms ('entry' by default).")
    }
  } else if(class(x)!="character"){
    stop("Please input a character, data.frame or tbl object.")
  }
  x[["id"]] <- 1:nrow(x)
  y$original <- as.character(y$original)
  y$no_saisiot <- as.character(y$no_saisiot)
  y$user_choice <- as.character(y$user_choice)
  empty_row <- which(y$user_choice == ""|is.na(y$user_choice))
  for(i in empty_row){
    y$user_choice[i]<- y$original[i]
  }
  id_ref <- y$id
  if(!all(x[[entry]][id_ref]==y$original)){
    stop("'Corpus' and 'saisiot' table must match.")
  }
  x[[entry]][y$id] <- y$user_choice
  return(x)
}
  