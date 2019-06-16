removeSaisiot <- function(data, entry = "entry", id = 1){
  if (class(data)[1]!="character") {
    if (any(class(data)=="data.frame")){
      if (is.null(data[[entry]])){
        stop("Must enter a column name for wordforms ('entry' by default).")
      }
      list.data <- as.list(data[[entry]])
      result <- rapply(list.data, removeSaisiot)
      result <- as.data.frame(matrix(result, ncol=4, byrow=T), stringsAsFactors=F)
      colnames(result) <- c("id","original","no_saisiot","user_choice")
      result$id <- 1:nrow(data)
      result <- result[complete.cases(result), ]
      return(result)
    } else stop("Please input a character, data.frame or tbl object.")
  }
  source(".\\hangul_converter.r", encoding = "UTF-8")
  jamo <- toJamo(data, removeEmptyOnset = F)
  CV <- CV_mark(jamo)
  split_jamo <- unlist(strsplit(jamo,""))
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
