if (!require(pbapply)) install.packages("pbapply")
library(pbapply)

changeWe <- function(data, entry = "entry") {
  if (class(data)[1]!="character") {
    if (any(class(data)=="data.frame")){
      if (is.null(data[[entry]])){
        stop("Must enter a column name for wordforms ('entry' by default).")
      }
      list.data <- as.list(data[[entry]])
      new_entry <- pbsapply(list.data, FUN = changeWe, entry = entry)
      data[[entry]] <- new_entry
      return(data)
    } else stop("Please input a character, data.frame or tbl object.")
  }
  
  source(".\\hangul_converter.r", encoding = "UTF-8")
  
  # if NA then NA
  if(is.na(data)){return(NA)}
  
  # change to jamo
  jamo <- toJamo(data)
  # change 'ㅚ' to 'ㅞ'
  jamo <- gsub("ㅚ|ㅙ", "ㅞ", jamo)
  # re-assemble into hangul strings
  output <- toHangul(jamo)
  # return the result
  return(output)
}
