syllableWordLoc <- function(data, entry, rules = T){
  source(".\\hangul_converter.r", encoding = "UTF-8")
  if (rules == F) {
    surface <- data[[entry]]
  } else {
    if (rules == T) {rules <- "pacstnh" }
    surface_table <- applyRulesToHangul(data = data, entry = entry, rules = rules)
    surface <- surface_table$surface
  }
  
  
  # apply rules in order to get the surface form. 
  # and then assemble surface forms into hangul strings ==> surface
  
  surface <- strsplit(surface, split="")
  
  # word initial
  word_initial <- unlist(sapply(surface, head, 1))
  firsttable<-table(word_initial)
  
  # word final
  word_final <- unlist(sapply(surface, tail, 1))
  finaltable <- table(word_final)
  
  # word medial
  word_medial <- unlist(sapply(surface, wordMedial))
  midtable <- table(word_medial)
  
  # combine all into a list and return
  result <- list("word_initial" = firsttable, "word_medial" = midtable, "word_final" = finaltable)
  return(result)
}

wordMedial <- function(input){
  if(length(input)>2){
    output <- input[2:(length(input)-1)]
  } else(output <- vector())
  return(output)
}
