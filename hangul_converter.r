if (!require(KoNLP)) install.packages("KoNLP")
library(KoNLP)

convertHangul <- function(data, entry = "entry", convention = "klat"){
  if (class(data)[1]!="character") {
    if (any(class(data)=="data.frame")){
      if (is.null(data[[entry]])){
        stop("Must enter a column name for wordforms ('entry' by default).")
      }
      list.data <- as.list(data[[entry]])
      result <- rapply(list.data, convertHangul, entry = entry, convention = convention)
      result <- as.data.frame(matrix(result, ncol=2, byrow=T), stringsAsFactors=F)
      colnames(result) <- c("jamo",convention)
      result <- cbind(data, result)
      return(result)
      } else stop("Please input a character, data.frame or tbl object.")
    }
  
  jamo <- toJamo(data)
  if(exists("transcription_location")){
    klat <- toKlat(jamo,convention = convention, environment(), transcription_location = transcription_location)
  } else {
    klat <- toKlat(jamo,convention = convention, environment())
  }
  
  result <- cbind(jamo,klat)
  colnames(result) <- c("jamo",convention)
  return(result)
}

toJamo <- function(data, removeEmptyOnset = TRUE) {
  criteria_DoubleCoda <- read.table(file=".\\criteria\\double_coda.csv", sep = ",", header=TRUE)
  
  syllable <- convertHangulStringToJamos(data)
  for (j in 1:length(syllable)) {
    DC <- match(substr(syllable[j],3,3), criteria_DoubleCoda$double)
    if (is.na(DC) == FALSE) {					#겹받침을 둘로 나눔 (eg. "ㄳ" -> "ㄱㅅ")
      substr(syllable[j], 3, 4) <- as.character(criteria_DoubleCoda$separated[DC])
    } 
    phonemic <- unlist(strsplit(syllable[j], split=""))	# 'syllable'의 j번째 element를 각 자모단위로 분리해서 새로운 vector 'phonemic'에 넣습니다.
    if (removeEmptyOnset == TRUE){
      if(phonemic[1] == "ㅇ") {phonemic[1] <- ""}		# 첫번째 자모(즉, 초성)가 'ㅇ'이면, 그것을 제거합니다.
    }
    syllable[j] <- paste(phonemic, collapse="")		# 'phonemic'을 결합해서 다시 음절단위로 만듭니다. 그러나 초성의 ㅇ은 제거된 상태입니다.
  }
  jamo <- paste(syllable, collapse="")				# 그 결과를 jamo에 저장합니다.
  return(jamo)
}

toKlat <- function(jamo, convention = "klat", env = NULL, transcription_location = NULL) {
  if (convention == "klat"){
    Klattese <- read.table(file = ".\\criteria\\klattese.csv", sep = ",", header=T)
  } else {
    while(length(transcription_location) == 0){
      transcription_location <- choose.files(default = "", 
                                             caption = "Select a jamo-to-phonetic-symbol table", multi = F)
      transcription_location <<- transcription_location
    }
    Klattese <- read.table(file = transcription_location, sep = ",", header=T)
  }
  
  letter <- unlist(strsplit(jamo,split=""))
  for (l in 1:length(letter)){
    if(is.na(match(letter[l], Klattese$C))==T){
      letter[l] <- as.character(Klattese$VKlattese[match(letter[l], Klattese$V)])
    } else {
      letter[l]<-as.character(Klattese$CKlattese[match(letter[l],Klattese$C)])}
  }
  klat <- paste(letter,collapse="")
  return(klat)
}

CV_mark <- function(input){
  CV_ref <- read.table(file = ".\\criteria\\klattese.csv", sep = ",", header=T)
  output <- vector()
  phoneme <- unlist(strsplit(input,split=""))
  for (j in 1:length(phoneme)){
    if (is.na (match (phoneme[j], CV_ref$C)) == TRUE) {
      phoneme[j]="V"
    }
    else {phoneme[j]="C"
    }
  }
  output <- paste(phoneme, collapse="")
  return(output)
}

toHangul <- function(input, emptyOnset = F){
  if (!is.character(input) | nchar(input) == 0) {
    stop("Input must be legitimate character!")
  }
  if (emptyOnset == F){
    cv <- CV_mark(input)
    input_split <- unlist(strsplit(input,split=""))
    cv_split <- unlist(strsplit(cv,split=""))
    if (cv_split[1] == "V") {                        # add empty 'ㅇ' before a V-starting word.
      input_split <- c("N", input_split)
      cv_split <- c("C", cv_split)
    }
    i = 2
    j = length(input_split)
    while (i <= j){
      if (cv_split[i] == "V"){
        if (cv_split[i-1] == "V"|input_split[i-1] == "ㅇ") {
          cv_split <- c(cv_split[1:(i-1)], "C", cv_split[i:length(cv_split)])
          input_split <- c(input_split[1:(i-1)], "N", input_split[i:length(input_split)])
        }
      }
      i = i + 1
      j = length(input_split)
    }
    input_split <- gsub("N", "ㅇ", input_split)
    input <- paste(input_split, collapse="")
  }

  tryCatch(
    output <- HangulAutomata(input, isForceConv = T),
    error = function(e) {
      confirm <- ""
      input_split <- unlist(strsplit(input,split=""))
      fortis_location <- grep("ㅃ|ㅉ|ㄸ|ㄲ|ㅆ", input_split)
      for (i in fortis_location){
        if (!is.na(cv_split[i+3])){
          if (cv_split[i+3] == "C"){
            input_split[i] <- HangulAutomata(paste(input_split[i:(i+2)],collapse=""))
            input_split[i+1:i+2] <- "X"
          } else {
            input_split[i] <- HangulAutomata(paste(input_split[i:(i+1)],collapse=""))
            input_split[i+1] <- "X"
          }
        } else {
          input_split[i] <- HangulAutomata(paste(input_split[i:length(input_split)],collapse=""))
          input_split[(i+1):length(input_split)] <- "X"
        }
      } 
      another_output <- paste(input_split,collapse="")
      another_output <- gsub("X","",another_output)
      another_output <- HangulAutomata(another_output)
      #for (i in 1:length(fortis_location)){
      #  another_output <- paste0(another_output,intermediate_syllable[i],input_split[fortis_location[i]])
      #}
      while (tolower(confirm) != "y"){
        confirm <- readline(prompt = paste0(input," = ", another_output, ".... Is it correct (y/n)? (y나 Y 대신 'ㅛ'입력 가능) "))
        if (confirm =="ㅛ") {confirm <- "y"}
        if (tolower(confirm) != "y"){
        userinput <- readline(prompt = paste0("What should be the Hangul syllables for ","\"", input,"\"?     \n>> "))
        another_output <- userinput
        }
      }
      output <<- another_output
    }
  )
  return(output)
}


applyRulesToHangul <- function(){
  # A function which applies phonological rules to Korean words written in Hangul string or Jamo.
}
