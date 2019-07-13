if (!require(ngram)) install.packages("ngram")
library(ngram)

nngram <- function(data, entry = "entry", convention = "klat", unit = NULL, ngramn = 2, sboundary = F) {
  while (nchar(convention) < 1) {
    convention <- readline(prompt = "You must specify a name for convention: ")
  }
  if (is.null(unit)) {
    unit <- convention
  }
  
  source(".\\hangul_converter.r", encoding = "UTF-8")
  data <- convertHangul(data, sboundary = sboundary)
  if (unit == "syllable"){
    if (sboundary == T) {
      stop("You can either specify 'unit = syllable' or 'sboundary = TRUE,' but not both.")
    }
    list_jamo <- as.list(data$jamo)
    x <- rapply(list_jamo, toHangul)
  } else {
    if (class(data)=="matrix"){
      data <- as.data.frame(data)
    }
    x <- data[[convention]]
  }
  
  wstop <- paste0(rep("#",ngramn-1),collapse="")
  ngram_data <- paste0(wstop,paste0(x, wstop, collapse="")) 
  ngr <- ngram(ngram_data, n=ngramn, sep="")
  return(ngr)
}


rlexgen <- function(x, entry = "entry", convention, unit, ngramn = 2, rlex = 1, num = 0, wordlength = 5) { 		
  # x: either ngram object (result of nngram()) or data.frame object
  # rlex: the number of phonotactic pseudo-lexicons to be generated
  if (class(x)[1] != "ngram") {
    ngr <- nngram(data = x, entry, convention, unit, ngramn) 
    wordlength <- mean(nchar(as.character(x[[entry]])))
    if (num ==0) num <- nrow(x)
  } else {
    ngr <- x
  }
  
  termi <- 0					# 생성된 렉시콘 개수를 counting하기 위한 변수
  wstop <- paste0(rep("#",(ngr@n)-1),collapse="")
  output <- vector()
  while(termi != rlex) {
    intermediate<-babble(ngr,num*15)		# 함수 babble은 ngr을 기준으로 markov chain 생성하는데, 단어길이 제어 및 중복단어 제거하면 떨어져나갈 outlier들을 고려하여 num에 비해 15배 생성.
    intermediate<-gsub(" ", "", intermediate, fixed = TRUE)
    intermediate<-unlist(strsplit(intermediate, wstop, fixed = TRUE)) # word boundary 기준으로 잘라준다.
    intermediate<-unique(intermediate)				  # 생성된 단어 상에서 중복단어 제거
    intermediate<-intermediate[intermediate != ""]				# 제거된 단어들을 지움
    tooLong <- which(nchar(intermediate)>(max(wordlength^2,wordlength*2)))
    if(length(tooLong)>0) {intermediate <- intermediate[-tooLong]}	# 과도하게 길게 생성된 단어를 제거함
    if(length(intermediate)>num+1){						# 생성된 random lexicon의 단어개수가 원래 lexicon보다 많을 경우에는,
      intermediate<-intermediate[2:(num+1)]				# 2번째부터 num+1번째까지를 random lexicon으로 삼음
      if (mean(nchar(intermediate))>wordlength*.9 & mean(nchar(intermediate))<wordlength*1.5){	# 평균단어길이 제약 통과시
        output<-cbind(output, intermediate)							# 출력값 output에 저장함
        termi<-termi+1
      }
    }
  }
  colnames(output) <- paste("lexicon" ,c(1:rlex))
  return(output)
}