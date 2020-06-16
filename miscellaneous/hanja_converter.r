if (!require(httr)) install.packages("httr")
library(httr)
GET("https://raw.githubusercontent.com/suminb/hanja/develop/hanja/table.yml", write_disk(tf <- tempfile(fileext = ".yml")))
raw_reference <- readLines(con = tf)
raw2 <- gsub("u'|\':|',","", raw_reference[2:(length(raw_reference)-1)])
concordance_table <- read.table(text = raw2, 
                                sep = " ", 
                                col.names = c("hanja","hangul"),
                                stringsAsFactors = F)
concordance_table[,1] <- paste0("0x",substr(concordance_table[,1], 3, 6))
concordance_table[,2] <- paste0("0x",substr(concordance_table[,2], 3, 6))
concordance_table = apply(concordance_table, MARGIN = c(1,2), FUN = intToUtf8)

hanjaToHangul <- function(input){
  pattern<-"^\\p{Han}"
  hanjaLoc <- grep(pattern, input, perl=T)
  for (i in hanjaLoc)
  input[i] <- hanjaWordToHangul(input[i])
  return(input)
}

hanjaWordToHangul <- function(word){
  syllable <- unlist(strsplit(word,""))
  output <- concordance_table[match(syllable, concordance_table),2]
  output <- paste0(output,collapse="")
  return(output)
}
