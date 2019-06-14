if (!require(doParallel)) install.packages("doParallel")
library(doParallel)

if (!require(igraph)) install.packages("igraph")
library(igraph)

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

genPNPair <- function (x, deletion = T) {
  result <- vector
  result <- foreach (i=1:length(x), .combine='rbind', .packages="base") %dopar% {
    output <- vector()
    if (deletion == T){                                                     # include PN by deletion / insertion
      for (j in i:length(x)) {
        if(nchar(x[j]) > (nchar(x[i])-2) & nchar(x[j]) < (nchar(x[i])+2)) { # 음소 개수가 같거나, 1개 차이가 날 경우에만
          if(adist(x[i], x[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
        }
      }
    } else {                                                          # only consider PN by substitution
      for (j in i:length(x)) {
        if(nchar(x[j]) == nchar(x[i])) {                                # 음소 개수가 같을 경우에만
          if(adist(x[i], x[j])==1) output <- rbind(output,c(i,j))				# adist() 연산을 해서, 그 결과가 1인 경우에만 output에 기록하기
        }
      }
    }
    output
  }
  alarm()
  return(result)
}

genPNN <- function(data, entry = "entry", convention = "klat", unit = NULL, deletion = T, pajek = F) {
  if (is.null(unit)) {
    unit <- convention
  }

  source(".\\hangul_converter.r", encoding = "UTF-8")
  data <- convertHangul(data, entry = entry, convention = convention)
  
  if (unit == "syllable"){
    list_jamo <- as.list(data$jamo)
    x <- rapply(list_jamo, toHangul)
  } else {
    x <- data[[convention]]
  }
  
  PNPair <- genPNPair(x, deletion)
  data$id <- 1:nrow(data)
  if (pajek == T) {
    pajek <- paste("*Vertices", length(x), sep=" ")
    for (i in 1:length(x)){
      pajek <- c(pajek, paste(i, data[[convention]][i], sep=" "))
    }
    result <- paste(PNPair[,1],PNPair[,2],sep=" ")
    pajek <- c(pajek, "*Edges", result)
    pajek_path <- choose.files(caption = "Save as Pajek .net file?", multi = F)
    if (!length(pajek_path) == 0) {
      write(pajek, file=pajek_path)
    }
  }
  net <- graph_from_data_frame(d=PNPair, vertices=data,directed=F)
  return(net)
}
