if (!require(parallel)) install.packages("parallel")
library(parallel)

if (!require(doSNOW)) install.packages("doSNOW")
library(doSNOW)

if (!require(igraph)) install.packages("igraph")
library(igraph)

genPNPair <- function (x, deletion = T) {
  cl <- makeCluster(detectCores()-1)
  registerDoSNOW(cl)
  
  pb <- txtProgressBar(max = length(x), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  result <- vector
  result <- foreach (i=1:length(x), .combine='rbind', .options.snow = opts) %dopar% {
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
  close(pb)
  stopCluster(cl)
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

noNeighbor <- function(data, pnn, append = F){
  if(!exists("pnn")){
    stop("A PNN must be specified!")
  }
  if(append == T){
    if(!exists("data")){
      stop("A corpus must be specified if you want something to be attached to it")
    }
  }
  no_neighbors <- vector()
  for (i in 1:gorder(net)){
    neighbor_list <- as.list(neighbors(pnn,i))
    no_neighbors[i] <- length(neighbor_list)
  }
  if(append == T){
    data[["number_of_neighbors"]] <- no_neighbors 
    output <- data
  } else {
    output <- no_neighbors
  }
  return(output)
}

meanNeighbor <- function(data, pnn, attribute, append = F){
  if(!exists("data")|!exists("pnn")|!exists("attribute")){
    stop("You must specify a corpus, a PNN, and an attribute to calculate the mean of!")
  }
  no_neighbors <- vector()
  mean_neighbor_att <- vector()
  for (i in 1:gorder(net)){
    neighbor_list <- as.list(neighbors(pnn,i))
    no_neighbors[i] <- length(neighbor_list)

    if(no_neighbors[i]!=0) {
      att_of_neighbors <- lapply(neighbor_list, vertex_attr, graph=pnn, name=attribute)
      mean_neighbor_att[i] <- mean(unlist(att_of_neighbors))
    } else {
      mean_neighbor_att[i] <- NA
    }
  }
  if(append == T){
    newcol <- paste0("mean_neighbor_",attribute)
    data[[newcol]] <- mean_neighbor_att
    output <- data
  } else {
    output <- mean_neighbor_att
  }
  return(output)
}
