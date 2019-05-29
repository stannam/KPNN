# read sample file
if (!require(readxl)) install.packages("readxl")
library(readxl)
library(httr)
GET("https://www.dropbox.com/s/4o12muqa6z2j67r/07a_top1000.uni.xlsx?dl=1", write_disk(tf <- tempfile(fileext = ".xlsx")))
raw <- read_xlsx(path = tf, col_names = T)

# data cleaning
source(".\\data_cleaning.r", encoding = "UTF-8")

data <- cleanData(raw)

# generate PNN
source(".\\generate_PNN.r", encoding = "UTF-8")

net <- genPNN(data = data,
              entry = "entry",
              convention = "klat",
              unit = "syllable",
              deletion = TRUE,
              pajek = T)

# plot the PNN
plot(net, vertex.label=data$entry, vertex.shape="none", vertex.label.cex=0.8, edge.width=2, edge.arrow.size=0, margin=0)
