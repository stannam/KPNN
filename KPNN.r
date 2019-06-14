# 0. read sample file
if (!require(readxl)) install.packages("readxl")
library(readxl)
library(httr)
GET("https://www.dropbox.com/s/4o12muqa6z2j67r/07a_top1000.uni.xlsx?dl=1", write_disk(tf <- tempfile(fileext = ".xlsx")))
raw <- read_xlsx(path = tf, col_names = T)

# 1. data cleaning
source(".\\data_cleaning.r", encoding = "UTF-8")

data <- cleanData(raw)

# 2. generate PNN (based on 1)
source(".\\generate_PNN.r", encoding = "UTF-8")

net <- genPNN(data = data,
              entry = "entry",
              convention = "klat",
              # surface = T,  #(surface is a parameter to be added for surfacing orthographic form)
              unit = "klat",
              deletion = TRUE,
              pajek = F)

# 2.1 do something with the generated PNN
# 2.1.1 plot the network
plot(net, vertex.label=data$entry,                 # plot the generated PNN, 
     vertex.shape="none", vertex.label.cex=0.8,    # with such vertex parameters
     edge.width=2, edge.arrow.size=0, margin=0)    # and such edge parameters

# get number of neighbours for each node (word).
new_data <- noNeighbor(data = data,
                       pnn = net,
                       append = TRUE)

number_of_neighbours_as_list <- noNeighbor(pnn = net,
                                           append = FALSE)


# calculate the mean value of the neighbours' frequencies
new_data <- meanNeighbor(data = data,
                         pnn = net,
                         attribute = "afreq",
                         append = TRUE)

mean_freq_of_neighbours_as_list <- meanNeighbor(data = data,
                                                pnn = net,
                                                attribute = "afreq",
                                                append = FALSE)

# 3. describe ngram and generate phonotactic pseudo-lexicons from the ngram (based on 1) 
source(".\\ngram.r", encoding = "UTF-8")

K_ngram <- nngram(data, 
                  entry = "entry",                  # entry: the name of the column where entries are located in 'data'
                  convention = "klat",
                  unit = "klat",
                  ngramn = 2)

ngram_result_in_table <- get.phrasetable(K_ngram)   # describes ngram object in a table

fake_lexicons <- rlexgen(K_ngram,                   # generates phonotactic pseudo-lexicons simply based on the ngram information.
                         rlex = 5,                  # rlex: the number of phonotactic pseudo-lexicons to be generated.
                         num = 30,                  # num: the number of words in a pseudo-lexicon
                         wordlength = 5)            # wordlength: mean word length of the pseudo-lexicon


# 4. Generate pseudo-lexicons directly from a real lexicon (which underwent the data cleaning in 1.).
source(".\\ngram.r", encoding = "UTF-8")

fake_lexicons <- rlexgen(data,                      # generates phonotactic pseudo-lexicons directly from a lexicon.
                         entry = "entry",           # entry: the name of the column where entries are located in 'data'
                         convention = "klat",
                         unit = "klat",
                         ngramn = 2,
                         num = 30,
                         rlex = 5)
