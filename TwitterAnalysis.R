

library(tidyverse) # metapackage of all tidyverse packages

# Input data files are available in the read-only "../input/" directory
# For example, running this (by clicking run or pressing Shift+Enter) will list all files under the input directory

list.files(path = "../input")



# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:52:04.758621Z","iopub.execute_input":"2022-05-31T18:52:04.760452Z","iopub.status.idle":"2022-05-31T18:52:04.999980Z"}}
LatestData <- read.csv("../input/latest/LatestData.csv")
View(LatestData)

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:52:17.761714Z","iopub.execute_input":"2022-05-31T18:52:17.789899Z","iopub.status.idle":"2022-05-31T18:52:17.959580Z"}}
library(tm)
corpus <- iconv(LatestData$Data)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:52:22.902398Z","iopub.execute_input":"2022-05-31T18:52:22.903863Z","iopub.status.idle":"2022-05-31T18:52:22.950573Z"}}
corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:52:35.125230Z","iopub.execute_input":"2022-05-31T18:52:35.126739Z","iopub.status.idle":"2022-05-31T18:52:35.196274Z"}}
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:52:47.102856Z","iopub.execute_input":"2022-05-31T18:52:47.104409Z","iopub.status.idle":"2022-05-31T18:52:47.135329Z"}}
corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:52:59.451394Z","iopub.execute_input":"2022-05-31T18:52:59.452952Z","iopub.status.idle":"2022-05-31T18:53:00.108899Z"}}
cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:53:12.687990Z","iopub.execute_input":"2022-05-31T18:53:12.689531Z","iopub.status.idle":"2022-05-31T18:53:13.192249Z"}}
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:53:27.326593Z","iopub.execute_input":"2022-05-31T18:53:27.328033Z","iopub.status.idle":"2022-05-31T18:53:30.402077Z"}}
tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:53:40.936807Z","iopub.execute_input":"2022-05-31T18:53:40.938194Z","iopub.status.idle":"2022-05-31T18:53:41.757626Z"}}
w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50))

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:54:16.010676Z","iopub.execute_input":"2022-05-31T18:54:16.012229Z","iopub.status.idle":"2022-05-31T18:54:17.920367Z"}}
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:54:29.871033Z","iopub.execute_input":"2022-05-31T18:54:29.872883Z","iopub.status.idle":"2022-05-31T18:54:30.029520Z"}}
library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle',
           rotateRatio = 0.5,
           minSize = 1)

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:54:48.416494Z","iopub.execute_input":"2022-05-31T18:54:48.418089Z","iopub.status.idle":"2022-05-31T18:54:48.932414Z"}}
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T18:55:02.095198Z","iopub.execute_input":"2022-05-31T18:55:02.097217Z","iopub.status.idle":"2022-05-31T18:55:02.120400Z"}}
tweets <- iconv(LatestData$Data)

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T19:06:05.603257Z","iopub.execute_input":"2022-05-31T19:06:05.604625Z","iopub.status.idle":"2022-05-31T19:11:29.542435Z"}}
s <- get_nrc_sentiment(tweets)
head(s)

# %% [code] {"execution":{"iopub.status.busy":"2022-05-31T19:11:49.613340Z","iopub.execute_input":"2022-05-31T19:11:49.615222Z","iopub.status.idle":"2022-05-31T19:11:49.698464Z"}}
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')