# set the working directory
# setwd("D:/")

# import packages
packages <- c("stm", "Rtsne", "geometry", "RColorBrewer")
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(stm)
library(Rtsne)
library(geometry)
library(RColorBrewer)

# load data
data <- read.csv("Corbyn.csv", header=TRUE)

# stemming/stopwords/etc.
customStopWords = c("jeremy", "corbyn")

# stemming/stopwords/etc.
processed <- textProcessor(documents=data$Text, metadata = data, customstopwords = customStopWords, stem = FALSE, striphtml = TRUE)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

#output will have object meta, Abstract, and vocab
Abstract <- out$documents
vocab <- out$vocab
meta <-out$meta

# set the value - this is the hyperparameter that can be changed - much like with k-means clustering
k <- 20

# build the model
result <- stm(Abstract, vocab, K = k, prevalence=~ s(Followers) + s(Friends), init.type = "LDA", data=meta)

#display model results
plot(result)
labelTopics(result, topics = NULL, n = 7, frexweight = 0.5)

# make wordclouds
pdf("topic_cloud.pdf")
for (i in 1:k){
  cloud(result, topic = i, max.words = 50, random.color = TRUE, colors = brewer.pal(6,"Dark2"))
  title(paste("Topic ", i))
}
dev.off() 

#end
