# install needed packages
# Needed <- c("tm", "SnowballCC", "RColorBrewer", "wordcloud", "biclust", 
#            "cluster", "igraph", "fpc")
# install.packages(Needed, dependencies = TRUE)
# install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(xlsx)

setwd("D:/RProjects/EG2019_word_cloud")

party <- "DA" # Introduce political party folder
party_color <- switch(party, "DA" = "#e9512a", "LA" = "#1eb1ec", "PS" = "#bf1322", "SDP" = "#a81853", "TV" = "#2e2a61", "UPA" = "#16428a", "all" = "#d3d3d3")

# Particular words to avoid
avoid_words <- c("eleccions", "programa", "benvolguda", "benvolgut", "del", "dels", "pàg", "pel", "part", "però","això", "pera", "manera",
  "als", "crearem", "abril", "dotarem", "establirem","tel", "març", "deis", "perque", "demòcrates", "peral", "liberals",
  "sdp", "puguin", "volem", "tenir", "fer", "andorra", "manera")

# Load files in texts folder
cname <- sprintf("D:/RProjects/EG2019_word_cloud/input/%s", party)
dir(cname) #chech folder cname

# Load texts into R with tm package
docs <- VCorpus(DirSource(cname)) # Best performance with .txt files. (export .pdf to accessible .txt)
tdm0 <- TermDocumentMatrix(docs)
total_words <- tdm0$nrow

# View loaded docs
summary(docs)
#check doc [1]
#inspect(docs[1])
#writeLines(as.character(docs[1]))

# Remove punctuation, numbers
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)

# Convert to lowercase
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, PlainTextDocument)

# Removing "stopwords" (common words) that usually have no analytic value
docs <- tm_map(docs, removeWords, stopwords("catalan"))  
docs <- tm_map(docs, PlainTextDocument)

# Remove particular words without analytical interest
docs <- tm_map(docs, removeWords, avoid_words)

# Tells R to treat your preprocessed documents as text documents
docs <- tm_map(docs, PlainTextDocument)

# Remove particular characters
for(j in seq(docs)){
  docs[[j]] <- gsub("'"," ", docs[[j]])
  docs[[j]] <- gsub("'"," ", docs[[j]]) # Include manually "'" copying from writeLines!!! 
}

docs <- tm_map(docs, stripWhitespace) # Delete whitespaces

# Combine words
for (j in seq(docs)){
  docs[[j]] <- gsub("taxa turística", "taxa_turística", docs[[j]])
  docs[[j]] <- gsub("habitatge públic", "habitatge_públic", docs[[j]])
  docs[[j]] <- gsub("llei electoral", "llei_electoral", docs[[j]])
  docs[[j]] <- gsub("sistema electoral", "sistema_electoral", docs[[j]])
  docs[[j]] <- gsub("sant julià", "sant_julià", docs[[j]])
  docs[[j]] <- gsub("unió europea", "unió_europea", docs[[j]])
  docs[[j]] <- gsub("desenvolupament sostenible", "desenvolupament_sostenible", docs[[j]])
  docs[[j]] <- gsub("cohesió social", "cohesió_social", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument) # tell R to treat your preprocessed documents as text documents
# This is the end of the preprocessing stage

# Create term document matrix
tdm <- TermDocumentMatrix(docs)
tdm

# Convert to matrix
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)

# Convert to dataframe
freq <- data.frame(word = names(v), freq = v)
head(freq, 20)
freq$freq_norm <- freq$freq / total_words
write.xlsx(freq, file=sprintf("output/freq_%s.xlsx", party))

# Plot bar chart (first X)
x <- if (party == "all") 5 else 25
freqx <- head(freq, x)
bar_plot <- ggplot(freqx, aes(x = reorder(word, freq), y = freq)) + geom_col(color = "white", fill = party_color) + coord_flip() + theme_minimal() +
  theme(
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.position="none"
  ) +
  xlab("") +
  ylab("Nombre de repeticions")
bar_plot   

# Plot wordcloud
set.seed(1234) # set the seed random number generator 
wordcloud(words = freq$word, freq = freq$freq_norm, scale=c(3,.05), min.freq = 5,
          max.words=25, random.order=FALSE, rot.per=0.15, 
          colors=party_color)