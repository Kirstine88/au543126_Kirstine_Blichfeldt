library(pdftools)
library(tm)
library(widyr)
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(ggraph)
library(igraph)
library(dplyr)
#install.packages("tidytext")
#install.packages("ggraph")
#install.packages("igraph")
#install.packages("widyr")
# install tesseract to use the built in OCR in pdf tools
#install.packages("tesseract")
getwd()#Set working directory to the folder where the PDF were downloaded
setwd("~/Desktop/Digital_history_exam_2020/LOTR_TEXTS/LOTR_PDF")
files <- list.files(pattern = "pdf$")
#creating an list object "LOTR" 
LOTR <- lapply(files, pdf_text)
length(LOTR) # shows 3 elements - each is a vector containing the text of the pdf
#text mining = tm
#install.packages("tm")

#Creating a corpus + object "corp"
corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF)) 
#creating term-document matrix (TDM)
LOTR_tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf))))
inspect(LOTR_tdm[1:10,]) 
#removing punctuation
corp <- tm_map(corp, removePunctuation, ucp = TRUE)
#re-creating TDM
LOTR_tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(stopwords = TRUE,
                                          tolower = TRUE,
                                          stemming = TRUE,
                                          removeNumbers = TRUE,
                                          bounds = list(global = c(3, Inf)))) 
inspect(LOTR_tdm[1:30,])
#find words that occur at least 100 times
findFreqTerms(LOTR_tdm, lowfreq = 100, highfreq = Inf)
#see counts of the words listed above - subsetting the TDM
ft <- findFreqTerms(LOTR_tdm, lowfreq = 100, highfreq = Inf)
#as.matrix(LOTR_tdm[ft,]) 
#to see total counts for the words
ft_tdm <- as.matrix(LOTR_tdm[ft,])
sort(apply(ft_tdm, 1, sum), decreasing = TRUE)
#find words in association with "tree"
findAssocs(LOTR_tdm,"tree",corlimit = 0.90)
#find word in association with "forest"
findAssocs(LOTR_tdm, "forest",corlimit = 0.90)
#wordcloud
LOTR_tdm
m<-as.matrix(LOTR_tdm)
v<-sort(rowSums(m),decreasing = TRUE)
d<-data.frame(word = names(v),freq = v)
wordcloud(d$word, d$freq, random.order = FALSE, rot.per = 0.3, scale =c(4,.5), max.words = 100, title.main = "Wordcloud -LOTR",font.main = 1,cex.main = 1.5, colors = brewer.pal(8,"Dark2"))
#comparison wordcloud
LOTR_doc <-as.matrix(LOTR_tdm)
#column names
colnames(LOTR_doc)
#renaming columns
colnames(LOTR_doc) <- c("part1", "part3","part2")
comparison.cloud(LOTR_doc, max.words = 100, random.order = FALSE, title.size = 2,title.colors = NULL, match.colors = FALSE, colors = brewer.pal(8,"Dark2"))
#visualization of words associated with "tree" and "forest"
findAssocs(LOTR_tdm,c("tree","forest"),corlimit = 1.00)
term1 <- "tree" #term of interest
term2 <- "forest"
corlimit <- 1.00 #lower correlation bound limit
corr1 <-  findAssocs(LOTR_tdm, term1, corlimit)[[1]]
corr1 <- cbind(read.table(text = names(corr1), stringsAsFactors = FALSE), corr1)
corr2 <- findAssocs(LOTR_tdm, term2, corlimit)[[1]]
corr2 <- cbind(read.table(text = names(corr2), stringsAsFactors = FALSE), corr2)
#join them together with library(dplyr)
two_terms_corrs <- full_join(corr1, corr2)
# gather for plotting with library(tidyr)
two_terms_corrs_gathered <- gather(two_terms_corrs, term, correlation, corr1:corr2)
# insert the actual terms of interest so that they show up on the legend
two_terms_corrs_gathered$term <- ifelse(two_terms_corrs_gathered$term  == "corr1", term1, term2)
# Draw the plot with library(ggplot2)
ggplot(two_terms_corrs_gathered, aes(x = V1, y = correlation, colour =  term ) ) +
  geom_point(size = 3) +
  ylab(paste0("Correlation with the terms ", "\"", term1,  "\"", " and ",  "\"", term2, "\"")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))                                       
                                        
       




