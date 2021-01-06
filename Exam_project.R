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
library(stringr)
library(textdata)
library(tokenizers)
#install.packages("tokenizers")
#install.packages("stringr")
#install.packages("textdata")
#install.packages("tidytext")
#install.packages("ggraph") 
#install.packages("igraph")
#install.packages("widyr")
#install.packages("tm")
# install tesseract to use the built in OCR in pdf tools
#install.packages("tesseract")
getwd()#Set working directory to the folder where the PDF were downloaded
setwd("~/Desktop/Digital_history_exam_2020/LOTR_TEXTS/LOTR_PDF")
files <- list.files(pattern = "pdf$")
#creating a list object "LOTR" 
LOTR <- lapply(files, pdf_text)
length(LOTR) # shows 3 elements - each is a vector containing the text of the pdf
#text mining = tm

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
                                          wordLengths = c(4,11),
                                          bounds = list(global = c(3, Inf))))
                                        
                                        
                              
                                          
LOTR_tdm
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
                                          wordLengths = c(4,11),
                                       bounds = list(global = c(3, Inf))))
LOTR_tdm
#find words that occur at least 100 times
findFreqTerms(LOTR_tdm, lowfreq = 100, highfreq = Inf)
#see counts of the words listed above - subsetting the TDM
ft <- findFreqTerms(LOTR_tdm, lowfreq = 100, highfreq = Inf)
#as.matrix(LOTR_tdm[ft,]) 
#to see total counts for the words
ft_tdm <- as.matrix(LOTR_tdm[ft,])
sort(apply(ft_tdm, 1, sum), decreasing = TRUE)
#find words in association with "Fangorn"
findAssocs(LOTR_tdm,"fangorn", corlimit = 0.90)
findAssocs(LOTR_tdm, "mirkwood",corlimit = 0.90)
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
comparison.cloud(LOTR_doc, max.words = 100, random.order = FALSE, title.size = 2,title.colors = NULL,font.main = 1, cex.main = 1.5, match.colors = FALSE, colors = brewer.pal(8,"Dark2"))

#counting 15 most frequent words in the 3 pdfs using tidy text
LOTR_tidy<-tidy(LOTR_tdm)
LOTR_tidy %>% 
  arrange(desc(count)) %>% 
  group_by(document) %>% 
  top_n(25) %>% 
  ungroup() %>% 
  mutate(term = reorder(term, count)) %>% 
  ggplot(aes(x = term, y = count)) + 
  geom_col()+
  facet_wrap(~document)+
  coord_flip()

#Analysing part2 "The_two_towers"
part2_tdm <-LOTR_tdm[,c("The_two_towers.pdf")] # creating a new object containing only part 2






  

 







  
 


 


   



