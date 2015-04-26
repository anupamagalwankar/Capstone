library(tm)
setwd("C:/Users/rohitga/Desktop/Coursera_Capstone/final/en_US")

unigram=read.table("unigram2")
bigram=read.table("bigram2")
trigram=read.table("trigram2")



#http://fffff.at/googles-official-list-of-bad-words/

clean_data=function (phrase){
    setwd("C:/Users/rohitga/Desktop/Coursera_Capstone/final/en_US")
    badwords=read.csv("swearWords.csv")
    badwords=data.frame(badwords, stringAsFactors=F)
    badwords=as.character(badwords[,1])
    
    
    corpus<-Corpus(VectorSource(phrase)) 
    corpus=tm_map(corpus, content_transformer(tolower))
    corpus=tm_map(corpus, removeWords, badwords)
    corpus=tm_map(corpus, removePunctuation)
    corpus=tm_map(corpus, removeNumbers)
    corpus=tm_map(corpus, stripWhitespace)
    
    
    
    
    text<-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
    text=as.character(text[,1])
    
    text=unlist(strsplit(text, " "))
    return(text)
}


nextWord=function(phrase){
  
  predictWord=""
  text=clean_data(phrase)
  len=length(text)
  index=integer(0)
  
  if (len>2) {
    
    pattern=paste("^", paste(text[len-2], text[len-1], text[len]), "$", sep="")
    index=grep(pattern, paste(trigram$word1, trigram$word2, trigram$word3) )
    if (length(index)!=0){
      predictWord=as.character(trigram[index,]$nextWord)
    }
  }  
  
  if(len>2 & length(index)==0){
    pattern=paste("^", paste(text[len-1], text[len]), "$", sep="")
    index=grep(pattern, paste(bigram$word1, bigram$word2) )
    if (length(index)!=0){
      predictWord=as.character(bigram[index,]$nextWord)
    }
  }
  
  if(len>2 & length(index)==0){
    pattern=paste("^", text[len], "$", sep="")
    index=grep(pattern, unigram$ngram) 
    if (length(index)!=0){
      predictWord=as.character(unigram[index,]$nextWord)
    }
  }
  
  
  
  
  if (len==2) {
    
    pattern=paste("^", paste(text[len-1], text[len]), "$", sep="")
    index=grep(pattern, paste(bigram$word1, bigram$word2) )
    if (length(index)!=0){
      predictWord=as.character(bigram[index,]$nextWord)
    }
  }
  
  if(len==2 & length(index)==0){
    pattern=paste("^", text[len], "$", sep="")
    index=grep(pattern, unigram$ngram) 
    if (length(index)!=0){
      predictWord=as.character(unigram[index,]$nextWord)
    }
  }
  
  
  
  
  if (len==1) {
    pattern=paste("^", text[len], "$", sep="")
    index=grep(pattern, unigram$ngram) 
    if (length(index)!=0){
      predictWord=as.character(unigram[index,]$nextWord)
    }
  }
  
  
  if (len==1 & length(index)==0){
    index=1
    if (length(index)!=0){
      predictWord=as.character(unigram[index,]$nextWord)
    }
  }
  
  
  print(predictWord)
}


shinyServer(
  function(input, output) {
    
    output$inputValue=renderPrint({input$text})
    output$prediction=renderPrint({nextWord(input$text)})
    
  }
)
