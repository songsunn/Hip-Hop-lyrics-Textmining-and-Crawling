setwd("C:/rdata")

# 2019년 힙합장르 순위 100 노래 가사 분석

library(tidyverse) 
library(KoNLP) 
library(reshape) 

# 감성분석에 필요한 패키지

library(plyr)
library(stringr)

# 사회관계망 시각화 패키지

library(qgraph)
library(tm)

# 연관규칙분석 패키지
library(arules)

# 토픽모델링 패키지
library(lda) 
library(topicmodels) 

k################################################################################################################

# 2019 빈도분석
doc_2019 <- readLines('2019.txt', encoding="UTF-8") 
mergeUserDic(data.frame(c('Flex','멘탈','발렌시아','구찌','샤넬','Gucci','Chanel','ATM','롤랙스','Moncler','bentley'),c('ncn'))) 
result1 <- extractNoun(doc_2019) 
filter1 = function(x){  nchar(x) >= 2  } 
filter2 = function(x){   Filter(filter1, x) } 
result2 = sapply(result1, filter2) 
result3 = unlist(result2) 
result4 = table(result3) 
result5 = head(sort(result4, decreasing=TRUE), 100) 
result5

################################################################################################################

# 2019 감성분석

emo_2019<-readLines('2019.txt', encoding="UTF-8") 

# 군산대 감성사전 사용
# 이런 문장에서 재미있다는 긍정어 하나와 별로라는 부정어 하나가 있는데 1(재미) - 1(별로) = 0이 나와 중립이 되는 방식

posDic <- readLines("positive.txt", encoding = "UTF-8")

negDic <- readLines("negative.txt", encoding = "UTF-8")

# 긍, 부정어 비교하는 함수

sentimental = function(sentences, posDic, negDic)
{scores = laply(sentences, function(sentence, posDic, negDic)
{sentence = gsub('[[:punct:]]', '', sentence) 
sentence = gsub('[[:cntrl:]]', '', sentence)
sentence = gsub('\\d+', '', sentence)
sentence = tolower(sentence)
word.list = str_split(sentence, '\\s+')
words = unlist(word.list)
pos.matches = match(words, posDic)
neg.matches = match(words, negDic)
pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)         
score = sum(pos.matches) - sum(neg.matches)          
return(score)}, posDic, negDic)
scores.df = data.frame(score=scores, text=sentences)  
return(scores.df) } 


# 2019 힙합가사에 함수 적용

result=sentimental(emo_2019,posDic,negDic) 
result$score 


# 감성분석한 것 시각화
result$color[result$score >=1] = "blue" 
result$color[result$score ==0] = "green" 
result$color[result$score < 0] = "red" 
table(result$color) 

result$remark[result$score >=1] = "긍정" 
result$remark[result$score ==0] = "중립" 
result$remark[result$score < 0] = "부정" 
sentiment_result= table(result$remark) 
sentiment_result 

pie(sentiment_result, main="감성분석결과", col=c("blue","red","green"), radius=0.8) 

###############################################################################################################

################################################################################################################

doc_2000 <- readLines('2000.txt',encoding="UTF-8") 
result6 <- extractNoun(doc_2000) 
filter6 = function(x){  nchar(x) >= 2  } 
filter7 = function(x){   Filter(filter6, x) } 
result7 = sapply(result6, filter7) 
result8 = unlist(result7) 
result9 = table(result8) 
result10 = head(sort(result9, decreasing=TRUE), 200) 
result10 

################################################################################################################

# 감성분석

emo_2000<-readLines('2000.txt', encoding="UTF-8") 

# 군산대 감성사전 사용
# 이런 문장에서 재미있다는 긍정어 하나와 별로라는 부정어 하나가 있는데 1(재미) - 1(별로) = 0이 나와 중립이 되는 방식

posDic <- readLines("positive.txt", encoding = "UTF-8")

negDic <- readLines("negative.txt", encoding = "UTF-8")

# 긍, 부정어 비교하는 함수

sentimental = function(sentences, posDic, negDic)
{scores = laply(sentences, function(sentence, posDic, negDic)
{sentence = gsub('[[:punct:]]', '', sentence) 
sentence = gsub('[[:cntrl:]]', '', sentence)
sentence = gsub('\\d+', '', sentence)
sentence = tolower(sentence)
word.list = str_split(sentence, '\\s+')
words = unlist(word.list)
pos.matches = match(words, posDic)
neg.matches = match(words, negDic)
pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)         
score = sum(pos.matches) - sum(neg.matches)          
return(score)}, posDic, negDic)
scores.df = data.frame(score=scores, text=sentences)  
return(scores.df) } 

# 2000 힙합가사에 함수 적용

result=sentimental(emo_2000,posDic,negDic) 
result$score 


# 감성분석한 것 시각화
result$color[result$score >=1] = "blue" 
result$color[result$score ==0] = "green" 
result$color[result$score < 0] = "red" 
table(result$color) 

result$remark[result$score >=1] = "긍정" 
result$remark[result$score ==0] = "중립" 
result$remark[result$score < 0] = "부정" 
sentiment_result= table(result$remark) 
sentiment_result 

pie(sentiment_result, main="감성분석결과", col=c("blue","red","green"), radius=0.8) 

###############################################################################################################

doc_2010 <- readLines('2010.txt',encoding="UTF-8") 
result11 <- extractNoun(doc_2010) 
filter11 = function(x){  nchar(x) >= 2  } 
filter12 = function(x){   Filter(filter11, x) } 
result12 = sapply(result11, filter12) 
result13 = unlist(result12) 
result14 = table(result13) 
result15 = head(sort(result14, decreasing=TRUE), 200) 
result15 

################################################################################################################

# 감성분석

emo_2010<-readLines('2010.txt', encoding="UTF-8") 

# 군산대 감성사전 사용
# 이런 문장에서 재미있다는 긍정어 하나와 별로라는 부정어 하나가 있는데 1(재미) - 1(별로) = 0이 나와 중립이 되는 방식

posDic <- readLines("positive.txt", encoding = "UTF-8")

negDic <- readLines("negative.txt", encoding = "UTF-8")

# 긍, 부정어 비교하는 함수

sentimental = function(sentences, posDic, negDic)
{scores = laply(sentences, function(sentence, posDic, negDic)
{sentence = gsub('[[:punct:]]', '', sentence) 
sentence = gsub('[[:cntrl:]]', '', sentence)
sentence = gsub('\\d+', '', sentence)
sentence = tolower(sentence)
word.list = str_split(sentence, '\\s+')
words = unlist(word.list)
pos.matches = match(words, posDic)
neg.matches = match(words, negDic)
pos.matches = !is.na(pos.matches)
neg.matches = !is.na(neg.matches)         
score = sum(pos.matches) - sum(neg.matches)          
return(score)}, posDic, negDic)
scores.df = data.frame(score=scores, text=sentences)  
return(scores.df) } 

# 2010 힙합가사에 함수 적용

result=sentimental(emo_2010,posDic,negDic) 
result$score 


# 감성분석한 것 시각화
result$color[result$score >=1] = "blue" 
result$color[result$score ==0] = "green" 
result$color[result$score < 0] = "red" 
table(result$color) 

result$remark[result$score >=1] = "긍정" 
result$remark[result$score ==0] = "중립" 
result$remark[result$score < 0] = "부정" 
sentiment_result= table(result$remark) 
sentiment_result 

pie(sentiment_result, main="감성분석결과", col=c("blue","red","green"), radius=0.8) 

###############################################################################################################

# 인스타그램 크롤링 데이터 텍스트마이닝 

html_rm <- c("class", "alt", "h1", "img", "div", "sizes",
             
             "srcset", "span", "header", "jpg", "style",
             
             "css", "com", "http", "tagged", "javascript", "src", "script")
flex<- readLines('flex.txt', encoding="UTF-8") 
flex01 <- str_replace_all(flex, "[<>,.?/=\"\':;*&^%$@!~]", " ")
flex02 <- str_replace_all(flex01, html_rm, " ")

result21 <- extractNoun(flex02) 
filter21 = function(x){  nchar(x) >= 2  } 
filter22 = function(x){   Filter(filter21, x) } 
result22 = sapply(result21, filter22) 
result23 = unlist(result22) 
result24 = table(result23) 
result25 = head(sort(result24, decreasing=TRUE), 100) 
result25

###############################################################################################################

# flex 인스타그램 사회관계망 분석

html_rm <- c("class", "alt", "h1", "img", "div", "sizes",
             
             "srcset", "span", "header", "jpg", "style",
             
             "css", "com", "http", "tagged", "javascript", "src", "script")
flex<- readLines('flex.txt', encoding="UTF-8") 
flex01 <- str_replace_all(flex, "[<>,.?/=\"\':;*&^%$@!~]", " ")
flex02 <- str_replace_all(flex01, html_rm, " ")

result21 <- extractNoun(flex02) 
cps <- VCorpus(VectorSource(result21)) 
doc <- TermDocumentMatrix(cps, control=list(tokenize=words, wordLengths=c(2, 8)))  
tdm.matrix <- as.matrix(doc) 

tdm.matrix
word.count <- rowSums(tdm.matrix) 
word.order <- order(word.count, decreasing=TRUE) 
tdm.matrix <- tdm.matrix[word.order[1:30], ] 
tdm.matrix <- tdm.matrix %*% t(tdm.matrix) 
tdm.matrix 

qgraph(tdm.matrix, labels=rownames(tdm.matrix),diag =F, layout='spring') 


###############################################################################################################
# 인스타그램 토픽 모델링

library(KoNLP)
library(lda)
library(topicmodels)
library(tm)
html_rm <- c("class", "alt", "h1", "img", "div", "sizes",
             
             "srcset", "span", "header", "jpg", "style",
             
             "css", "com", "http", "tagged", "javascript", "src", "script")
flex<- readLines('flex.txt', encoding="UTF-8")  
flex01 <- str_replace_all(flex, "[<>,.?/=\"\':;*&^%$@!~]", " ")
flex02 <- str_replace_all(flex01, html_rm, " ")

result21 <- extractNoun(flex02) 
gsub(" "," ",result21) 

result18 <- extractNoun(flex02) 

cps18 <- VCorpus(VectorSource(result18)) 

cps18 <- tm_map(cps18, stripWhitespace) 

cps18 <- tm_map(cps18, removeNumbers) 

cps18 <- tm_map(cps18, removePunctuation) 

cps18 <- tm_map(cps18, content_transformer(tolower)) 

cps18 <- tm_map(cps18, removeWords, stopwords("en")) 

doc18 <- TermDocumentMatrix(cps18, control=list(tokenize=words,wordLengths=c(2, 8)))  
dtm <- as.DocumentTermMatrix(doc18) 
rowtotal = apply(dtm,1,sum) 
dtm.new = dtm[rowtotal > 0,] 
lda <- LDA(dtm.new, k=8,control=list(seed=123456))  

term <- terms(lda, 20)  
term 
??? 
