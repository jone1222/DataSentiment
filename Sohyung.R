library(tm)
naver <- VCorpus(VectorSource(df$content_all))
naver

str(naver[[1]])
content(naver[[1]])
meta(naver[[1]])

# 문장부호 제거
naver <- tm_map(naver, removePunctuation)
naver[[1]]$content
# 숫자 제거
naver = tm_map(naver, removeNumbers)
naver[[1]]$content
# 공백문자 제거
naver = tm_map(naver, stripWhitespace)
naver[[1]]$content

library(KoNLP)
library(NIADic)
useNIADic()
buildDictionary(ext_dic=c('sejong', 'woorimalsam', 'insighter'))

# 명사 추출
# extractNoun(naver[[1]]$content)
for(i in seq_along(naver)){
  nouns <- extractNoun(naver[[i]]$content)
  nouns <- nouns[nchar(nouns) > 2]
  naver[[i]]$content <- paste(nouns, collapse=" ")
}
naver[[1]]$content

naver_tdm <- TermDocumentMatrix(naver, control=list(tokenize="scan", wordLengths=c(2, 7)))
inspect(naver_tdm)
nTerms(naver_tdm)
# 3142
nDocs(naver_tdm)
# 250

# 빈번 단어의 추출
findFreqTerms(naver_tdm, lowfreq = 5, highfreq = Inf)

# 단어 간 연관성
findAssocs(naver_tdm, c("고구마라떼", "맛있는우유"), c(0.75))

# 희소단어의 제거
naver_tdm <- removeSparseTerms(naver_tdm, sparse=0.95)

# 단어 빈도계산
wordFreq <- slam::row_sums(naver_tdm)
wordFreq <- sort(wordFreq, decreasing=TRUE)
library(wordcloud)
pal <- brewer.pal(8,"Dark2")
w <- names(wordFreq)
wordcloud(words=w, freq=wordFreq,
          min.freq=3, random.order=F,
          random.color=T, colors=pal)
rm.idx <- grep("[남양유업|인터넷]", names(wordFreq))
wordFreq1 <- wordFreq[-rm.idx]
stopwords <- c("^ㅎ^ㅎ", "^ㅋ^ㅋ^ㅋ", "^ㅎ^ㅎ^ㅎ")
wordFreq1 <- wordFreq1[(!(names(wordFreq1) %in% stopwords))]
w1 <- names(wordFreq1)
wordcloud(words=w1, freq=wordFreq1,
          min.freq=2, random.order=F, random.color=T, colors=pal)

# 키워드 클러스터
tds <- naver_tdm[Terms(naver_tdm) %in% w1,]
m2 <- as.matrix(tds)
# colnames(m2) <- gsub(".txt", "", colnames(m2))
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit, xlab="", sub="", main="clustering keywords")
rect.hclust(fit, k = 13)

# 문서 클러스터
tds <- naver_tdm[Terms(naver_tdm) %in% w1,]
m2 <- as.matrix(tds)
tm2 <- t(m2)
distMatrix <- dist(scale(tm2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit, xlab="", sub="", main="clustering documents")
rect.hclust(fit, k = 12)
clusterCut <- cutree(fit, 12)
table(clusterCut, rownames(tm2))
df$cluster <- clusterCut

######

p <- function(c){
  naver <- VCorpus(VectorSource(df$content_all[df$cluster==c]))
  
  # 문장부호 제거
  naver <- tm_map(naver, removePunctuation)
  # 숫자 제거
  naver = tm_map(naver, removeNumbers)
  # 공백문자 제거
  naver = tm_map(naver, stripWhitespace)
  
  # 명사 추출
  # extractNoun(naver[[1]]$content)
  for(i in seq_along(naver)){
    nouns <- extractNoun(naver[[i]]$content)
    nouns <- nouns[nchar(nouns) > 2]
    naver[[i]]$content <- paste(nouns, collapse=" ")
  }
  
  naver_tdm <- TermDocumentMatrix(naver, control=list(tokenize="scan", wordLengths=c(2, 7)))
  
  # 희소단어의 제거
  # naver_tdm <- removeSparseTerms(naver_tdm, sparse=0.95)
  
  # 단어 빈도계산
  wordFreq <- slam::row_sums(naver_tdm)
  wordFreq <- sort(wordFreq, decreasing=TRUE)
  pal <- brewer.pal(8,"Dark2")
  w <- names(wordFreq)
  rm.idx <- grep("[남양유업|인터넷]", names(wordFreq))
  wordFreq1 <- wordFreq[-rm.idx]
  stopwords <- c("^ㅎ^ㅎ", "^ㅋ^ㅋ^ㅋ", "^ㅎ^ㅎ^ㅎ")
  wordFreq1 <- wordFreq1[(!(names(wordFreq1) %in% stopwords))]
  w1 <- names(wordFreq1)
  # wordcloud(words=w1, freq=wordFreq1,
  #           min.freq=2, random.order=F, random.color=T, colors=pal)
  print(wordFreq1[1])
}

r <- function(c){
  naver <- VCorpus(VectorSource(df$content_all[df$cluster==c]))
  
  # 문장부호 제거
  naver <- tm_map(naver, removePunctuation)
  # 숫자 제거
  naver = tm_map(naver, removeNumbers)
  # 공백문자 제거
  naver = tm_map(naver, stripWhitespace)
  
  # 명사 추출
  # extractNoun(naver[[1]]$content)
  for(i in seq_along(naver)){
    nouns <- extractNoun(naver[[i]]$content)
    nouns <- nouns[nchar(nouns) > 2]
    naver[[i]]$content <- paste(nouns, collapse=" ")
  }
  
  naver_tdm <- TermDocumentMatrix(naver, control=list(tokenize="scan", wordLengths=c(2, 7)))
  
  # 희소단어의 제거
  # naver_tdm <- removeSparseTerms(naver_tdm, sparse=0.95)
  
  # 단어 빈도계산
  wordFreq <- slam::row_sums(naver_tdm)
  wordFreq <- sort(wordFreq, decreasing=TRUE)
  pal <- brewer.pal(8,"Dark2")
  w <- names(wordFreq)
  rm.idx <- grep("[남양유업|인터넷|아이들|스티커|영수증]", names(wordFreq))
  wordFreq1 <- wordFreq[-rm.idx]
  stopwords <- c("^ㅎ^ㅎ", "^ㅋ^ㅋ^ㅋ", "^ㅎ^ㅎ^ㅎ")
  wordFreq1 <- wordFreq1[(!(names(wordFreq1) %in% stopwords))]
  w1 <- names(wordFreq1)
  # wordcloud(words=w1, freq=wordFreq1,
  #           min.freq=2, random.order=F, random.color=T, colors=pal)
  print(wordFreq1[1])o
}

for(i in 1:6){
  print(i)
  p(i)
}
for(i in 7:12){
  print(i)
  r(i)
}

for(i in 1:12){
  print(i)
  print(prop.table(table(df$class[df$cluster==i])))
}