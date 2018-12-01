#install.packages("stringr")
#install.packages("readxl")
#install.packages("KoNLP") 
#install.packages("arules")

################ environment #############
setwd("C:/Users/ok3651004/Desktop/DataSentiment")

library(KoNLP) # Korean natural language processor
#dictionary buildup
dics <- c('woorimalsam', 'sejong') #sejong ...
category <- c('emotions')
user_d <- data.frame(readLines("wenoun.txt"), "ncn") # help NLP improve
buildDictionary(ext_dic = dics,
                category_dic_nms = category, 
                user_dic = user_d,
                replace_usr_dic = F,)

##################### rawdata loading ######################
library(readxl)
df <- read_excel("NamYang_Data_Refined.xlsx",col_names=T,na="NA")
df <- df[rowSums(sapply(df[,11], is.na) == 0) > 0, ]
colSums(sapply(df, is.na))
df.bkup <- df

##################### sentiment dictionary loading ######################
# library(devtools)
# install_github("plgrmr/readAny", force = T)
# library(readAny)
sf <- read_excel("SentiWord_Dict_excel.xlsx",col_names=T,na="NA")
colSums(sapply(sf, is.na))
sf.bkup <- sf

################### elimination 수정 필요 ######################
library(stringr)
str_replace_all(df$content, "[ㄱ-ㅎ]", "") %>%  #remove ㅋㅋㅋ
  str_replace_all("[0-9]", "") %>%              #remove number
  str_replace_all("[[:punct:]]", "")            #remove punctuation


# test <- "ㅋㅋㅋ ㅎㅎㅎ 1월 2일 ^^ ;; @@ :)"
# str_replace_all(test, "[ㄱ-ㅎ]", "") %>%  #remove ㅋㅋㅋ
#   str_replace_all("[0-9]", "") %>%              #remove number
#   str_replace_all("[[:punct:]]", "")            #remove punctuation
###################### pos tagging ##########################
textToBasket <- function(text){
  taggedText <- paste(MorphAnalyzer(text))
  #attrs <- str_match(taggedText, "(\"|\\+)([가-힣]{1,4})(/ncps|/ncn|/pvg|/paa)") 
  attrs <- str_match(taggedText, "(\"|\\+)((([가-힣]{2,4})/(ncps|ncn))|(([가-힣]{2,4})/(pvg|paa)))") #상태명사, 비서술명사 (2,4) 일반동사, 성상형용사 (1,3)
  attrs <- unique(na.omit(c(attrs[,5], attrs[,8])))
  basketline <- paste(attrs, collapse = ",")
  #print(basketline) #print(text) #print(taggedText)  #디버깅용
  return(basketline)
}

#textToTags("데이터 마이닝 좋다. 텍스트 마이닝 좋다.") #테스트를 하는 곳
####################### basket formatting ###############################
file <- "basket.txt"
if (file.exists(file)) file.remove(file)
#멈추지 않는 텍스트: 2316, 4071
#warning : 1512, 1522
#tag가 null 텍스트 다수 --> (4568 - 2) - 4485 = 81 개
for(i in c(1:1511, 1513:1521, 1523:2315, 2317:4070, 4072:4568)){
  #for(i in c(1:100)){
  print(i)
  write.table(textToBasket(df$content[i]), file = "basket.txt", append = T, row.names = F, col.names = F, quote = F)
  i <- i + 1
}

(basket <- read.table(file = "basket.txt", strip.white = T)) #test reading into data frame
#######################  ###############################
sf.test <- data.frame()
for(i in 1:nrow(df)){
  print(i)
  df$sentiment[i] <- sum(sf$score[which(sf$text %in% unlist(str_split(df$content[i], " ")))])
}

senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content[29], " ")))]
senti_rate[2]

for(i in 1:nrow(df)){
  senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content[i], " ")))]
  if(is.na(senti_rate[1])){
    next
  } else{
    for(j in 1:length(senti_rate)){
      if(senti_rate[j] == 1){
        df$good1[i] <- df$good1[i]+1
      } else if(senti_rate[j] == 2){
        df$good2[i] <- df$good2[i]+1
      } else if(senti_rate[j] == -1){
        df$bad1[i] <- df$bad1[i]+1
      } else if(senti_rate[j] == -2){
        df$bad2[i] <- df$bad2[i]+1
      } else if(senti_rate[j] == 0){
        df$normal[i] <- df$normal[i]+1
      }
    }
  }
}

for(i in 1:nrow(df)){
  senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content[i], " ")))]
  rate_sum <- sum(senti_rate)
  if(rate_sum == 0){
    df$sentiment_mean[i] <- 0
  } else{
    df$sentiment_mean[i] <- round(sum(senti_rate) / length(senti_rate), digits = 2)  
  }
  
}

for(i in 1:nrow(df)){
  df$sentiment_mean[i] <- 0
}

for(i in 1:nrow(df)){
  df$bad2[i] <- 0
  df$bad1[i] <- 0
  df$normal[i] <- 0
  df$good1[i] <- 0
  df$good2[i] <- 0
}

sum(sf$score[which(sf$text %in% unlist(str_split(df$content[1], " ")))])

aa <- sf$score[which(sf$text %in% unlist(str_split(df$content[29], " ")))]
aa
aa[1]
aa[2]
length(aa)

for(i in 1:nrow(raw_datas)){
  print(i)
  raw_datas[i]
}

######################## reading transaction ##########################
library(arules)
tr.data <- read.transactions(file = "basket.txt", format = "basket", sep = ',')
######################## exploration on tr.dataansaction ##########################
summary(tr.data)
image(tr.data)
itemFrequencyPlot(tr.data, support = 0.1)
itemLabels(tr.data)
######################## association rules mining ##########################
elim.word <- c("개인거래", "판매", "안전", "미사용", "완료", "분유", "양유") #관심 없는 단어
rules <- apriori(tr.data,
                 parameter = list(minlen=2,supp=0.007, conf=0.2),
                 appearance = list(none = elim.word, default = "both")
)
rules <- sort(rules, decreasing = TRUE, by = "support")
inspect(rules)
######################## new negative keywords ##########################
nega.word <- c("논란", "이물질") #부정어 등록하는 벡터
(nega.new <- subset(rules, lhs %in% nega.word))
rules <- sort(nega.new, decreasing = TRUE, by = "confidence")
inspect(nega.new)
######################## new negative keywords ##########################
nega.word <- c("논란", "이물질", "혼입") #부정어 등록하는 벡터
(nega.new <- subset(rules, lhs %in% nega.word))
rules <- sort(nega.new, decreasing = TRUE, by = "confidence")
inspect(nega.new) 
######################## new negative keywords ##########################
nega.word <- c("논란", "이물질", "혼입", "코딱지", "이정", "임페리얼") #부정어 등록하는 벡터
(nega.new <- subset(rules, lhs %in% nega.word))
rules <- sort(nega.new, decreasing = TRUE, by = "confidence")
inspect(nega.new) 
######################## new negative keywords ##########################
nega.word <- c("논란", "이물질", "혼입", "코딱지", "이정", "임페리얼", "보내기") #부정어 등록하는 벡터
(nega.new <- subset(rules, lhs %in% nega.word))
rules <- sort(nega.new, decreasing = TRUE, by = "confidence")
inspect(nega.new) 
###################### pos tagging 연습 ##########################
library(stringr)
#(tagged <- paste(SimplePos22(df$content[1])))
(tagged <- paste(MorphAnalyzer(df$content[1]))) #1:100
##unique(str_match(tagged, "([가-힣]+)/ncp")[,2]) #서술성 명사
##unique(str_match(tagged, "([가-힣]+)/ncpa")[,2]) #동작성 명사 .. 주말여행, 매매, 저축, 운전, 수원, 감수, 구도, 경주, 상승, 적용, 소방, 양보, 한잔, 출산
(noun <- str_match(tagged, "([가-힣]+)/ncps")[,2]) #상태성 명사 .. 특별, 따듯, 지난, 유한, 상이, 불구, 자유, 사사
(noun <- str_match(tagged, "([가-힣]+)/ncn")[,2]) #비서술성 명사 .. 중학생, 현대아파트, 충남, 인간, 농촌
##unique(str_match(tagged, "([가-힣]+)/ncr")[,2]) #직위 명사 .. 수상, 경찰관, 고문, 임원, 신부, 교장, 기자
##unique(str_match(tagged, "([가-힣]+)/pvd")[,2]) #지시 동사 .. NA
(verb <- str_match(tagged, "([가-힣]+)/pvg")[,2]) #일반 동사 .. 훔치, 즐기, 비오, 먹, 막히, 건너, 소문나, 입, 오르
##unique(str_match(tagged, "([가-힣]+)/pad")[,2]) #지시형용사 .. 있다, 기다
(adjective <- str_match(tagged, "([가-힣]+)/paa")[,2]) #성상형용사 .. 좋, 잘, 달, 잘생기, 아름답, 새롭, 춥, 쌀쌀하, 드넓, 가볍
#unique(str_match(tagged, "([가-힣]+)/px")[,2]) #보조용언 .. 내, 받, 않, 말, 줄, 두, 대, 들

###################### 정규식 연습 ##########################
str_match(c("12a", "123b", "1234a", "12345b"), "^([0-9]{2,3})(a|b)$")
str_match(c("가나다a", "가나다라b", "가나다/paa", "가나/pvg"), "([가-힣]{2,3})(/paa|b|/pvg)")
str_match(tagged, "([가-힣]{1,3})(/ncps|/pvg|/paa)")

(taggedText <- paste(MorphAnalyzer(df$content[1700])))
(taggedWord <- str_match(taggedText, "(\"|\\+)((([가-힣]{2,4})/(ncps))|(([가-힣]{1,3})/(pvg)))"))
unique(na.omit(taggedWord[,c(8)]))
unique(na.omit(c(taggedWord[,5], taggedWord[,8])))
na.omit(taggedWord[5])
na.omit(taggedWord[,5])
taggedWord(attrs[,8])
taggedWord(attrs[,8])
taggedWord[,c(5, 8)]