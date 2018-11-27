#install.packages("stringr")
#install.packages("readxl")
#install.packages("KoNLP") 


################ environment #############
setwd("/Users/theorist/Documents/R-WorkingDirectory/DataSentiment")

library(KoNLP) # Korean natural language processor
#dictionary buildup
dics <- c('woorimalsam', 'sejong') #sejong ...
category <- c('emotions')
user_d <- data.frame(readLines("wenoun.txt"), "ncn") # help NLP improve
buildDictionary(ext_dic = dics,
                category_dic_nms = category, 
                user_dic = user_d,
                replace_usr_dic = F,)

##################### data loading ######################
library(readxl)
df <- read_excel("NamYang_Data_Refined.xlsx",col_names=T,na="NA")
df <- df[rowSums(sapply(df[,11], is.na) == 0) > 0, ]
colSums(sapply(df, is.na))
View(df)
df.bkup <- df

################### elimination 수정 필요 ######################
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
  attrs <- str_match(taggedText, "(\"|\\+)([가-힣]{2,4})(/ncps|/ncn|/pvg|/paa)") #상태명사, 비서술명사, 일반동사, 성상형용사 # 단어 길이 2~3 안전
  attrs <- unique(na.omit(attrs[,3]))
  basketline <- paste(attrs, collapse = ",")
  print(text)
  #print(taggedText) #디버깅용
  print(basketline)
  return(basketline)
}

#textToTags("데이터 마이닝 좋다. 텍스트 마이닝 좋다.") #테스트를 하는 곳
####################### basket formatting ###############################
time.start <- Sys.time()
file <- "basket.txt"
if (file.exists(file)) file.remove(file)
for(text in df$content){
  write.table(textToBasket(text), file = "basket.txt", append = T, row.names = F, col.names = F)
}
time.end <- Sys.time()

(basket <- read.table(file = "basket.txt", strip.white = T))

posi <- readLines("neg_pol_words.txt")
test <- unique(unlist(strsplit(PosiNega(posi[1:9826]), " ")))

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

(taggedText <- paste(MorphAnalyzer(df$content[2])))
(taggedWord <- str_match(taggedText, "(\"|\\+)(([가-힣]{2,4}/(ncps|ncn))|([가-힣]{1,3}/(pvg|paa)))"))
(na.omit(taggedWord))



