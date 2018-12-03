#install.packages("stringr")
#install.packages("readxl")
#install.packages("KoNLP") 
#install.packages("arules")

################ environment #############
# setwd("C:/Users/ok3651004/Desktop/DataSentiment")

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
df <- read_excel("NamYang_Data_Added_Total.xlsx",col_names=T,na="NA")
# df <- df[rowSums(sapply(df[,11], is.na) == 0) > 0, ]
# colSums(sapply(df, is.na))
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
  df$sentiment_mean[i] <- 0
  df$bad2[i] <- 0
  df$bad1[i] <- 0
  df$normal[i] <- 0
  df$good1[i] <- 0
  df$good2[i] <- 0
  df$good_total[i] <- 0
  df$bad_total[i] <- 0
}

for(i in 1:nrow(df)){
  print(i)
  df$sentiment[i] <- sum(sf$score[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))])
}

senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content[29], " ")))]
senti_rate[2]

for(i in 1:nrow(df)){
  senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))]
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
  senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))]
  rate_sum <- sum(senti_rate)
  if(rate_sum == 0){
    df$sentiment_mean[i] <- 0
  } else{
    df$sentiment_mean[i] <- round(sum(senti_rate) / length(senti_rate), digits = 2)  
  }
}

for(i in 1:nrow(df)){
  if((df$good1[i]+df$good2[i])==0)
    df$good_total[i] <- 0
  else
    df$good_total[i] <- (df$good1[i] + df$good2[i]*2)/(df$good1[i]+df$good2[i])
  if((df$bad1[i]+df$bad2[i])==0)
    df$bad_total[i] <- 0
  else
    df$bad_total[i] <- (df$bad1[i] + df$bad2[i]*2)/(df$bad1[i]+df$bad2[i])
}

colnames(df)
#check elbow
find_elbow <- function(data,cols){
  wss <- 0
  for(i in 1:10){
    wss[i] <- sum(kmeans( data[cols], centers=i ) $ withiness )
  }
  plot( 1:10, wss, type="b",xlab="The Number of Clusters", ylab = "Within group sum of squares")
}
# [12] "sentiment_mean"
# [13] "bad2"           "bad1"           "normal"         "good1"         
# [17] "good2"          "good_total"     "bad_total"      "sentiment"  


# min(df$upload_time) #2009-08-19
# max(df$upload_time) #2018-11-29
# 
# df_201202 <- which(df$upload_time<'2013-01-01')
# df_201301 <- which(df$upload_time>='2013-01-01' & df$upload_time<'2013-08-01')
# df_201302 <- which(df$upload_time>='2013-08-01' & df$upload_time<'2014-01-01')
# df_201401 <- which(df$upload_time>='2014-01-01' & df$upload_time<'2014-08-01')
# df_201402 <- which(df$upload_time>='2014-08-01' & df$upload_time<'2015-01-01')
# df_201501 <- which(df$upload_time>='2015-01-01' & df$upload_time<'2015-08-01')
# df_201502 <- which(df$upload_time>='2015-08-01' & df$upload_time<'2016-01-01')
# df_201601 <- which(df$upload_time>='2016-01-01' & df$upload_time<'2016-08-01')
# df_201602 <- which(df$upload_time>='2016-08-01' & df$upload_time<'2017-01-01')
# df_201701 <- which(df$upload_time>='2017-01-01' & df$upload_time<'2017-08-01')
# df_201702 <- which(df$upload_time>='2017-08-01' & df$upload_time<'2018-01-01')
# df_201801 <- which(df$upload_time>='2018-01-01' & df$upload_time<'2018-08-01')
# df_201802 <- which(df$upload_time>='2018-08-01' & df$upload_time<'2019-01-01')
# 
# df$upload_time[df_201202] <- '2012-08-01'
# 
# df$upload_time[df_201301] <- '2013-01-01'
# df$upload_time[df_201302] <- '2013-08-01'
# df$upload_time[df_201401] <- '2014-01-01'
# df$upload_time[df_201402] <- '2014-08-01'
# df$upload_time[df_201501] <- '2015-01-01'
# df$upload_time[df_201502] <- '2015-08-01'
# df$upload_time[df_201601] <- '2016-01-01'
# df$upload_time[df_201602] <- '2016-08-01'
# df$upload_time[df_201701] <- '2017-01-01'
# df$upload_time[df_201702] <- '2017-08-01'
# df$upload_time[df_201801] <- '2018-01-01'
# df$upload_time[df_201802] <- '2018-08-01'

View(df)

library(ggplot2)
as.vector(levels(as.factor(df$upload_time)))

time_df <- data.frame(matrix(NA,nrow=13,ncol=4))
colnames(time_df) <- c('time','pos','neg','total')
time_df$time <- as.vector(levels(as.factor(df$upload_time)))

length(which((as.character(df$upload_time))=="2018-07-31 15:00:00"))

View(df)
df_class_refined <- df[!is.na(df$class),]

for(i in 1:length(levels(as.factor(df$upload_time)))){
  target_date <- levels(as.factor(df$upload_time))[i]
  print(target_date)
  
  pos_count <- length(which((as.character(df_class_refined$upload_time)==target_date) & (df_class_refined$class==3)))
  neg_count <- length(which((as.character(df_class_refined$upload_time)==target_date) & (df_class_refined$class==2)))

  time_df$pos[i] <- pos_count
  time_df$neg[i] <- neg_count
  time_df$total[i] <- pos_count+neg_count
}
time_df$pos_rate <- round(time_df$pos/time_df$total,digits = 2)
time_df$neg_rate <- round(time_df$neg/time_df$total,digits = 2)

for(i in 1:nrow(time_df)){
  time_df$senti_pos <- 0
  time_df$senti_neg <- 0
}
for(i in 1:length(levels(as.factor(df$upload_time)))){
  target_date <- levels(as.factor(df$upload_time))[i]
  print(target_date)
  
  senti_pos_count <- length(which((as.character(df$upload_time)==target_date) & (df$sentiment_mean>0)))
  senti_neutral_count <- length(which((as.character(df$upload_time)==target_date) & (df$sentiment_mean==0)))
  senti_neg_count <- length(which((as.character(df$upload_time)==target_date) & (df$sentiment_mean<0)))
  
  time_df$senti_pos[i] <- senti_pos_count
  time_df$senti_neg[i] <- senti_neg_count
  time_df$senti_neutral[i] <- senti_neutral_count
}
time_df$senti_toal <- time_df$senti_pos+time_df$senti_neg+time_df$senti_neutral
time_df$senti_pos_rate <- round(time_df$senti_pos/time_df$senti_toal,digits=2)
time_df$senti_neg_rate <- round(time_df$senti_neg/time_df$senti_toal,digits=2)
time_df$senti_neutral_rate <- round(time_df$senti_neutral/time_df$senti_toal,digits=2)

time_df_reshape <- as.data.frame(matrix(NA,nrow=26,ncol=6))
colnames(time_df_reshape) <- c('time','class','count','rate','senti_count','senti_rate')

for(i in 1:nrow(time_df)){
  target_date <- levels(as.factor(df$upload_time))[i]
  print(target_date)
  
  time_df_reshape$time[i*2-1] <- target_date
  time_df_reshape$time[i*2] <- target_date
  
  time_df_reshape$class[i*2-1] <- "pos"
  time_df_reshape$class[i*2] <- "neg"
  
  time_df_reshape$count[i*2-1] <- time_df$pos[i]
  time_df_reshape$count[i*2] <- time_df$neg[i]
  
  time_df_reshape$rate[i*2-1] <- time_df$pos_rate[i]
  time_df_reshape$rate[i*2] <- time_df$neg_rate[i]
  
  time_df_reshape$senti_count[i*2-1] <- time_df$senti_pos[i]
  time_df_reshape$senti_count[i*2] <- time_df$senti_neg[i]
  
  time_df_reshape$senti_rate[i*2-1] <- time_df$senti_pos_rate[i]
  time_df_reshape$senti_rate[i*2] <- time_df$senti_neg_rate[i]
}
colnames(time_df_reshape)


time_df_reshape$time <- gsub('-31 15:00:00','',time_df_reshape$time)
time_df_reshape$time <- gsub('-31','',time_df_reshape$time)
time_df_reshape_pos <- time_df_reshape[time_df_reshape$class=='pos',]
time_df_reshape_neg <- time_df_reshape[time_df_reshape$class=='neg',]

time_df_reshape_pos
time_df_reshape_neg

windows()
ggplot()+
  geom_line(data=time_df_reshape_pos,aes(x=time,y=senti_rate,group=1),color="blue")+
  geom_line(data=time_df_reshape_neg,aes(x=time,y=senti_rate,group=1),color="red")+
  xlab('Time')+
  ylab('Rate')

#kmeans
find_elbow(df,13:19)
df.kmeans <- kmeans(df[,18:19], centers=2)
plot(df[,18:19],col=df.kmeans$cluster,main="good_total-bad_total")

cluster_1 = which(df.kmeans$cluster==1)
cluster_2 = which(df.kmeans$cluster==2)

#world cloud
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#remove punctual characters
df$content <- gsub("[.,():;+-]","",df$content)
df$content <- gsub("[[:punct:]]", " ", df$content)
df$content <- gsub("]","",df$content)
df$content <- gsub("'", "", df$content)
df$content <- gsub("[0-9]","",df$content)
df$content <- gsub("[a-z]","",df$content)
df$content <- gsub("[A-Z]","",df$content)

#remove quotation ' , "
df$content <- noquote(df$content)

df$NounContent <- sapply(df$content,extractNoun,USE.NAMES = F)
NounContentList_1 <- unlist(df$NounContent[cluster_1])
NounContentList_2 <- unlist(df$NounContent[cluster_2])


LastData_1 <- Filter(function(x){
  nchar(x)>=2
},NounContentList_1)
LastData_2 <- Filter(function(x){
  nchar(x)>=2
},NounContentList_2)

View(LastData)
ListWordCount_1 <- table(LastData_1)
ListWordCount_2 <- table(LastData_2)
View(ListWordCount)

windows()
windowsFonts(font=windowsFont("맑은고딕"))
wordcloud(names(ListWordCount_1),freq=ListWordCount_1,scale=c(5,0.2),
          rot.per=0.1,min.freq=3,max.words = 100, 
          random.order=F,random.color=T,
          colors=brewer.pal(11,"Paired"),family="font")
wordcloud(names(ListWordCount_2),freq=ListWordCount_2,scale=c(5,0.2),
          rot.per=0.1,min.freq=3,max.words = 100, 
          random.order=F,random.color=T,
          colors=brewer.pal(11,"Paired"),family="font")

####################################################

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