# install.packages("stringr")
# install.packages("readxl")
# install.packages("KoNLP") 
# install.packages("arules")
# install.packages("wordcloud2")
# install.packages("gcookbook")


##################### setting ##################### 
setwd("C:/Users/ok3651004/Desktop/DataSentiment")
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_144')
library(KoNLP) # Korean natural language processor
library(readxl)
library(stringr)
library(wordcloud2)
library(caret)
library(cluster)
library(arules)
library(ggplot2) # fancy k-means plot
library(gcookbook)
library(plyr)
library(reshape2)

##################### rawdata loading ##################### 
df <- read_excel("NamYang_Data_Added_Total.xlsx",col_names=T,na="NA")

##################### sentiment dictionary loading ######################
sf <- read_excel("SentiWord_Dict_excel.xlsx",col_names=T,na="NA")

##################### col setting ######################
for(i in 1:nrow(df)){
  df$bad2[i] <- 0           # the number of -2 score senti keyword 
  df$bad1[i] <- 0           # the number of -1 score senti keyword 
  df$normal[i] <- 0         # the number of 0 score senti keyword 
  df$good1[i] <- 0          # the number of 1 score senti keyword
  df$good2[i] <- 0          # the number of 2 score senti keyword
  df$good_total[i] <- 0     # mean of 1, 2 score senti keyword
  df$bad_total[i] <- 0      # mean of -1, 2 score senti keyword
  df$sentiment_sum[i] <- 0  # sum of score senti keyword
  df$sentiment_mean[i] <- 0 # mean of score senti keyword
}

##################### insert sentidata ######################
# good1, 2, normal, bad 1, 2, keywords
for(i in 1:nrow(df)){
  senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))] # bad -2 ~ 2 good
  senti_text <- sf$text[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))] # senti word
  if(is.na(senti_rate[1])){
    next
  } else{
    very_good_keyword <- c()
    good_keyword <- c()
    bad_keyword <- c()
    very_bad_keyword <- c()
    for(j in 1:length(senti_rate)){
      if(senti_rate[j] == 1){
        df$good1[i] <- df$good1[i]+1
        good_keyword <- c(good_keyword, senti_text[j])
      } else if(senti_rate[j] == 2){
        df$good2[i] <- df$good2[i]+1
        very_good_keyword <- c(very_good_keyword, senti_text[j])
      } else if(senti_rate[j] == -1){
        df$bad1[i] <- df$bad1[i]+1
        bad_keyword <- c(bad_keyword, senti_text[j])
      } else if(senti_rate[j] == -2){
        df$bad2[i] <- df$bad2[i]+1
        very_bad_keyword <- c(very_bad_keyword, senti_text[j])
      } else if(senti_rate[j] == 0){
        df$normal[i] <- df$normal[i]+1
      }
    }
    very_good_keywords <- paste(very_good_keyword, collapse = ",")
    good_keywords <- paste(good_keyword, collapse = ",")
    bad_keywords <- paste(bad_keyword, collapse = ",")
    very_bad_keywords <- paste(very_bad_keyword, collapse = ",")
    df$very_good_keywords[i] <- very_good_keywords
    df$good_keywords[i] <- good_keywords
    df$bad_keywords[i] <- bad_keywords
    df$very_bad_keywords[i] <- very_bad_keywords
  }
}

# good_total, bad_total
for(i in 1:nrow(df)){
  if((df$good1[i]+df$good2[i])==0)
    df$good_total[i] <- 0
  else
    df$good_total[i] <- round((df$good1[i] + df$good2[i]*2)/(df$good1[i]+df$good2[i]), digits = 2)
  if((df$bad1[i]+df$bad2[i])==0)
    df$bad_total[i] <- 0
  else
    df$bad_total[i] <- round((df$bad1[i] + df$bad2[i]*2)/(df$bad1[i]+df$bad2[i]), digits = 2)
}

# sentiment_sum
for(i in 1:nrow(df)){
  df$sentiment_sum[i] <- sum(sf$score[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))])
}

# sentiment_mean
for(i in 1:nrow(df)){
  senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))]
  rate_sum <- sum(senti_rate)
  if(rate_sum == 0){
    df$sentiment_mean[i] <- 0
  } else{
    df$sentiment_mean[i] <- round(sum(senti_rate) / length(senti_rate), digits = 2)  
  }
}

##################### bad_keyword vextor ######################
company_bad_keyword <-c()
for(i in 1:nrow(df)){
  keyword <- df$very_bad_keywords[i]
  keyword2 <- df$bad_keywords[i]
  if(keyword == ""){
    next
  } else{
    frag <- str_split(keyword, ",")
    frag2 <- str_split(keyword2, ",")
    for(j in 1: length(frag[[1]])){
      company_bad_keyword <- c(company_bad_keyword, frag[[1]][j])
      company_bad_keyword <- c(company_bad_keyword, frag2[[1]][j])
    }
  }
}
company_bad_keyword <- na.omit(company_bad_keyword)
company_bad_keyword

# make bad_keyword table
c_bad_list <- table(company_bad_keyword)
c_bad_list

###################### pos tagging ##########################  -> not use
taggedText <- paste(MorphAnalyzer(company_bad_keyword))
attrs <- str_match(taggedText, "(\"|\\+)([가-힣]{2,4})(/ncps|/ncn|/pvg|/paa)")
company_bad_keyword_tagged <- na.omit(attrs[,3])
company_bad_keyword_tagged


##################### show wordcloud2 ######################
wordcloud2(c_bad_list, size=4, minSize = 1)

##################### k-means cluster ######################
# find elbow point : k=6
wss <- 0
for(i in 1:15){
  wss[i] <- sum(kmeans(df[c("good_total", "bad_total")], centers = i) $ withinss)
}
plot(1:15, wss, type = "b", xlab = "클러스터 수", ylab = "ss값")

# alg
df.kmeans <- kmeans(df[,c("good_total", "bad_total")], centers=6)
df.kmeans
df.kmeans$cluster
plot(df[,c("good_total", "bad_total")],col=df.kmeans$cluster, main="good_total-bad_total")

# make plot using text and point
text(x=df$good_total, y=df$bad_total, labels = df$index, col=df.kmeans$cluster+1)
points(df.kmeans$centers[,c("good_total", "bad_total")], pch=8, cex=3)

# show cluster for fancy plot
qplot(df$good_total, df$bad_total)
ggplot(df, aes(x = good_total, y = bad_total)) + geom_point(aes(colour=df.kmeans$cluster))
# clustering index
cluster_1 = which(df.kmeans$cluster==1)
cluster_1
cluster_2 = which(df.kmeans$cluster==2)
cluster_2
cluster_3 = which(df.kmeans$cluster==3)
cluster_3
cluster_4 = which(df.kmeans$cluster==4)
cluster_4
cluster_5 = which(df.kmeans$cluster==5)
cluster_5
cluster_6 = which(df.kmeans$cluster==6)
cluster_6

# select bad : 1 ~ 2 / good : 0 (only has bad keyword) => mostly bad contents
df_onlybad_clustered <- df[cluster_5, ]
df.kmeans

##################### clusterd_bad_keyword vextor ######################
clusterd_bad_keyword <-c()
# -2 keywords
for(i in 1:nrow(df_onlybad_clustered)){
  keyword <- df_onlybad_clustered$very_bad_keywords[i]
  if(keyword == ""){
    next
  } else{
    frag <- str_split(keyword, ",")
    for(j in 1: length(frag[[1]])){
      clusterd_bad_keyword <- c(clusterd_bad_keyword, frag[[1]][j])
    }
  }
}

# -1 keywords
for(i in 1:nrow(df_onlybad_clustered)){
  keyword <- df_onlybad_clustered$bad_keywords[i]
  if(keyword == ""){
    next
  } else{
    frag <- str_split(keyword, ",")
    for(j in 1: length(frag[[1]])){
      clusterd_bad_keyword <- c(clusterd_bad_keyword, frag[[1]][j])
    }
  }
}

clusterd_bad_keyword
# make clusterd_bad_keyword table
clusterd_bad_list <- table(clusterd_bad_keyword)


##################### show wordcloud2 ######################
wordcloud2(clusterd_bad_list, size=1, minSize = 1)


##################### make transaction data ######################
for(i in 1:nrow(df_onlybad_clustered)){
  if(textToBasket(df_onlybad_clustered$content_all[i])==""){
    next
  } else{
    write.table(textToBasket(df_onlybad_clustered$content_all[i]), file = "bad_contents_transaction.txt", append = T, row.names = F, col.names = F, quote = F)
  }
}

##################### read transaction data ######################
tr.data.bad_content <- read.transactions(file = "bad_contents_transaction.txt", format = "basket", sep = ',')

##################### find new bad_keyword use apriori ######################
# show transaction data
summary(tr.data.bad_content)
image(tr.data.bad_content)
itemFrequencyPlot(tr.data.bad_content, support = 0.16)
itemLabels(tr.data.bad_content)
# use apriori
bad_keyword_rule <- apriori(tr.data.bad_content,
                  parameter = list(minlen=2, supp=0.2, conf=0.2))
# sort
bad_keyword_rule <- sort(bad_keyword_rule, decreasing = TRUE, by = "support")
# show rule
inspect(bad_keyword_rule)
# insert our keyword => no selected
clusterd_bad_keyword_unique <- unique(clusterd_bad_keyword)
clusterd_bad_keyword_unique
nega.new <- subset(bad_keyword_rule, items %pin% clusterd_bad_keyword_unique)
inspect(nega.new)

# delete keword(no_related)
elim.word <- c("모르", "양유", "보이", "이제", "영상", "맛잇", "대리점", "회사", "사실", "분유", "제품", "등이", "동아", 
               "사업", "기업", "하기", "되기", "양에","자료", "내용")
# use apriori delete no related keyword
bad_keyword_rule_concen <- apriori(tr.data.bad_content,
                  parameter = list(minlen=2, supp=0.08, conf=0.5),
                  appearance = list(none = elim.word, default = "both"))
bad_keyword_rule_concen <- sort(bad_keyword_rule_concen, decreasing = TRUE, by = "support")
inspect(bad_keyword_rule_concen)

# extract new bad word
new_bad_keyword_list <- as(rhs(bad_keyword_rule_concen), "list")
new_bad_keyword <- c()
for(i in 1:length(new_bad_keyword_list)){
  new_bad_keyword <- c(new_bad_keyword, new_bad_keyword_list[[i]])
}
new_bad_keyword

# make new bad word table
new_bad_keyword_table <- table(new_bad_keyword)
new_bad_keyword_table

# show new bad word for wordcloud
wordcloud2(new_bad_keyword_table, size=0.8, minSize = 1)

# new bad keyword
beta_bad_keyword <- unique(new_bad_keyword)


# insert our keyword in new rule
nega.new <- subset(bad_keyword_rule_concen, items %pin% "아니")
nega.new <- subset(bad_keyword_rule_concen, items %pin% "다르")
inspect(nega.new)

df_new_bad_keyword <- df[grep("갑질", df$content_all) ,]






