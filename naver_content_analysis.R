  # install.packages("stringr")
  # install.packages("readxl")
  # install.packages("KoNLP") 
  # install.packages("arules")
  # install.packages("wordcloud2")
  # install.packages("gcookbook")
  # install.packages("xlsx")
  
  
  ##################### setting ##################### 
  setwd("/Users/theorist/Documents/R-WorkingDirectory/DataSentiment")
  #setwd("C:/Users/ok3651004/Desktop/DataSentiment")
  #Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_144')
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
  library(xlsx)
  
  ##########################################################################################
  ##########################################################################################
  ########################################### F ############################################
  ########################################### U ############################################
  ########################################### N ############################################
  ########################################### C ############################################
  ########################################### T ############################################
  ########################################### I ############################################
  ########################################### O ############################################
  ########################################### N ############################################
  ########################################### S ############################################
  ##########################################################################################
  ##########################################################################################
  
  ##################### preprocess ######################
  #column setting
  initKeywordAttrbs <- function(df){
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
    return(df)
  }
  ##################### preprocess ######################
  #count sentimen keywords and write both number and values to the given data frame 
  #all 5 attibutes are used; good1, good2, normal, bad1, bad2.
  countKeywordsAndWriteKeywordsToDataPerRecord <- function(df, sf){
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
    return(df)
  }
  
  # good_total, bad_total
  scoreTotal <- function(df){
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
    return(df)
  }
  
  
  # sentiment_sum
  sumSentimentCounts <- function(df){
    for(i in 1:nrow(df)){
      df$sentiment_sum[i] <- sum(sf$score[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))])
    }
    return(df)
  }
  
  # sentiment_mean
  meanSentimnetCounts <- function(df){
    for(i in 1:nrow(df)){
      senti_rate <- sf$score[which(sf$text %in% unlist(str_split(df$content_all[i], " ")))]
      rate_sum <- sum(senti_rate)
      if(rate_sum == 0){
        df$sentiment_mean[i] <- 0
      } else{
        df$sentiment_mean[i] <- round(sum(senti_rate) / length(senti_rate), digits = 2)  
      }
    }
    return(df)
  }
  
  # bad_keyword vector
  getBadKeywordsRelatedToTheCompany <- function(df){
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
    return(company_bad_keyword)
  }
  
  ##################### k-means cluster ######################
  # find elbow point : k=6
  plotElbowPoint<-function(df){
    wss <- 0
    for(i in 1:15){
      wss[i] <- sum(kmeans(df[c("good_total", "bad_total")], centers = i) $ withinss)
    }
    plot(1:15, wss, type = "b", xlab = "클러스터 수", ylab = "ss값")
  }
  
  # performs K-means clustering algorithm ; x axis for nubmer of good keywords, y axis for the number of bad keywords
  plotKMeansCluster <- function(df){
    df.kmeans <- kmeans(df[,c("good_total", "bad_total")], centers=6)
    df.kmeans
    df.kmeans$cluster
    plot(df[,c("good_total", "bad_total")],col=df.kmeans$cluster, main="good_total-bad_total")
    return(df.kmeans)
  }
  
  # make plot using text and point
  writeTextAndMarkPointsOnPlot <- function(df){
    text(x=df$good_total, y=df$bad_total, labels = df$index, col=df.kmeans$cluster+1)
    points(df.kmeans$centers[,c("good_total", "bad_total")], pch=8, cex=3)
  }
  
  
  # show cluster for fancy plot
  plotClusterFancy<-function(df){
    ggplot(df, aes(x = good_total, y = bad_total)) + geom_point(aes(colour=df.kmeans$cluster))
  }
  
  ##################### clusterd_bad_keyword vector ######################
  clusteredBadKeywordVector<-function(df_onlybad_clustered){
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
    return(clusterd_bad_keyword)
  }
  
  ##################### make transaction data ######################
  
  textToBasket <- function(text){
    taggedText <- paste(MorphAnalyzer(text))
    attrs <- str_match(taggedText, "(\"|\\+)((([가-힣]{2,4})/(ncps|ncn))|(([가-힣]{2,4})/(pvg|paa)))") #상태명사, 비서술명사 (2,4) 일반동사, 성상형용사 (1,3)
    attrs <- unique(na.omit(c(attrs[,5], attrs[,8])))
    basketline <- paste(attrs, collapse = ",")
    #print(basketline) #print(text) #print(taggedText)  #디버깅용
    return(basketline)
  }
  
  makeTransactionDataFile<-function(df_onlybad_clustered){
    for(i in 1:nrow(df_onlybad_clustered)) {
      if(textToBasket(df_onlybad_clustered$content_all[i])==""){
        next
      } else {
        write.table(textToBasket(df_onlybad_clustered$content_all[i]), file = "bad_contents_transaction.txt", append = T, row.names = F, col.names = F, quote = F)
      }
    }
  }
  
  
  ###################### pos tagging ########################## 
  #taggedText <- paste(MorphAnalyzer(company_bad_keyword))
  #attrs <- str_match(taggedText, "(\"|\\+)([가-힣]{2,4})(/ncps|/ncn|/pvg|/paa)")
  #company_bad_keyword_tagged <- na.omit(attrs[,3])
  #company_bad_keyword_tagged
  
  
  ##########################################################################################
  ##########################################################################################
  ########################################### N ############################################
  ########################################### E ############################################
  ########################################### W ############################################
  ###########################################   ############################################
  ########################################### B ############################################
  ########################################### A ############################################
  ########################################### D ############################################
  ###########################################   ############################################
  ########################################### K ############################################
  ########################################### E ############################################
  ########################################### Y ############################################
  ########################################### W ############################################
  ########################################### O ############################################
  ########################################### R ############################################
  ########################################### D ############################################
  ########################################### S ############################################
  ##########################################################################################
  ##########################################################################################
  
  ##################### rawdata loading ##################### 
  df <- read_excel("NamYang_Data_Added_Total.xlsx",col_names=T,na="NA")
  
  ##################### sentiment dictionary loading ######################
  sf <- read_excel("SentiWord_Dict_excel.xlsx",col_names=T,na="NA")
  
  ##################### preprocess ######################
  df<-initKeywordAttrbs(df)
  df<-countKeywordsAndWriteKeywordsToDataPerRecord(df, sf)
  df<-scoreTotal(df)
  df<-sumSentimentCounts(df)
  df<-meanSentimnetCounts(df)
  
  ##################### print word cloud ######################
  company_bad_keyword <- getBadKeywordsRelatedToTheCompany(df)
  wordcloud2(table(company_bad_keyword), size=4, minSize = 1)
  
  ##################### plotting ######################
  plotElbowPoint(df)
  df.kmeans <- plotKMeansCluster(df)
  writeTextAndMarkPointsOnPlot(df)
  plotClusterFancy(df)
  
  
  ##################### cluster indexing ######################
  cluster_1 = which(df.kmeans$cluster==1)
  cluster_2 = which(df.kmeans$cluster==2)
  cluster_3 = which(df.kmeans$cluster==3)
  cluster_4 = which(df.kmeans$cluster==4)
  cluster_5 = which(df.kmeans$cluster==5)
  cluster_6 = which(df.kmeans$cluster==6)
  
  # select bad : 1 ~ 2 / good : 0 (only has bad keyword) => mostly bad contents
  df_onlybad_clustered <- df[cluster_3, ]
  clustered_bad_keyword <- clusteredBadKeywordVector(df_onlybad_clustered)
  wordcloud2(table(clustered_bad_keyword), size=1, minSize = 1)
  makeTransactionDataFile(df_onlybad_clustered)
  
  # read transaction data file
  tr.data.bad_content <- read.transactions(file = "bad_contents_transaction.txt", format = "basket", sep = ',')
  
  
  ##################### find new bad_keyword use apriori ######################
  # show transaction data
  summary(tr.data.bad_content)
  image(tr.data.bad_content)
  itemFrequencyPlot(tr.data.bad_content, support = 0.16)
  itemLabels(tr.data.bad_content)
  # use apriori
  # bad_keyword_rule <- apriori(tr.data.bad_content,
  #                             parameter = list(minlen=2, supp=0.2, conf=0.2))
  # # sort
  # bad_keyword_rule <- sort(bad_keyword_rule, decreasing = TRUE, by = "support")
  # # show rule
  # inspect(bad_keyword_rule)
  # # insert our keyword => no selected
  # clusterd_bad_keyword_unique <- unique(clustered_bad_keyword)
  # clusterd_bad_keyword_unique
  # nega.new <- subset(bad_keyword_rule, items %pin% clusterd_bad_keyword_unique)
  # nega.new <- subset(bad_keyword_rule, items %pin% clusterd_bad_keyword_unique)
  # inspect(nega.new)
  
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
  
  (df_new_bad_keyword <- df[grep("갑질", df$content_all) ,])
  write.xlsx(df_new_bad_keyword, file="test.xlsx")
  
  
  
  
  
  
  
  
  ##########################################################################################
  ##########################################################################################
  ########################################### T ############################################
  ########################################### I ############################################
  ########################################### M ############################################
  ########################################### E ############################################
  ###########################################   ############################################
  ########################################### S ############################################
  ########################################### E ############################################
  ########################################### R ############################################
  ########################################### I ############################################
  ########################################### E ############################################
  ########################################### S ############################################
  ###########################################   ############################################
  ########################################### A ############################################
  ########################################### N ############################################
  ########################################### A ############################################
  ########################################### L ############################################
  ########################################### S ############################################
  ########################################### I ############################################
  ########################################### S ############################################
  ##########################################################################################
  ##########################################################################################
  
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
  
  ################### elimination ?닔?젙 ?븘?슂 ######################
  library(stringr)
  # str_replace_all(df$content_all, "[ㄱ-ㅎ]", "") %>%  #remove ㅋㅋㅋㅋ
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
  
  
  df_201202 <- which(df$upload_time<'2013-01-01')
  df_201301 <- which(df$upload_time>='2013-01-01' & df$upload_time<'2013-08-01')
  df_201302 <- which(df$upload_time>='2013-08-01' & df$upload_time<'2014-01-01')
  df_201401 <- which(df$upload_time>='2014-01-01' & df$upload_time<'2014-08-01')
  df_201402 <- which(df$upload_time>='2014-08-01' & df$upload_time<'2015-01-01')
  df_201501 <- which(df$upload_time>='2015-01-01' & df$upload_time<'2015-08-01')
  df_201502 <- which(df$upload_time>='2015-08-01' & df$upload_time<'2016-01-01')
  df_201601 <- which(df$upload_time>='2016-01-01' & df$upload_time<'2016-08-01')
  df_201602 <- which(df$upload_time>='2016-08-01' & df$upload_time<'2017-01-01')
  df_201701 <- which(df$upload_time>='2017-01-01' & df$upload_time<'2017-08-01')
  df_201702 <- which(df$upload_time>='2017-08-01' & df$upload_time<'2018-01-01')
  df_201801 <- which(df$upload_time>='2018-01-01' & df$upload_time<'2018-08-01')
  df_201802 <- which(df$upload_time>='2018-08-01' & df$upload_time<'2019-01-01')
  
  df$upload_time[df_201202] <- '2012-08-01'
  
  df$upload_time[df_201301] <- '2013-01-01'
  df$upload_time[df_201302] <- '2013-08-01'
  df$upload_time[df_201401] <- '2014-01-01'
  df$upload_time[df_201402] <- '2014-08-01'
  df$upload_time[df_201501] <- '2015-01-01'
  df$upload_time[df_201502] <- '2015-08-01'
  df$upload_time[df_201601] <- '2016-01-01'
  df$upload_time[df_201602] <- '2016-08-01'
  df$upload_time[df_201701] <- '2017-01-01'
  df$upload_time[df_201702] <- '2017-08-01'
  df$upload_time[df_201801] <- '2018-01-01'
  df$upload_time[df_201802] <- '2018-08-01'
  
  
  # min(df$upload_time) #2009-08-19
  # max(df$upload_time) #2018-11-29
  # 
  
  View(df)
  
  library(ggplot2)
  
  time_df <- data.frame(matrix(NA,nrow=13,ncol=4))
  colnames(time_df) <- c('time','pos','neg','total')
  time_df$time <- as.vector(levels(as.factor(df$upload_time)))
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
    senti_neg_count <- length(which((as.character(df$upload_time)==target_date) & (df$sentiment_mean<=0)))
    
    time_df$senti_pos[i] <- senti_pos_count
    time_df$senti_neg[i] <- senti_neg_count
    
  }
  time_df$senti_toal <- time_df$senti_pos+time_df$senti_neg
  time_df$senti_pos_rate <- round(time_df$senti_pos/time_df$senti_toal,digits=2)
  time_df$senti_neg_rate <- round(time_df$senti_neg/time_df$senti_toal,digits=2)
  
  
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
  df$upload_time <- gsub('-31 15:00:00','',df$upload_time)
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
  df$content_all <- gsub("[.,():;+-]","",df$content_all)
  df$content_all <- gsub("[[:punct:]]", " ", df$content_all)
  df$content_all <- gsub("]","",df$content_all)
  df$content_all <- gsub("'", "", df$content_all)
  df$content_all <- gsub("[0-9]","",df$content_all)
  df$content_all <- gsub("[a-z]","",df$content_all)
  df$content_all <- gsub("[A-Z]","",df$content_all)
  
  #remove quotation ' , "
  df$content_all <- noquote(df$content_all)
  
  df$NounContent <- sapply(df$content_all,extractNoun,USE.NAMES = F)
  NounContentList_1 <- unlist(df$NounContent[cluster_1])
  NounContentList_2 <- unlist(df$NounContent[cluster_2])
  
  
  LastData_1 <- Filter(function(x){
    nchar(x)>=2
  },NounContentList_1)
  LastData_2 <- Filter(function(x){
    nchar(x)>=2
  },NounContentList_2)
  
  ListWordCount_1 <- table(LastData_1)
  ListWordCount_2 <- table(LastData_2)
  ListWordCount_1 <- ListWordCount_1[ListWordCount_1>20]
  
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
  
  wordcloud2(ListWordCount_1, size=4, minSize = 1)
  
  
  #########timeSeries Data Topic############
  time_category <- as.vector(levels(as.factor(df$upload_time)))
  
  pos_topic <- data.frame(matrix(NA,nrow=13,ncol=2))
  colnames(pos_topic) <- c("upload_time","topics")
  neg_topic <- data.frame(matrix(NA,nrow=13,ncol=2))
  colnames(neg_topic) <- c("upload_time","topics")
  
  for(i in 1:length(time_category)){
    target_date <- levels(as.factor(df$upload_time))[[i]]
    #by Sentiment rate
    NounContentList_pos <- unlist(df$NounContent[which((as.character(df$upload_time)==target_date) & df$sentiment_mean > 0)])
    NounContentList_neg <- unlist(df$NounContent[which((as.character(df$upload_time)==target_date) & df$sentiment_mean <= 0)])
    # pos_count <- length(which((as.character(df_class_refined$upload_time)==target_date) & (df_class_refined$class==3)))
    
    LastData_pos <- Filter(function(x){
      nchar(x)>=2
    },NounContentList_pos)
    LastData_neg <- Filter(function(x){
      nchar(x)>=2
    },NounContentList_neg)
    
    ListWordCount_pos <- table(LastData_pos)
    ListWordCount_neg <- table(LastData_neg)
    
    pos_topic[["upload_time"]][i] <- target_date
    pos_len <- length(ListWordCount_pos)
    pos_top7 <- ListWordCount_pos[order(ListWordCount_pos)[(pos_len - 7):pos_len]]
    pos_topic[["topics"]][i] <- paste(as.vector(as.data.frame(pos_top7)[[1]]),collapse=",")
    
    neg_topic[["upload_time"]][i] <- target_date
    neg_len <- length(ListWordCount_neg)
    neg_top7 <- ListWordCount_neg[order(ListWordCount_neg)[(neg_len - 7):neg_len]]
    neg_topic[["topics"]][i] <- paste(as.vector(as.data.frame(neg_top7)[[1]]),collapse=",")
  }
  
  windows()
  ggplot()+
    geom_line(data=time_df_reshape_pos,aes(x=time,y=senti_rate,group=1),color="blue")+
    geom_line(data=time_df_reshape_neg,aes(x=time,y=senti_rate,group=1),color="red")+
    xlab('Time')+
    ylab('Rate')
  
  time_pos_df <-cbind(time_df_reshape_pos,topics=pos_topic[["topics"]])
  time_neg_df <-cbind(time_df_reshape_neg,topics=neg_topic[["topics"]])
  
  windows()
  ggplot(time_pos_df,aes(x=time,y=senti_rate))+
    geom_text(aes(label=topics),color="blue",size=5,fontface="bold",check_overlap = T)+
    geom_line(aes(x=time,y=senti_rate,group=1),color="blue")+
    geom_text(data=time_neg_df,aes(label=topics),color="red",size=5,fontface="bold",check_overlap = T)+
    geom_line(data=time_neg_df,aes(x=time,y=senti_rate,group=1),color="red")
  # for(i in 1:length(time_category)){
  #   #by ground-truth class
  #   
  #   
  # }
  
  
  create_wordcloud <- function(data,content='content_all',row_index=1:nrow(data)){
    #world cloud
    library(tm)
    library(SnowballC)
    library(wordcloud2)
    library(RColorBrewer)
    
    content_vector <- data[,content]
    #remove punctual characters
    content_vector <- gsub("[.,():;+-]","",content_vector)
    content_vector <- gsub("[[:punct:]]", " ", content_vector)
    content_vector <- gsub("]","",content_vector)
    content_vector <- gsub("'", "", content_vector)
    content_vector <- gsub("[0-9]","",content_vector)
    content_vector <- gsub("[a-z]","",content_vector)
    content_vector <- gsub("[A-Z]","",content_vector)
    
    #remove quotation ' , "
    content_vector <- noquote(content_vector)
    
    NounContent <- sapply(content_vector,extractNoun,USE.NAMES = F)
    NounContentList <- unlist(NounContent[row_index])
    
    LastData <- Filter(function(x){
      nchar(x)>=2
    },NounContentList)
    
    ListWordCount <- table(LastData)
    
    
    # windows()
    # windowsFonts(font=windowsFont("맑은고딕"))
    wordcloud2(ListWordCount, size=4, minSize = 1)
  }
  # create_wordcloud(df)
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
  
  ##########################################################################################
  ##########################################################################################
  ########################################### T ############################################
  ########################################### O ############################################
  ########################################### P ############################################
  ########################################### I ############################################
  ########################################### C ############################################
  ###########################################   ############################################
  ########################################### B ############################################
  ########################################### A ############################################
  ########################################### S ############################################
  ########################################### E ############################################
  ########################################### D ############################################
  ###########################################   ############################################
  ########################################### C ############################################
  ########################################### L ############################################
  ########################################### U ############################################
  ########################################### S ############################################
  ########################################### T ############################################
  ########################################### E ############################################
  ########################################### R ############################################
  ########################################### I ############################################
  ########################################### N ############################################
  ########################################### G ############################################
  ##########################################################################################
  ##########################################################################################
  
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
  
