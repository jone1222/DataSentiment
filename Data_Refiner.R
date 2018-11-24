library(readxl)
setwd("/Users/theorist/Documents/R-WorkingDirectory/DataSentiment")
df <- read_excel("NamYang_Data_Refined.xlsx",col_names=T,na="NA")

library(KoNLP)

#build dic
dics <- c('woorimalsam') #sejong ...
category <- c('emotions')
user_d <- data.frame(readLines("wenoun.txt"), "ncn") #fill this file
buildDictionary(ext_dic = dics,
                category_dic_nms = category, 
                user_dic = user_d,
                replace_usr_dic = F,)

#extraction for noun
#install.packages("stringr")
library(stringr)
user_werm <- data.frame(readLines("werm.txt"), "ncn")
doc2 <- paste(SimplePos22(df$content[1]))
doc3 <- str_match(doc2, "([가-힣]+)/NC")
doc3
?str_match
doc4 <- doc3[,2]
doc4
doc4[!is.na(doc4)]

NC <- function(doc){
  #doc <- as.character(doc)
  doc2 <- paste(SimplePos22(doc))
  doc3 <- str_match(doc2, "([가-힣]+)/NC")
  doc4 <- doc3[,2]
  doc4[!is.na(doc4)]
  return(doc4[!is.na(doc4)])
}

unique(unlist(strsplit(PosiNega(posi[1:100]), " ")))

P <- function(doc){
  #doc <- as.character(doc)
  doc2 <- paste(SimplePos22(doc))
  doc3 <- str_match(doc2, "([가-힣]+)/PV")
  doc4 <- doc3[,2]
  doc4[!is.na(doc4)]
  return(doc4[!is.na(doc4)])
}


PosiNega <- function(vec){
  ret <- c()
  i <- 0
  for(line in vec){
    print(paste(SimplePos22(line)))
    nc <- NC(line)
    print("NCCCCCC")
    print(nc)
    print("PPPPPPP")
    p <- P(line)
    print(p)
    ret <- c(ret, nc[str_length(nc) > 1], p[str_length(p) > 1])
    
    print("------------")
    i <- i + 1
  }
  return(ret)
}
a <- c("난", "가난 ", "배고", " ")


posi <- readLines("neg_pol_word.txt")

test <- unique(unlist(strsplit(PosiNega(posi[1:9826]), " ")))
class(PosiNega(str_split("나는 매우 가난하다 그런데 ..", " ")))
PosiNega("나는 매우 가난하다 그런데 ..")

# data1 <- df$content[1:10]
# data2 <- Map(extractNoun, txt1)
# data2
# rapply(data2, function(x) gsub())

#--------trash
#Quotes_Eliminator <- function(x){
#  return(substr(x,3,6))
#}
#df[2:nrow(df),1] = sapply(df[2:nrow(df),1],Quotes_Eliminator)
#df<-df[,2:(ncol(df)-1)]
#df
#colnames(df)<-c("search_time","class","negative_word","keyword1","keyword2","channel","source","source_url","title","content")

#con <- file('NamYang_Data_Table.csv',encoding="UTF-16")
#write.csv(df,file=con)
#write.xlsx(df,file="NY_Data_Refined.xlsx")
