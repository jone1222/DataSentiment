library(readxl)
setwd("/Users/theorist/Documents/R-WorkingDirectory/DataSentiment")
df <- read_excel("NamYang_Data_Refined.xlsx",col_names=T,na="NA")

library(KoNLP)

dics <- c('woorimalsam')
category <- c('emotions')
user_d <- data.frame(readLines("fillmeup.txt"), "ncn")

buildDictionary(ext_dic = dics,
                category_dic_nms = category, 
                user_dic = user_d,
                replace_usr_dic = F,)

df$content[1]
print(extractNoun(df$content[1]))

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
