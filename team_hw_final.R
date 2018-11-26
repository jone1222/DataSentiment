setwd("C:/Users/ok3651004/Desktop/데이터마이닝")
library(readxl)
df <- read_excel("NamYang_Data_Refined.xlsx",col_names=T,na="NA")

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_144')
install.packages("rJava")
library(rJava)
install.packages("KoNLP")
library(KoNLP)

# 명사 추출 재료 
install.packages("stringr")
library(stringr)

# 명사 추출 함수 
NC <- function(doc){
  doc2 <- paste(SimplePos22(doc))
  doc3 <- str_match(doc2, "([가-힣]+)/NC")
  doc4 <- doc3[,2]
  doc4[!is.na(doc4)]
  return(doc4[!is.na(doc4)])
}

# 제일 긴 친구에 맞춰 na 포함 cbind 
combine.df <- function(x, y) {
  rows.x <- nrow(x)
  rows.y <- nrow(y)
  if (rows.x > rows.y) {
    diff <- rows.x - rows.y
    df.na <- matrix(NA, diff, ncol(y))
    colnames(df.na) <- colnames(y)
    cbind(x, rbind(y, df.na))
  } else {
    diff <- rows.y - rows.x
    df.na <- matrix(NA, diff, ncol(x))
    colnames(df.na) <- colnames(x)
    cbind(rbind(x, df.na), y)
  }
}

# RAW Data 만들기 => 아예 단어가 없는 content는 pass
# n : 1줄부터 n줄까지 돌리기 
MK_RAW <- function(n){
  new_n <- data.frame(content = c(1, NC(df$content[1])))
  for (i in 2:n) {
    if(is.na(df$content[i])){
      next;
    }
    plus_data <- data.frame(content = c(i, NC(df$content[i])))
    new_n <- combine.df(new_n, plus_data)
  }
  new_n <- t(new_n)
  return(new_n)
}

new_n <- MK_RAW(1500)
View(new_n)

# nwd <- c(1, NC(df$content[1]))
# wd_list <- paste(nwd, collapse =',')
# wd_list2 <- paste(df$content[2], collapse=',')
# wd_list <- rbind(wd_list, wd_list2)
# wd_list

# sum(is.na(df$content))
# NC(df$content[4500])
nwd <- c(1, NC(df$content[1]))
wd_list_add <- paste(nwd, collapse = ',')
wd_list_add <- c(wd_list_add, "불량")
wd_list_add < t(wd_list_add)
wd_list_add
wd <- c("불량")
wd <- c(wd, "갑질")
wd
wdwd <- paste(wd, collapse = ',')
wdwd
wd_list_add <- c(wd_list_add, wdwd)
wd_list_add
bad_keyword <- c()
bad_keyword

# 1 ~ n행까지 데이터셋 만들기
# flag => 0: 전체, 1: 부정 데이터 생성 
MK_wd_list <- function(n, flag){
  # 초기화 
  nwd <- c(1, NC(df$content[1]))
  wd_list <- paste(nwd, collapse =',')
  bad_wd_list <- cbind(wd_list, "부정")
  
  # 부정어
  bad_wd <- c("불매", "갑질", "사태", "불량", "소송", "불공정", "이물질", "논란", "구설수", "벌레", "과징금")
  
  for(i in 2:n){
    # na인건 패스 
    if(is.na(df$content[i])){
      next;
    }
    
    # 추가할 line 
    nwd_add <- c(i, NC(df$content[i]))
    wd_list_add <- paste(nwd_add, collapse = ',')
    
    #### 부정
    if(flag == 1){  
      bad_keyword <- c() # 이 게시물에 포함된 부정어 리스트 
      for(k in 1:length(nwd_add)){
        for(j in 1:length(bad_wd)){
          if(nwd_add[k] == bad_wd[j]){ # 부정 단어가 포함돼 있다면 
            bad_keyword <- c(bad_keyword, bad_wd[j])
          }
        }
      }
      if(is.null(bad_keyword)){ # 부정단어 안나왔으면 통과
        next; 
      } else{ # 나왔으면 걸렸던 부정단어들과 함께 저장
        bad_keywords <- paste(bad_keyword, collapse = ',')
        bad_wd_list_add <- cbind(wd_list_add, bad_keywords)
        bad_wd_list <- rbind(bad_wd_list, bad_wd_list_add)
      }
      
    } 
    #### 전체
    else if (flag == 0){
      wd_list <- rbind(wd_list, wd_list_add)
    }
  }
  
  if(flag == 0){
    return(wd_list)
  } else if(flag == 1){  
    return(bad_wd_list)
  } 
  
}

# 1500개의 전체 데이터 
raw_data <- MK_wd_list(1500, 0)
View(raw_data)
# 4500개 중에 부정 데이터 추출 
meta_data <- MK_wd_list(1500, 1)
View(meta_data)


####################################

unique(unlist(strsplit(PosiNega(posi[1:100]), " ")))

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


posi <- readLines("pos_neg_word.txt")

test <- unique(unlist(strsplit(PosiNega(posi[1:9826]), " ")))
class(PosiNega(str_split("나는 매우 가난하다 그런데 ..", " ")))
PosiNega("나는 매우 가난하다 그런데 ..")

