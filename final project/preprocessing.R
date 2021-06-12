data <- read.csv("/Users/user/Desktop/googleplaystore.csv")

## change format 
## size M => 10^6, k => 10^3
char_M <- "M"
char_k <- "k"
count_M = 0
count_k = 0
for(i in c(1:10840)){
  if (grepl(char_M, data$Size[i], fixed = TRUE)){
    new <- gsub("M", "", data$Size[i])
    data$Size[i] <- as.double(new) * 1000000
    count_M = count_M+1
  } else if (grepl(char_k, data$Size[i], fixed = TRUE)){
    new <- gsub("k", "", data$Size[i])
    data$Size[i] <- as.double(new) * 1000 
    count_k = count_k+1
  }
}
class(data$Size)

## preprocessing
#data <- read.csv("/Users/user/Desktop/data.csv")
dim(data) ## 10840x16
summary(data)
head(data)

## if size == "Varies with Device" => size = mean(size)
count = 0
for(i in c(1:10840)){
  if (data$Size[i] == "Varies with device"){
    data$Size[i] <- NA
  }
  else{
    data$Size[i] <- as.integer(data$Size[i])
    count = count + 1
  }
}
mean <- mean(as.integer(data$Size),na.rm = TRUE)
for(i in c(1:10840)){
  if (is.na(data$Size[i])) 
    data$Size[i] <- as.integer(mean)
}

## rating == NA => delete the row
count = 0
for(i in c(1:10840)){
  if (data$Rating[i] == "") 
    data$Rating[i] = NA
  else
    count = count + 1
}
data <- na.omit(data)

## check
dim(data)
write.csv(data,file="/Users/user/Desktop/clean_data.csv",row.names = FALSE)

## EDA
# install.packages("SparkR")
library("SparkR")
status(data)
# install.packages("psych")
library(psych)
describe(data)

## language
install.packages("textcat")
library("textcat")
lang <- textcat(data$App)
unique(lang)
count(sum(lang))
