## preprocessing
data <- read.csv("/Users/user/Desktop/data.csv")
dim(data) ## 10840x16
summary(data)
head(data)

## if size_clean == Varies with Device => size_clean = mean(size_clean)
count = 0
for(i in c(1:10840)){
  if (data$Size_clean[i] == "Varies with device") 
    data$Size_clean[i] = NA
  else{
    data$Size_clean[i] <- as.double(data$Size_clean[i])
    count = count + 1
  }
}
mean <- mean(as.double(data$Size_clean),na.rm = TRUE)
for(i in c(1:10840)){
  if (is.na(data$Size_clean[i])) 
    data$Size_clean[i] <- mean
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
