data <- read.csv("/Users/user/Desktop/clean_data.csv")
head(data)
colnames(data)
class(data)

## delete '$'
for(i in c(1:9366)){
  data$Installs[i] <- sub(',', '', data$Installs[i])
  data$Installs[i] <- sub(',', '', data$Installs[i])
  data$Installs[i] <- sub(',', '', data$Installs[i])
  data$Installs[i] <- sub(',', '', data$Installs[i])
  if (nchar(data$Price[i])>1) {
    data$Price[i]<-sub('.', '', data$Price[i])
  } 
}

## convert data type
class(data$Price)
data$Rating <- as.double(data$Rating)
data$Size <- as.double(data$Size)
data$Installs <- as.double(data$Installs)
data$Category <- as.factor(data$Category)
class(data$Category)
data$Genres <- as.factor(data$Genres)
data$Content.Rating <- as.factor(data$Content.Rating)

## ----------

## Price
## size, CategoryFINANCE, CategoryLIFESTYLE,
model <- lm(Price~Rating+Reviews+Size+Installs+Category+Content.Rating , data=data)
summary(model)

## size
### Linear
model_size <- lm(Price~Size , data=data)
summary(model_size)
plot(data$Size, data$Price)

### Polynomial
data$Size2 <- data$Size^2
data$Size3 <- data$Size^3
model_size <- lm(Price~Size+Size2+Size3 , data=data)
summary(model_size)

## category (CategoryFINANCE, CategoryLIFESTYLE)
model_Category <- lm(Price~Category , data=data)
summary(model_Category)

## ----------

## Review
## rating, size, installs, categorySocial
model <- lm(Reviews~Rating+Price+Size+Installs+Category+Content.Rating , data=data)
summary(model)

## Rating
### Linear
model_rating <- lm(Reviews~Rating, data=data)
summary(model_rating)
plot(data$Rating, data$Reviews)

### Polynomial
data$Rating2 <- data$Rating^2
data$Rating3 <- data$Rating^3
model_rating <- lm(Reviews~Rating+Rating2+Rating3 , data=data)
summary(model_rating)

## Size
model_size <- lm(Reviews~Size, data=data)
summary(model_size)

## Installs
### Linear
model_installs <- lm(Reviews~Installs, data=data)
summary(model_installs)
plot(data$Installs, data$Reviews)

### Polynomial
data$Installs2 <- data$Installs^2
data$Installs3 <- data$Installs^3
model_installs <- lm(Reviews~Installs+Installs2+Installs3 , data=data)
summary(model_installs)
