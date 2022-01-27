install.packages('kernlab')
library('kernlab')
library(caret)
set.seed(123)
df=print(spam)

#find correlation
library(ggcorrplot)
corr<-cor(df)
ggcorrplot(corr)
index <- findCorrelation(abs(corr), 0.50,exact=FALSE)
to_be_removed <- colnames(corr)[index]
df=df[!names(df) %in% to_be_removed]
ncol(df)
df
corr<-cor(df$type,df)
ggcorrplot(corr)
summary(df)
#remove Outliers
dum=df
dum <- subset(dum, subset = !(capitalTotal %in% boxplot.stats(capitalTotal)$out))
dum=dum[-boxplot.stats(dum$capitalAve)$out, ]
dum=dum[-boxplot.stats(dum$capitalTotal)$out, ]
dum=dum[-boxplot.stats(dum$lab)$out, ]
dum=dum[-boxplot.stats(dum$credit)$out, ]
dum=dum[-boxplot.stats(dum$data)$out, ]
colnames(dum)
summary(dum)
df=dum

#splitting Data
smp_size <- floor(0.80 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
colnames(df)
train_df <- df[train_ind, ]
train_df
test_df <- df[-train_ind, ]
test_df
require(dplyr)
df <- df%>%
  mutate(type = ifelse(type == "spam",1,0))
df
log_reg=glm(data=test_df,formula=type~.,family = binomial)
log_reg
ggplot(data=df,aes(x=credit,y=type))+stat_smooth(method='glm',method.args = list(family=binomial),
                                                   color='red',lwd=2)
table(spam$type)#to check type of values in spam

#using boruta for feature selection
install.packages('randomForest')
library(Boruta)
library(mlbench)
library(randomForest)
boruta <- Boruta(x = df[,1:49],y = df[,50], doTrace = 1, maxRuns = 500)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
