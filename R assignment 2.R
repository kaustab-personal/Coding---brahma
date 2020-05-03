library(tidyverse)
library(readxl)
library(dplyr)
library(data.table)
library(lsr)
Placement <- read_excel("C:/Users/kaustab pc/Desktop/Placement.xlsx")
View(Placement)
names(Placement)[names(Placement)%in%c("hsc_p","etest_p")]=c('H_P','E_P')
mean(Placement$degree_p)
median(Placement$degree_p)
var(Placement$degree_p)
sd(Placement$degree_p)
quantile(Placement$degree_p, .25)
quantile(Placement$degree_p, .75)
IQR(Placement$degree_p)
mad(Placement$degree_p)
sort(Placement$degree_p)
stem(Placement$degree_p)
cut_number(Placement$degree_p, n=5)
table(cut_number(Placement$degree_p, n=5))
barplot(table(Placement$degree_p))
boxplot(Placement$degree_p)
hist(Placement$degree_p, probability = TRUE)
ggplot(Placement)+geom_point(mapping = aes(x=ssc_p, y= H_P))
ggplot(Placement)+geom_point(mapping = aes(x=ssc_p, y= mba_p))
ggplot(Placement)+geom_smooth(mapping = aes(x=ssc_p, y= H_P))
ggplot(Placement)+geom_bar(mapping = aes(x=workex))
ggplot(Placement)+geom_bar(mapping= aes(x=hsc_s))
ggplot(Placement)+geom_bar(mapping= aes(x=degree_t))
ggplot(Placement)+geom_histogram(mapping = aes(mba_p), binwidth = .5)
ggplot(Placement)+geom_bin2d(mapping = aes(x=mba_p, y=degree_p))
P1 <- select(Placement, H_P, ssc_p)
P2 <- select(Placement, degree_p, E_P)
P3 <- select(Placement, workex, status)
as_tibble(P1)
as_tibble(P2)
as_tibble(P3)
#  All the graphs are at Significance level = 0.05 at 95% confidence
# Hypothesis testing using t test where Null hypothesis = Students with lower ssc percentage will get lower hsc percentage
t.test( P1$H_P,P1$ssc_p) # here we will compare the p-value with alpha which is 0.05.
# Hypothesis testing using cor test where Null hypothesis = Candidate who have good percentage in their college degrees will get good percentage in employment test also. 
cor.test(P2$degree_p,P2$E_P) # here also we will compare the p value with alpha which is equal to 0.05.
# Hypothesis testing using chi square for categorical variable. Here Null Hypothesis = candidates with early work experience have higher chances getting selected during placements.                                                                                                                                                                                                                                                                                
chisq.test(P3$workex,P3$status) # here also we will compare the p-value with alpha.
# Linear Regression Model
Placement <- as_tibble(Placement)
view(Placement)
ggplot(Placement)+geom_point(mapping = aes(x=H_P,y=E_P))
cor(Placement$H_P,Placement$E_P)
linear_mod <- lm(data=Placement,E_P~H_P)
print(linear_mod)
ggplot(Placement)+geom_point(mapping = aes(x=H_P,y=E_P))+geom_abline(slope = 0.2986,intercept = 52.2928)
ggplot(Placement,mapping = aes(H_P,E_P))+geom_point(mapping = aes(x=H_P,y=E_P))+geom_smooth(method = "lm",se=FALSE)
model_summary <- summary(linear_mod)
print(model_summary)
sigma(linear_mod)/mean(Placement$E_P)
set.seed(100)
sample <- sample(c(TRUE, FALSE), nrow(Placement), replace=T, prob=c(0.7,0.3))
train <- Placement[sample,]
test <- Placement[!sample,]
sample
!sample
pmodel <- lm(data=train,E_P~H_P)
print(pmodel)
ggplot(train,mapping = aes(H_P,E_P))+
  geom_point()+
  geom_abline(slope = 0.2856,intercept = 52.9273,color="blue")+
  geom_abline(slope=0.2986,intercept = 52.2928,color="red")
prediction <- predict(pmodel, test)
avsp <- data.frame(cbind(actuals=test$E_P, predicted=prediction))
avsp
avsp <- avsp%>% mutate(predError = actuals-predicted)  
avspT <- as_tibble(avsp)
ggplot(data=avspT)+geom_point(mapping = aes(x=actuals, y= predError))
# KNN Algorithm
P <- select(Placement,gender,ssc_b,hsc_b,degree_t,workex,specialisation,status,ssc_p,H_P,degree_p,E_P,mba_p)
view(P)
head(P)
ran <- sample(1:nrow(P), 0.9*nrow(P))
nor <- function(x){(x-min(x))/(max(x)-min(x))}
P_norm <- as.data.frame(lapply(P[c(8,9,10,11,12)], nor))
P_norm
summary(P_norm)
P_train <- P_norm[ran,]
P_test <- P_norm[-ran,]
P_target_category <- P[ran, 7]
P_test_category <- P[-ran, 7]
library(class)
cl = P_target_category[,1, drop = TRUE]
Pr <- knn(P_train,P_test,cl,k=15)
tab <- table(Pr,P_test_category[,1, drop=TRUE])
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tab)