#Load Libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

#Load Data
setwd("C:/Users/user/Desktop/Korea University/2022년 1학기/PAINS 2~3기/")
EPL <- read_excel("EPL 1-16라운드.xlsx")
EPL
attach(EPL)


# 1 W/D/L by difference of xG
nrow(EPL) #160
head(EPL[,11])
sum(EPL[,11]=="DRAW")   #42
(160-42)/2  #59

###Arrange top 59 as WIN, middle 42 as DRAW, bottom 59 as LOSE###

EPL.1 <- EPL %>% arrange(desc(EPL[,8]))
EPL.1[,12] <- c(rep("WIN", 59), rep("DRAW", 42), rep("LOSE", 59))
EPL.1
colnames(EPL.1)[12] <- c("기대득점 차이로 인한 결과")

EPL.1[c(59,60,101,102), 8]
sum(EPL.1[59:60, 8])/2   #0.85
sum(EPL.1[101:102, 8])/2 #-0.065

#IF P(W)=P(D)=P(L) = 1/3
c(160*1/3, 160*2/3)
sum(EPL.1[53:54, 8])/2   #0.885
sum(EPL.1[106:107, 8])/2 #-0.13




#Logistic Regression of Result~abs(xG)
colnames(EPL)[9:10] <- c("abs.exp.goal.diff", "result1")
attach(EPL)
EPL[,c(9, 10)]

L <- glm(result1~abs.exp.goal.diff, family="binomial")
summary(L)    
#p-value 0.00299 = abs(xG) has statistically significant relationship with Result

anova(L, test="Chisq")
ggplot(L)

dev.off()


#2 Classify Draw/Not Draw by difference of abs(xG)
EPL.2 <- EPL %>% arrange(desc(EPL[,9]))
EPL.2[,12] <- c(rep(0, 119), rep(0, 41))
tail(EPL.2[,9:12])

sum(EPL.2[118:119, 9])/2  #0.355 - critical value of draw

#IF P(W)=P(D)=P(L) = 1/3
sum(EPL.2[106:107, 9])/2  #0.53


# Observe the distribution
summary(EPL.2[,8])
class(EPL.2[,8])
A<-unlist(EPL.2[,8])
hist(A, breaks=15)
mean(A)  #0.30125
sd(A)    #1.432


(t.test(A, mu=0))   
#p-value 0.008 - population does not have mu=0

summary(EPL.2[,9])
B <- unlist(EPL.2[,9])
hist(B, breaks=25)
B

class(B)
class(B^2)
hist(B^2, breaks=15)


# Theoratical distribution of goals
Goals <- c(EPL[-c(127, 157),6:7]) # Drop postponed games
Goals <- unlist(Goals)
summary(Goals)
hist(Goals, breaks=seq(0, 10, by=1), right=FALSE, main="EPL Goals")
mean(Goals) #1.376582 from 316 variables
var(Goals)  #1.587894 - Poisson? (Since mean and var are similar)
Goals.1 <- as.vector(Goals)
Goals.1
table(Goals.1)

#Chi-squared test
classnum <-c(0:9)
obsum <-c(90,100,71,36,12,6,0,1,0,0)
lambda.1 <-mean(Goals)

hyp.prob.1 <- round(dpois(classnum, lambda.1),5);
hyp.prob.1[10] <- 1-sum(hyp.prob.1[1:9])
sum(hyp.prob.1)  # Check if it is 1 or not
hyp.prob.1

chisq.test(obsum, p=hyp.prob.1)
#p.value 0.3172 -> 유follows Poisson distribution with parameter 1.377 under significance level 5%

M <- matrix(rep(0, 225), 15, 15)
for (i in 1:15){
  for (j in 1:15){
    M[i,j] <- dpois(i-1, lambda.1)*dpois(j-1, lambda.1)
    j = j+1
  }
  i = i+1
}
M
sum(M)
sum(diag(M)) 
# If assume Poisson dist with parameter 1.377, theoretical probability of draw is 0.2553

round(160*0.2553) #Expect that about 41 games out of 160 will be draw


real.draw.e <- c(42, 160-42)
exp.draw.e <- c(sum(diag(M)), 1-sum(diag(M)))
chisq.test(real.draw.e, p=exp.draw.e)
#p-value 0.8341, Cannot reject the hypothesis that probability of draw is 0.2553 

#Can we assume P(Draw) = 1/3?
real.draw.e1 <- c(160*1/3, 160*2/3)
exp.draw.e <- c(sum(diag(M)), 1-sum(diag(M)))
chisq.test(real.draw.e1, p=exp.draw.e)
#p-value 0.02355, can reject that probability is 0.2553 under significance level 5%, so it is different with theoretical probability of draw


#Change parameters?
lambda.2 <-1.34  # 1.34 avg goals per team in Season 20/21
hyp.prob.2 <- round(dpois(classnum, lambda.2),6);
hyp.prob.2[10] <- 1-sum(hyp.prob.2[1:9])
sum(hyp.prob.2)  #1이 되는지 확인해야한다
hyp.prob.2

chisq.test(obsum, p=hyp.prob.2)
#p.value 0.2195 -> Follows Poisson dist with parameter 1.34 under significance level 5%

lambda.3 <-1.4  # User defined value
hyp.prob.3 <- round(dpois(classnum, lambda.3),6);
hyp.prob.3[10] <- 1-sum(hyp.prob.3[1:9])
sum(hyp.prob.3)  #Check 1 or not
hyp.prob.3

chisq.test(obsum, p=hyp.prob.3)
#p.value 0.3527 -> 유의수준 5%에서 모수가 1.4인 푸아송 분포를 따른다