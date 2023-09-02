#Load Libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)

#분데스도 같은 방법으로 적용
setwd("C:/Users/user/Desktop/Korea University/2022년 1학기/PAINS 2~3기/")
Bundes <- read.csv("Bundesliga.csv")
Bundes <- na.omit(Bundes)
attach(Bundes)
tail(Bundes,4)

colnames(Bundes)[9:10] <- c("abs.exp.goal.diff.b", "result2")
attach(Bundes)
G <- glm(result2~abs.exp.goal.diff.b, family="binomial")
summary(G)
#p-value 0.0312, by Logistic Regression, E(abs(xG)) has statistically significant relationship with the game result

nrow(Bundes) #153
head(Bundes[,11])
sum(Bundes[,11]=="DRAW")   #36
(153-36)  #117

Bundes.1 <- Bundes %>% arrange(desc(Bundes[,9]))
Bundes.1[,12] <- c(rep(1, 117), rep(0, 36))
tail(Bundes.1[,9:12])
tail(Bundes.1)
colnames(Bundes.1)[12] <- c("기대득점 차이로 인한 승패여부")

sum(Bundes.1[117:118, 9])/2   #0.35


# Distribution of goals in Bundesliga
summary(Bundes.1[,8])
class(Bundes.1[,8])
A.b<-unlist(Bundes.1[,8])
hist(A.b, breaks=15)
mean(A.b)  #0.30125
sd(A.b)    #1.432

(t.test(A.b, mu=0))   
#p-value 0.001, The population mean is not zero

# Theoretical distribution of goals in Bundesliga?
Goals.b <- c(Bundes[,6:7])
Goals.b <- unlist(Goals.b)
summary(Goals.b)
hist(Goals.b, breaks=seq(0, 10, by=1), right=FALSE, main="Goals of Bundesliga")
mean(Goals.b) #1.545752 from 316 variables
var(Goals.b)  #1.94708
Goals.b1 <- as.vector(Goals.b)
Goals.b1
table(Goals.b1)
반올림

#Chi-squared test of Bundesliga
classnum.b <-c(0:9)
obsum.b <-c(70,109,67,30,17,8,3,2,0,0)
lambda.b <-mean(Goals.b)

hyp.prob.b <- round(dpois(classnum.b, lambda.b),5);
hyp.prob.b[10] <- 1-sum(hyp.prob.b[1:9])
sum(hyp.prob.b)  #check if 1, if over due to rounding, increase the number of decimal point
hyp.prob.b

chisq.test(obsum.b, p=hyp.prob.b)
#p.value가 0.01294 -> Under significance level 5%, not Poisson. Under 1%, Poisson dist with parameter 1.5458

#Change paramters?
928/(306*2)
lambda.b2 <-1.516  #From avg goals per team in 20/21 season = 1.516 goals
hyp.prob.b2 <- round(dpois(classnum.b, lambda.b2),6);
hyp.prob.b2[10] <- 1-sum(hyp.prob.b2[1:9])
sum(hyp.prob.b2)  #Check if 1
hyp.prob.b2

chisq.test(obsum.b, p=hyp.prob.b2)
#p.value 0.0066 -> Does not follow Poisson dist of parameter 1.516 under significance level 5%

(mean(Goals.b)+var(Goals.b))/2
lambda.b3 <-1.7464  #(mean+var)/2, because Poisson dist have same mean and variance
hyp.prob.b3 <- round(dpois(classnum.b, lambda.b3),6);
hyp.prob.b3[10] <- 1-sum(hyp.prob.b3[1:9])
sum(hyp.prob.b3)  #Check if 1
hyp.prob.b3

chisq.test(obsum.b, p=hyp.prob.b3)
#p.value 0.0083 -> Does not follow Poisson dist with paramter 1.7464 under significance level 1%


M.b <- matrix(rep(0, 225), 15, 15)
for (i in 1:15){
  for (j in 1:15){
    M.b[i,j] <- dpois(i-1, lambda.b)*dpois(j-1, lambda.b)
    j = j+1
  }
  i = i+1
}
sum(M.b)
sum(diag(M.b)) 
#Theoretical prob of Draw = 0.2389 under Poisson dist with paramter 1.5458

round(153*0.2389) #Expect that 37 games will be draw out of 153 games


real.draw.b <- c(37, 153-42)
exp.draw.b <- c(sum(diag(M.b)), 1-sum(diag(M.b)))
chisq.test(real.draw.b, p=exp.draw.b)
#p-value 0.7511, cannot reject the hypothesis that theoretical prob of draw is 1.5458