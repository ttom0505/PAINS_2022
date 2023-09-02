"
Score ~ 공격지표 단순회귀
Score ~ 
"
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)

Brazil <- read_excel("2014 WC.xlsx")
head(Brazil)
Russia <- read_excel("2018 WC.xlsx")
head(Russia)

#2018 월드컵
Y.18 <- Russia$Score

#평균득점
X.18.avg.g <- Russia$평균득점
plot(X.18.avg.g, Y.18)  #2차함수?
model.18.1.2 <- lm(Y.18~poly(X.18.avg.g, 2))
model.18.1.1 <- lm(Y.18~X.18.avg.g)
summary(model.18.1.2) #0.627
summary(model.18.1.1) #0.5881
abline(model.18.1.1)

ggplot(Russia, aes(x=평균득점, y=Score))+geom_point() + geom_smooth(method=lm)
ggplot(Russia, aes(x=평균득점, y=Score))+geom_point() + geom_smooth(aes(x=평균득점, y=predict(model.18.1.2, newdata=Russia)), col="red")

#평균기대득점
X.18.avg.xg <- Russia$`경기당 기대득점`
plot(X.18.avg.xg, Y.18)
model.18.2.2 <- lm(Y.18~poly(X.18.avg.xg, 2))
model.18.2.1 <- lm(Y.18~X.18.avg.xg)
summary(model.18.2.2) #0.155
summary(model.18.2.1) #0.1113

ggplot(Russia, aes(x=`경기당 기대득점`, y=Score))+geom_point() + geom_smooth(method=lm)
ggplot(Russia, aes(x=`경기당 기대득점`, y=Score))+geom_point() + geom_smooth(aes(x=`경기당 기대득점`, y=predict(model.18.2.2, newdata=Russia)), col="red")


#슈팅수
X.18.avg.s <- Russia$`경기당 슈팅수`
plot(X.18.avg.s, Y.18)
model.18.3.2 <- lm(Y.18~poly(X.18.avg.s, 2))
model.18.3.1 <- lm(Y.18~X.18.avg.s)
summary(model.18.3.2)
summary(model.18.3.1) #0.05734
abline(model.18.3.1, col="red")


#AIC
install.packages("MASS")
library(MASS)
model.18.AIC <- lm(Y.18~poly(X.18.avg.g, 2)+X.18.avg.xg+X.18.avg.s)
stepAIC(model.18.AIC, direction="backward", scope=Y.18~1)
stepAIC(model.18.1.1, direction="backward", scope=Y.18~1)

#공격 : AIC 29.755, 결정계수 0.627

#평균실점
X.18.avg.l <- Russia$평균실점
plot(X.18.avg.l, Y.18)  #분수함수?
model.18.4.1 <- lm(Y.18~X.18.avg.l)
summary(model.18.4.1) #0.1649

par(mfrow=c(1,1))
X.18.avg.l.inverse <- 1/X.18.avg.l
plot(X.18.avg.l.inverse, Y.18)
model.18.4.2 <- lm(Y.18~X.18.avg.l.inverse)
summary(model.18.4.2) #0.1084

ggplot(Russia, aes(x=평균실점, y=Score))+geom_point() + geom_smooth(method=lm)
ggplot(Russia, aes(x=평균실점, y=Score))+geom_point() + geom_smooth(aes(x=평균실점, y=predict(model.18.4.2, newdata=Russia), col="red"))


#기대실점
X.18.avg.xl <- Russia$`경기당 기대실점`
plot(X.18.avg.xl, Y.18)  
model.18.5.1 <- lm(Y.18~X.18.avg.xl)
summary(model.18.5.1) #0.1742

X.18.avg.xl.inverse <- 1/X.18.avg.xl
plot(X.18.avg.xl.inverse, Y.18)
model.18.5.2 <- lm(Y.18~X.18.avg.xl.inverse)
summary(model.18.5.2) #0.1737

ggplot(Russia, aes(x=`경기당 기대실점`, y=Score))+geom_point() + geom_smooth(method=lm)
ggplot(Russia, aes(x=`경기당 기대실점`, y=Score))+geom_point() + geom_smooth(aes(x=`경기당 기대실점`, y=predict(model.18.5.2, newdata=Russia), col="red"))

#수비 AIC
model.18.l.AIC <- lm(Y.18~X.18.avg.l+X.18.avg.l.inverse+X.18.avg.xl+X.18.avg.xl.inverse)
stepAIC(model.18.l.AIC, direction="backward", scope=Y.18~1)

model.18.l.best <- lm(Y.18~X.18.avg.l+X.18.avg.xl.inverse)
summary(model.18.l.best)
#AIC : 52.834, 결정계수: 0.2327 -> 18월드컵 공격 중요

#왜 공격 중요? 조별리그 탈락한 16팀중 2팀 제외하고 3득점 이하, 4강 팀들은 모두 10득점 이상인 대신 실점도 많다.

#2014월드컵
Y.14 <- Brazil$Score
par(mfrow=c(1,1))

#평균득점
X.14.avg.g <- Brazil$`경기당 득점`
plot(X.14.avg.g, Y.14)  #2차함수?
model.14.1.2 <- lm(Y.14~poly(X.14.avg.g, 2))
model.14.1.1 <- lm(Y.14~X.14.avg.g)
summary(model.14.1.2) #0.4148
summary(model.14.1.1) #0.3813
abline(model.18.1.1)

ggplot(Brazil, aes(x=`경기당 득점`, y=Score))+geom_point() + geom_smooth(method=lm)
ggplot(Brazil, aes(x=`경기당 득점`, y=Score))+geom_point() + geom_smooth(aes(x=`경기당 득점`, y=predict(model.14.1.2, newdata=Brazil)), col="red")


#슈팅
X.14.avg.s <- Brazil$`경기당 슈팅 수`
plot(X.14.avg.s, Y.14)  #2차함수?
model.14.2.1 <- lm(Y.14~X.14.avg.s)
summary(model.14.2.1)
abline(model.14.2.1)

ggplot(Brazil, aes(x=`경기당 슈팅 수`, y=Score))+geom_point() + geom_smooth(method=lm)

#공격 AIC
model.14.AIC <- lm(Y.14~poly(X.14.avg.g, 2)+X.14.avg.s)
stepAIC(model.14.AIC, direction="backward", scope=Y.14~1)

#AIC : 45.342, 결정계수 : 0.4148


#평균실점
X.14.avg.l <- Brazil$`경기당 실점`
plot(X.14.avg.l, Y.14)

model.14.3.1 <- lm(Y.14~X.14.avg.l)
summary(model.14.3.1)

X.14.avg.l.inverse <- 1/X.14.avg.l
plot(X.14.avg.l.inverse, Y.14)
model.14.3.2 <- lm(Y.14~X.14.avg.l.inverse)
summary(model.14.3.2)

ggplot(Brazil, aes(x=`경기당 실점`, y=Score))+geom_point() + geom_smooth(method=lm)
ggplot(Brazil, aes(x=`경기당 실점`, y=Score))+geom_point() + geom_smooth(aes(x=`경기당 실점`, y=predict(model.14.3.2, newdata=Brazil)), col="red")

#평균 슈팅허용
X.14.avg.sa <- Brazil$`경기당 슈팅 허용`
plot(X.14.avg.sa, Y.14)

model.14.4.1 <- lm(Y.14~X.14.avg.sa)
summary(model.14.4.1) #0.04766

ggplot(Brazil, aes(x=`경기당 슈팅 허용`, y=Score))+geom_point() + geom_smooth(aes(x=`경기당 슈팅 허용`, y=predict(model.14.4.1, newdata=Brazil)), col="blue")

X.14.avg.sa.inverse <- 1/X.14.avg.sa
plot(X.14.avg.sa.inverse, Y.14)
model.14.4.2 <- lm(Y.14~X.14.avg.sa.inverse)
summary(model.14.4.2)

ggplot(Brazil, aes(x=`경기당 슈팅 허용`, y=Score))+geom_point() + geom_smooth(aes(x=`경기당 슈팅 허용`, y=predict(model.14.4.2, newdata=Brazil)), col="red")

#AIC
model.14.l.AIC <- lm(Y.14~X.14.avg.l+X.14.avg.l.inverse+X.14.avg.sa+X.14.avg.sa.inverse)
stepAIC(model.14.l.AIC, direction="backward", scope=Y.14~1)

#AIC : 39.091, 0.4876

#왜 수비 중요? 스페인, 포르투갈, 대한민국 등 조별리그에서 실점 많이 한 팀이 모두 탈락, 브라질 제외 8강 진출한 팀은 5~7경기에서 4실점 이하


