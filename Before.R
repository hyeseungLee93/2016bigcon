# 패키지설치
install.packages("sqldf") install.packages("glmnet") install.packages("randomForest") install.packages("caret") install.packages("earth") install.packages("RWeka")

# 패키지로드
libs <- c("sqldf", "glmnet", "randomForest", "caret", "earth", "RWeka")
unlist(lapply(libs, require, character.only = TRUE ))
#성공적으로 로드된 경우 TRUE반환

# 데이터불러오기 - 17일차 관객수데이터: movie17.csv
adnc17data<-read.csv("D:/bigcon/movie17.csv") # <-파일위치지정!!
str(adnc17data)

# 연도, 월, 등급, 국가 변수 팩터형으로 변환
adnc17data[ ,1]<-as.data.frame(as.factor(adnc17data[ ,1]))
adnc17data[ ,2]<-as.data.frame(as.factor(adnc17data[ ,2]))
adnc17data[ ,3]<-as.data.frame(as.factor(adnc17data[ ,3]))
adnc17data[ ,4]<-as.data.frame(as.factor(adnc17data[ ,4]))
str(adnc17data)

#################################################
##################변수탐색#######################
#################################################

##개봉년도
table(adnc17data$year)
plot(adnc17data$year,adnc17data$audience17)

##상영월
table(adnc17data$month)
plot(adnc17data$month,adnc17data$audience17)
month_mean <- sqldf("select month,avg(audience17) from adnc17data group by month order by 2")
month_mean

##상영등급
table(adnc17data$age)
plot(adnc17data$age,adnc17data$audience17)
age_mean <- sqldf("select avg(audience17) from adnc17data group by age")
age_mean

##국적
table(adnc17data$country)
plot(adnc17data$country,adnc17data$audience17)

##17일간 휴일수
table(adnc17data$holiday17)
plot(adnc17data$holiday17,adnc17data$audience17)
holi_mean <- sqldf("select holiday17,sum(audience17),count(audience17),avg(audience17) from adnc17data group by holiday17 order by 1")
holi10 <- sqldf("select * from adnc17data where holiday17=11")
### 휴일수가 10이상인 경우 데이터가 1,2개로 부족해 신뢰성이 떨어진다.

## 개봉당일 스크린수
plot(adnc17data$screen,adnc17data$audience17)
## 위에서 스크린수 10 미만인 경우 제외
scr<- sqldf("select * from adnc17data where screen>10") 
nrow(scr)
plot(scr$screen, scr$audience17)
### 개봉당일 스크린수가 10 미만인 경우(대체로 재개봉영화)를 제외하면 약간의 선형성이 보임


##배급사파워
plot(adnc17data$company,adnc17data$audience17)
com<-sqldf("select company, round(avg(audience17)) avg_audi from adnc17data group by company")
plot(com$company, com$avg_audi)
summary(com$avg_audi)
cor(com$company, com$avg_audi)
###배급사파워별 평균 17일 누적관객수가 약간의 선형성을 보이며 correlation이 0.7로 높은 수치


##장르점유율
table(adnc17data$genre)
plot(adnc17data$genre,adnc17data$audience17)
genre_mean <- sqldf("select genre,round(avg(audience17)) from adnc17data group by genre order by 2")
genre1<- sqldf("select * from adnc17data where genre=1")

##감독파워
plot(adnc17data$director,adnc17data$audience17)
cor(adnc17data$director, adnc17data$audience17)
###감독파워가 작은부분을 제외하고 선형성이 있어보임-> 감독파워 산정 시 기간설정에 의한것으로 보임
###correlation이 0.5를 넘어 어느정도의 상관성이 있다고 할수있다
hist(sqrt(adnc17data$director))

# 스타파워: 주,조연배우의 영향력을 수치화
#          (해당 영화 개봉 전까지 주,조연으로 출연한 영화들의 관객수 합)

##스타파워1 
plot(adnc17data$star1,adnc17data$audience17)
cor(adnc17data$star1, adnc17data$audience17)
starmax<- sqldf("select star1,audience17 from adnc17data where star1>1000000 order by 1 desc")
s<- sqldf("select * from adnc17data where star1= 12811213")
summary(log(adnc17data$star1))

##스타파워2
plot(adnc17data$star2,adnc17data$audience17)
cor(adnc17data$star2, adnc17data$audience17)
summary(log(adnc17data$star2))

##스타파워3
plot(adnc17data$star3,adnc17data$audience17)
cor(adnc17data$star3, adnc17data$audience17)
summary(log(adnc17data$star3))

##토탈스타파워: 배우 셋의 영향력의 합
plot(adnc17data$totalstar,adnc17data$audience17)
cor(adnc17data$totalstar, adnc17data$audience17)
###스타파워에 따른 분포들은 대체로 비슷, 1->3으로 갈수록 상관성은 떨어지는 것을 알수있음
###토탈스타파워 역시 분포는 비슷해보이며, correlation이 0.4를 넘어 양의 상관성이 있다
summary(log(adnc17data$totalstar))
###스타파워 관련된 4개의 변수 모두 1의 값을 갖는 데이터의 갯수가 크며 그 이외의 값과 큰 차이를 보인다
###4개 변수 모두 왼쪽으로 크게 치우친 분포를 보이므로 변수변환을 한다.


## 0일차 관객수 = 개봉 첫 날 누적 관객 수 - 당일 관객 수 
## (공식적인 개봉일 이전에 기록된 관객수를 의미. 선개봉, 유료시사회 관객수를 포함한다.)
plot(adnc17data$day0a,adnc17data$audience17)
cor(adnc17data$day0a, adnc17data$audience17)
summary(adnc17data$day0a)
dy0 <- sqldf("select day0a,audience17 from adnc17data where day0a<500000")
nrow(dy0)
cor(dy0[,1], dy0[,2])
###전체데이터에서 상관계수가 0.79로 매우크다 하지만 재개봉 영화로 의심되는 데이터를 제거 후 구했을때는
###0.049로 매우 낮은 수치를 보인다.


##개봉전 평점 (데이터 출처: 네이버 영화 평점 - '개봉 전 평점')
plot(adnc17data$pregrade,adnc17data$audience17)
cor(adnc17data$pregrade, adnc17data$audience17)
### 개봉전 평점과 관객수의 상관성은 -0.08로 매우 낮은 음의 상관성을 보였는데,
### 개봉전 평점이 0인 경우는 투표 참여자가 없는 경우이다.
###따라서 사람의 평가가 이루어진(pregrade>0) 데이터만 뽑아내보았다.
pg<- sqldf("select * from adnc17data where pregrade>0")
plot(pg$pregrade, pg$audience17)
cor(pg$pregrade, pg$audience17)
###그럼에도 불구하고 상관성은 0.04로 매우 낮은 상관성을 보였다.
### --> '참여자수' 변수와 곱하여 '총 평점' 변수를 생성하였다.

##개봉전 평점 참여자수
plot(adnc17data$preparti,adnc17data$audience17)
cor(adnc17data$preparti, adnc17data$audience17)
precnt<- sqldf("select * from adnc17data where preparti>0 and preparti<5000")
plot(precnt$preparti, precnt$audience17)
cor(precnt$preparti, precnt$audience17)
###평점참여자수 역시 상관성이 낮으나 참여자수가 있고 이상치로 보이는 5000이상의 자료를 제거 후
###correlation이 0.5를 넘는 수치를 보여준다

### -개봉전 평점 참여자수 5000명 이상인 데이터 3개- (데이터 확인용)
precnt2<-sqldf("select * from adnc17data where preparti>5000")
table(precnt2$preparti, precnt2$year)




###################################################
###############데이터 분할#########################
###################################################
####총 평점 변수추가
adnc17data[ ,20]<-adnc17data[ ,"pregrade"]*adnc17data[ ,"preparti"]
names(adnc17data)[20]<-"SumGrade"

#훈련데이터 90% 테스트데이터 10%
set.seed(800)
index<-sample(nrow(adnc17data),size=floor(nrow(adnc17data)*0.9))

adnc17data.tr <-adnc17data[index, ]
adnc17data.te <-adnc17data[-index, ]

# RMSE 함수 생성
RMSE<-function(x,y){
  sqrt(mean((x-y)^2))
}

###################################################
###############lasso모델링#########################
###################################################

k<- data.matrix(adnc17data.tr[,-19])
t<- as.vector(adnc17data.tr$audience17)

lar.fit<- glmnet(k,t,alpha=1, family="gaussian",standardize = TRUE)
lar.fit
summary(lar.fit)
set.seed(800)
cv.lfit<- cv.glmnet(k,t,alpha=1, family="gaussian",standardize = TRUE)
plot(cv.lfit)
bestlam <- cv.lfit$lambda.min


lar.fit<- glmnet(k,t,alpha=1, family="gaussian", lambda=bestlam,standardize = TRUE)
predict(lar.fit,s=bestlam, type="coefficients")
newx<- data.matrix(adnc17data.te[,-19])
pre<- predict(lar.fit, newx, s=bestlam)
pre

error_lars<-RMSE(pre,adnc17data.te$audience17) #RMSE: 493836.2
comp<- cbind(round(pre),adnc17data.te$audience17)


###################################################
############모델트리 모델링########################
###################################################
set.seed(800) ##error: Error in .jcall(o, "Ljava/lang/Class;", "getClass") : java.lang.NoClassDefFoundError: no/uib/cipr/matrix/Matrix
m.m5p<-M5P(audience17 ~ age+country+holiday17+screen+company+genre+director+star1+star2+star3+day0a+SumGrade, data=adnc17data.tr, control=Weka_control(N=TRUE))
p.m5p <-predict(m.m5p, adnc17data.te[,-19])
error_mt<-RMSE(adnc17data.te$audience17,p.m5p)
error_mt


###################################################
###############랜덤포레스트 모델링#################
###################################################

set.seed(800)
m.rf<-randomForest(audience17 ~ age+country+holiday17+screen+company+genre+director+star1+star2+star3+day0a+SumGrade ,data=adnc17data.tr)
p.rf<-predict(m.rf, adnc17data.te[,-19])
error_rf <- RMSE(adnc17data.te$audience17,p.rf)
error_rf #RMSE: 334715



###################################################
###############MARS 모델링#########################
###################################################
##bagEarth

fit.be<-train(audience17 ~ age+country+holiday17+screen+company+genre+director+star1+star2+star3+day0a+SumGrade, data=adnc17data.tr, method = "bagEarth")

p.be<-predict(fit.be, adnc17data.te[-19])
p.be
a<-cbind.data.frame(adnc17data.te$audience17,p.be,adnc17data.te$audience17-p.be)
a
error_be<-RMSE(adnc17data.te$audience17,p.be)
error_be



#####################################################
######################최종예측-17일##################
#####################################################
magcafe<- read.csv("magcafe.csv")
magcafe[ ,20]<-magcafe[ ,"pregrade"]*magcafe[ ,"preparti"]
names(magcafe)[20]<-"SumGrade"
str(magcafe)

fulldt<- rbind(adnc17data, magcafe)
tail(fulldt)

#훈련데이터 90% 테스트데이터 10%
set.seed(800)
ind<-sample(nrow(adnc17data),size=floor(nrow(adnc17data)*0.9))

adnc17data.tr <-adnc17data[ind, ]
adnc17data.te <-adnc17data[-ind, ]


final_output<-predict(fit.be, fulldt[c(1374,1375),-19])
final_output


##매그니피센트7 : 1347929명
##카페소사이어티 : 63037명






