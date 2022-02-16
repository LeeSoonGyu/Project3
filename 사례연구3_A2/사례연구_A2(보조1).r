#빅데이터A 2차 임성현, 이순규, 한호종
#사례연구3 통계기반 데이터 분석

#1.
#(1)
data('state') #데이터 로드
dfStateX77 <- data.frame(state.x77)  #데이터프레임으로 변환/원래 데이터의 Life Exp 와 HS Grad가 각각
                                    #Life.Exp와 HS.Grad 로 변환

#(2)
x77Result.lm <- lm(formula = Life.Exp ~ .,data = dfStateX77) #Life.Exp 종속변수 / 나머지 컬럼은 독립변수
x77Result.lm
summary(x77Result.lm)
#회귀식 : Life.Exp = 7.094e+01 + 5.180e-05*Population - 2.180e-05*Income + 3.382e-02*Illiteracy
#                   - 3.011e-01*Murder + 4.893e-02*HS.Grad - 5.735e-03*Frost - 7.383e-08*Area
# Pr(>|t|) 부분에서 확인 할 수 있는 p값을 통해  유의수준을 보면 ***의 Murder와 *의 HS.Grad를 뺀 나머지는
# 유의하지 않다.

#(3)
x77Result.lm2 <- lm(formula = Life.Exp ~ Population+Murder+HS.Grad+Frost, data = dfStateX77)
x77Result.lm2
summary(x77Result.lm2)
#회귀식 : Life.Exp = 7.103e+01 + 5.014e-05*Population - 3.001e-01*Murder + 4.658e-02*HS.Grad - 5.943e-03*Frost
# Pr(>|t|) 부분에서 확인 할 수 있는 p값을 통해  유의수준을 (2)와 비교해 보면
# ***의 Murder는 그대로 이고, **의 HS.Grad는 (2)번의 회귀식에 비해 *가 하나 늘었다. 더 유의미해졌다.
# Frost도 .에서 * 하나만큼으로 유의미해졌고, Population은 . 으로 그대로 유의미하지 않다.

#(4)
#독립변수 = 원인변수, 예측변수, 설명변수 / 종속변수 = 반응변수, 결과변수
x77Result.lm3 <- lm(formula = Life.Exp ~Murder+HS.Grad, data = dfStateX77)
x77Result.lm3
summary(x77Result.lm3)
#회귀식 : Life.Exp =  70.29708 - 0.23709*Murder + 0.04389*HS.Grad
# Pr(>|t|) 부분의 유의수준 판정을 *** = Murder 와 ** 의 HS.Grad로 (3)과 비교해보면 값은 달라졌지만 판정은 그대로이다.

#(5)
#(4)의 회귀식을 사용하여 계산하면
Y = 70.29708 - 0.23709*8.0 + 0.04389*55.0
Y #70.81431 이 예측값이다.

#(6)
#install.packages("ggplot2")
library(ggplot2)
ggplot(x77_result.lm3)



#2.
getwd()
setwd('C:/Rwork/casestudy3')
kospiData <- read.csv('KOSPI(110103~211108).csv')
#is.data.frame(kospiData) #TRUE
kospiClosingTs <- ts(kospiData$종가, start = c(2011,01,03), frequency = 246)
#11년 1월 3일부터 시작하여 21년 11월 8일까지의 데이터의 nrow = 2673개의 데이터가 있다.
#11년~20년까지의 10년분 데이터와 21년 1월 1일부터 21년 11월 8일까지의 311일 데이터
#11년~20년까지를 10 + 311일 데이터는 21년1월1일부터 21년12월31일까지 364일 중의 데이터
#(10 + 311/364) 이것을 분모로 두고 분자를 2673으로 둬서 계산된 값
#2673 / (10 + 311/364) = 246.2597  이 값을 소수 첫번째의 자리에서 반올림하여 246 으로 frequency 설정
plot(kospiClosingTs, type = "l", col = "red") #추세선 시각화
#(2)
plot(stl(kospiClosingTs, "periodic")) #시계열자료의 변동 4요인 분해1
plot(decompose(kospiClosingTs)) #시계열자료의 변동 4요인 분해2
#(3) 위 코드에 시각화포함
#(4)
