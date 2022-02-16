# 사례연구 A2조 임성현,이순규,한호종
# 1. 'state'에 있는 state.x77 데이터셋 불러오기

data('state')
state.x77

# 1-1 state 데이터셋을 load하고, state.x77 dataset을 데이터프레임으로 변환하고, Life Exp 변수를
# Life.Exp로 HS Grad변수를 HS.Grad로 변경하시오.
library(plyr)
state.df <- as.data.frame(state.x77)
state.df
state.df <- rename(state.df, replace = c('Life Exp' = 'Life.Exp'))
state.df <- rename(state.df, replace = c('HS Grad' = 'HS.Grad'))

head(state.df)

# 1-2 Life Expectancy 변수를 종속변수로 설정하고 나머지 변수를 독립변수로 설정하여 회귀분석을
# 실시하시오. 실시 후 결과에 대해 해석하시오.
# lm() <- 회귀분석 함수 lm(종속변수 ~ 독립변수, 데이터)
soon <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, state.df)
soon
summary(soon)
# 회귀식 : Life.Exp = 7.094e+01 + 5.180e-05*Population - 2.180e-05*Income + 3.382e-02*Illiteracy - 3.011e-01*Murder + 4.893e-02*HS.Grad - 5.735e-03*Frost - 7.383e-08*Area
# p값을 통해  유의수준을 보면 Murder와 HS.Grad를 뺀 나머지 변수는 유의하지 않음.

# 1-3 1-2번 회귀모형에서 Income, Illiteracy, Area 변수를 제외하고 회귀분석을 실시하고 결과에 대해
# 해석하시오.
gyu <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, state.df)
gyu
summary(gyu)
# 회귀식 : Life.Exp = 7.103e+01 + 5.014e-05*Population - 3.001e-01*Murder + 4.658e-02*HS.Grad - 5.943e-03*Frost
# 1-2와 p값을 비교하면 Muder변수는 변동이 없으며, HS.Grad 변수는 *추가로 1-2보다는 유의해졌으며, Frost 변수도 *추가 1-2보다는 유의해짐.

# 1-4 Life Expectancy 변수를 종속변수로 설정하고 HS.Grad와 Murder 변수를 예측변수(predictor variable)로 설정하여 회귀분석을 실시하시오.
# 예측변수, 설명변수 = 독립변수 / 반응변수 = 종속변수
soongyu <- lm(Life.Exp ~ HS.Grad + Murder, state.df)
soongyu
summary(soongyu)
# 1-3 의 분석과 다른게 없음.

# 1-5 전 인구의 55%가 고졸이고 살인비율이 10만명당 8명일 때 Life Expectancy 결과값을 예측하시오.
model <- lm(formula = Life.Exp ~ HS.Grad + Murder, state.df)
model
summary(model)
Y = 70.29708 + 0.04389*55 -0.23709*8
Y # 예측값

# 1-6 1-4번에서 처럼 2개의 독립변수, 1개의 종속변수의 데이터와 fit된 회귀평면(fitted regression plane)을 3D 그래프로 시각화하시오.
install.packages('predict3d')
library(predict3d)
predict3d(soongyu,
          xlab = 'Murder',
          ylab = 'HS.Grad',
          zlab = 'Life.Exp',
          radius = 0.4,
          show.subtitle = F,
          show.plane = T,
          plane.color = 'green',
          plane.alpha = 0.5,
          type = 's')

# 2. 과거 10년간 일별 KOSPI 지수(종가기준) 데이터를 기준으로 시계열분석을 실시하시오.
getwd()
setwd('D:/')
kospi <- read.csv('kospi.csv', header = T)
kospi

kospi_rev <- rev(kospi$종가)

kospi_ts <- ts(kospi_rev, start = c(2011, 226), frequency = 246)
kospi_ts

# 2-1 추세선 확인
X11() # windows() 함수랑 동일
ts.plot(kospi_ts, type = 'l', col = 'red')

# 2-2 4가지 시계열 자료의 변동요인을 분해 및 2-3 시각화
windows()
plot(stl(kospi_ts, "periodic"))

# 2-4 결과 해석
# arima분석
library(forecast)
arima <- auto.arima(kospi_ts)
arima
leesoongyu <- arima(kospi_ts, order = c(2,1,2))
leesoongyu

windows()
tsdiag(leesoongyu)
Box.test(leesoongyu$residuals, lag = 1, type = 'Ljung')
# Box-Ljung검정: 모형의 잔차를 이용하는 카이제곱 검정방법. 시계열 모형이 통계적으로 적절한지검정.
# P-value가 0.05이상이면 모형이 통계적으로 적절

# 미래 예측
windows()
par(mfrow = c(1,2))
fore <- forecast(leesoongyu, h = 3)
plot(fore)
fore2 <- forecast(leesoongyu, h = 6)
plot(fore2)

# 이동평균법
library(TTR)
windows()
par(mfrow = c(2,2))
plot(kospi_ts, main = '원 시계열 자료')
plot(SMA(kospi_ts, n = 1), main = ' 1개월 단위 이동평균법으로 평활')
plot(SMA(kospi_ts, n = 2), main = ' 2개월 단위 이동평균법으로 평활')
plot(SMA(kospi_ts, n = 3), main = ' 3개월 단위 이동평균법으로 평활')

# 차분적용 - 평균정상화
diff_kospi <- diff(kospi$종가)
diff_kospi_ts <- diff(kospi_ts)

windows()
par(mfrow = c(1,2))
plot(diff_kospi)
plot(diff_kospi_ts)

# 로그적용 - 분산정상화
log_diff_kospi <- diff(log(kospi$종가))
log_diff_kospi_ts <- diff(log(kospi_ts))

windows()
par(mfrow = c(1,2))
plot(log_diff_kospi)
plot(log_diff_kospi_ts)

# 자기 상관 함수 시각화
windows()
acf(na.omit(kospi_ts), main = '자기상관함수', col = 'red')

# 부분 자기 상관 함수 시각화
windows()
pacf(na.omit(kospi_ts), main = '부분자기상관함수', col = 'red')

# 시계열 분해와 변동요인 제거
m <- decompose(kospi_ts)
attributes(m)
plot(kospi_ts - m$trend)
plot(kospi_ts - m$seasonal)
plot(kospi_ts - m$trend - m$seasonal)
