#1
state.x77
str(state.x77)

#1-1
state.x77_df <- data.frame(state.x77)
state.x77_df
library(dplyr)
colnames(state.x77_df)
state.x77_df <- rename(state.x77_df, 'Life.Exp' = 'Life.Exp', 'HS.Grad' = 'HS.Grad')
colnames(state.x77_df)



#1-2
state.x77_df_rt = lm(formula = Life.Exp ~ . , data = state.x77_df)
state.x77_df_rt

#가설
#귀무가설 : Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area는 Life.Exp에 영향을 미친다고 볼 수 없다.
#대립가설 : Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area는 Life.Exp에 영향을 미친다고 볼 수 있다.

#분석결과 
summary(state.x77_df_rt)

#가설검정 : 다중 공선성 문제 확인
#install.packages('car')
#library(car)
vif(state.x77_df_rt) > 10 #vif 값이 10이 넘는 값이 없으므로 문제 없음

#회귀모형 결정계수 Multiple R-squared
#0.7362 전체의 73%를 설명가능

#수정 결정계수 Adjusted R-squared
#0.6922 전체의 69%를 설명가능

#회귀모형의 적합성 : p-value
#2.534e-10 < 0.05

#독립변수 설명
#독립변수들의 Pr(>|t|) 값중 유의수준 0.05 수준을 기준으로 하였을 때,
#독립변수 Murder, HS.Grad는 유의하나, 나머지 변수들은 유의미하지 않다.
#회귀모형 Life.Exp = 5.180e-05*Population - 2.180e-05*Income + 3.382e-02*Illiteracy - 3.011e-01*Murder + 4.893e-02*HS.Grad - 5.735e-03*Frost - 7.383e-08*Area + 7.094e+01
#그대로 사용해서는 안 된다.
  


#1-3
state.x77_df_rt2 = lm(formula = Life.Exp ~ Population + Murder + HS.Grad + Frost , data = state.x77_df)
state.x77_df_rt2

#가설
#귀무가설 : Population, Murder, HS.Grad, Frost는 Life.Exp에 영향을 미친다고 볼 수 없다.
#대립가설 : Population, Murder, HS.Grad, Frost는 Life.Exp에 영향을 미친다고 볼 수 있다.

#분석결과 
summary(state.x77_df_rt2)

#가설검정 : 다중 공선성 문제 확인
#install.packages('car')
#library(car)
vif(state.x77_df_rt2) > 10 #vif 값이 10이 넘는 값이 없으므로 문제 없음

#회귀모형 결정계수 Multiple R-squared
#0.736 1-2 결과보다 높아짐 / 전체의 73%를 설명가능

#수정 결정계수 Adjusted R-squared
#0.7126 1-2 결과보다 높아짐 / 전체의 71%를 설명가능

#회귀모형의 적합성 : p-value
#1.696e-12 < 0.05 1-2 결과보다 더 유의미함

#독립변수 설명
#독립변수들의 Pr(>|t|) 값중 유의수준 0.05 수준을 기준으로 하였을 때,
#독립변수 Murder, HS.Grad, Frost는 유의하나, 나머지 변수 Population은 유의하지 않다.
#회귀모형 Life.Exp = 5.014e-05*Population - 3.001e-01*Murder + 4.658e-02*HS.Grad - 5.943e-03*Frost + 7.103e+01
#독립변수 Population 때문에 그대로 사용해서는 안 된다.



#1-4
state.x77_df_rt3 = lm(formula = Life.Exp ~ Murder + HS.Grad, data = state.x77_df)
state.x77_df_rt3

#가설
#귀무가설 : Murder, HS.Grad는 Life.Exp에 영향을 미친다고 볼 수 없다.
#대립가설 : Murder, HS.Grad는 Life.Exp에 영향을 미친다고 볼 수 있다.

#분석결과 
summary(state.x77_df_rt3)

#가설검정 : 다중 공선성 문제 확인
#install.packages('car')
#library(car)
vif(state.x77_df_rt3) > 10 #vif 값이 10이 넘는 값이 없으므로 문제 없음

#회귀모형 결정계수 Multiple R-squared
#0.6628 1-3 결과보다 낮아짐 / 전체의 66%를 설명가능

#수정 결정계수 Adjusted R-squared
#0.6485 1-3 결과보다 낮아짐 / 전체의 64%를 설명가능

#회귀모형의 적합성 : p-value
#8.016e-12 < 0.05 1-3 결과보다 덜 유의미함

#독립변수 설명
#독립변수들의 Pr(>|t|) 값중 유의수준 0.05 수준을 기준으로 하였을 때,
#독립변수 Murder, HS.Grad 유의하다
#회귀모형 Life.Exp = -0.23709*Murder + 0.04389*HS.Grad + 70.29708
#그대로 사용가능



#1-5
#회귀모형 Life.Exp = -0.23709*Murder + 0.04389*HS.Grad + 70.29708
Y = - 0.23709*8 + 0.04389*55 + 70.29708 
Y



#1-6
#install.packages('predict3d')
library(predict3d)
predict3d(state.x77_df_rt3,
          xlab = 'Murder',
          ylab = 'HS.Grad',
          zlab = 'Life.Exp',
          radius = 0.4,
          show.subtitle = F,
          show.plane = T,
          plane.color = 'green',
          color = 'blueviolet',
          plane.alpha = 0.5,
          type = 's')



#2
getwd()
kospi <- read.csv('kospi4.csv')
library(dplyr)
kospi <- arrange(kospi, kospi$일자)

#2-1
nrow(kospi)
kospi_C.P_ts <- ts((kospi$종가), frequency = 246, start = c(2011,226))
plot(kospi_C.P_ts, type = "l", col = "red")

#2-2
stl(kospi_C.P_ts, "periodic")

#2-3
plot(stl(kospi_C.P_ts, "periodic"))

#2-4
plot(stl(kospi_C.P_ts, "periodic"))
par(mfrow = c(2, 2))
#=================차분, 로그화==============#
diff_kospi <- diff(kospi$종가)
diff_kospi_C.P_ts <- diff(kospi_C.P_ts)

plot(diff_kospi)
plot(diff_kospi_C.P_ts)

plot(log(kospi_C.P_ts))

log_diff_kospi <- diff(log(kospi$종가))
log_diff_kospi_C.P_ts <- diff(log(kospi_C.P_ts))

plot(log_diff_kospi)
plot(log_diff_kospi_C.P_ts)
#==================변동요인 제거======================#
m_kospi_C.P_ts <- decompose(kospi_C.P_ts)
attributes(m_kospi_C.P_ts)
plot(m_kospi_C.P_ts)

par(mfrow = c(1,1))
plot(kospi_C.P_ts)
plot(kospi_C.P_ts - m_kospi_C.P_ts$seasonal)
plot(kospi_C.P_ts - m_kospi_C.P_ts$trend)
plot(kospi_C.P_ts - m_kospi_C.P_ts$seasonal - m_kospi_C.P_ts$trend)

par(mfrow = c(2,1))
acf(na.omit(kospi_C.P_ts), main = "자기 상환함수", col = "red")
pacf(na.omit(kospi_C.P_ts), main = "부분 자기 상관 함수", col = "red")
plot(diff(kospi_C.P_ts), differences = 1)
#=====================ARIMA===========================#
library(forecast)
diff_kospi_C.P_ts
par(mfrow = c(1,1))
plot(diff_kospi_C.P_ts)

Arima_kospi <- arima(kospi_C.P_ts, order = c(1,1,0))
Arima_kospi
tsdiag(Arima_kospi)

Box.test(Arima_kospi$residuals, lag = 1, type = "Ljung")

fore <- forecast(Arima_kospi)
fore
par(mfrow = c(1, 2))
plot(fore)
model2 <- forecast(Arima_kospi, h = 6)
plot(model2)

























