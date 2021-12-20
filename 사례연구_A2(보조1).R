# A2조 : 임성현, 이순규, 한호종

getwd() # 현재 위치 찾기
setwd('C:/Temp/Rwork/bank') # 사용자 마다 불러올 데이터 위치가 다르므로 설정이 다를 수 있습니다.
setwd('D:/bank')
population <- read.csv('world_bank_data_pop.csv', header = T) # 준비한 데이터 셋 불러오기
city <- read.csv('world_bank_data_city1.csv', header = T)
population
head(population)
View(population)
city
head(city)
View(city)

# 데이터 전처리
population1 <- population[-c(2:3, 5:63, 65)] # 2019 자료 뽑기 
population1
head(population1, 10)
str(population1)
View(population1)
city1 <- city[-c(2:3, 5:63, 65)] # 2019년 자료 뽑기
city1
head(city1, 10)
str(city1)
View(city1)
TOTAL.POP <- subset(population1, Indicator.Code == 'SP.POP.TOTL')
CITY.POP <- subset(city1, Indicator.Code == 'EN.URB.LCTY')

names(TOTAL.POP) <- c('Country.name', 'Code', 'Country.pop.Data') # 컬럼명 변경
names(CITY.POP) <- c('Country.name', 'Code', 'Country.city.Data') # 컬럼명 변경
# ------------------------------------------------------
# (1) 2019년 기준 나라별 총 인구수, 그 나라에서 가장 큰 도시에 사는 인구수, 나라별 총 인구수
# 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율을 구하시오. 
# (Population in largest city 항목 데이터 사용)
# ------------------------------------------------------

TOTAL.POP # 2019년 나라별 총 인구
CITY.POP # 2019년 큰 도시 나라별 인구

total_data <- cbind(TOTAL.POP, CITY.POP$Country.city.Data)
names(total_data) <- c('Country.name', 'code', 'Country.pop.Data' , 'Country.city.Data') # CITY.POP$Country.city.Data 컬럼명 변경
total_data <- total_data[-2] # 불필요한 데이터 제거
total_data
head(total_data, 10)
total_data2 <- total_data
Rate.pop <- round((as.numeric(total_data2$Country.pop.Data) / 
                      as.numeric(total_data2$Country.city.Data)) * 100, 2) # 인구비율 계산, NA값 포함
total_data3 <- cbind(total_data2, Rate.pop)
total_data3

View(total_data)
View(total_data3)

# ------------------------------------------------------
# (2) 총 인구수 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율이 가장 높은 나라 20개국 리
# 스트를 작성하시오 (비율 항목 포함)
# (3) 총 인구수 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율이 가장 낮은 나라 20개국 리
# 스트를 작성하시오. (비율 항목 포함)
# ------------------------------------------------------
install.packages('dplyr')
library(dplyr)

total_data4 <- na.omit(total_data3) # NA 데이터 삭제
total_data4 <- total_data4[-c(2:3)] # 불필요 데이터 삭제

higher_data <- total_data4 %>% arrange(desc(Rate.pop))
higher_data[c(1:20), ]  # (2) 비율 항목 포함 비율 높은 나라 20개국

lower_data <- total_data4 %>% arrange(Rate.pop)
lower_data[c(1:20), ]  # (3) 비율 항목 포함 비율 낮은 나라 20개국

View(higher_data)
View(lower_data)

# ------------------------------------------------------
# (4) 해당 항목에 데이터가 없는 나라의 리스트 전체를 작성하시오
# ------------------------------------------------------

non_data <- subset(total_data3, is.na(total_data3$Rate.pop) == T)
non_data  # 각 칼럼에서 데이터가 없는 나라

View(non_data)
