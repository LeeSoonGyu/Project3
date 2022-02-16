#빅데이터A 2회 임성현, 이순규, 한호종
#####################################
#데이터 로딩 및 전처리
getwd()
setwd('C:/Rwork/bank') #각 나라별 최대도시의 인구수
setwd('D:/bank')

#pdata <- read.csv('world_bank_data_city.csv') #에러
#pdata <- read.csv('world_bank_data_city.csv', header = T) #에러
#pdata <- read.csv('world_bank_data_city.csv', header = F) #에러는 나지 않지만 컬럼명이 사라짐
pdata <- read.csv('world_bank_data_city1.csv', sep = ',', header = F, encoding = "UTF-8")
#str(pdata)
#View(pdata)

pdata_2019 <- pdata[c(1,64)] #2019년도의 나라 이름과 최대도시인구수 추출
#pdata_2019
pdata_2019_2 <- pdata_2019[4:269,] #1은 데이터소스, 2는 업데이트날짜, 3은 컬럼명 제거
#pdata_2019_2
pdata_2019_3 <- data.frame(country_name = pdata_2019_2$V1, '2019_PiLC' = pdata_2019_2$V64)
pdata_2019_3 #2019년도 각 나라의 가장 큰 도시의 인구수[나라이름, 가장 큰 도시 인구수]

ptotal <- read.csv('world_bank_data_pop.csv', sep = ',', header = F, encoding = "UTF-8")
#str(ptotal)
#View(ptotal)

ptotal_2019 <- ptotal[c(1,64)] #2019년도 나라이름, 총인구수 추출출
#ptotal_2019
ptotal_2019_2 <- ptotal_2019[4:269,] #1 데이터소스, 2 업데이트날짜, 3 컬럼명 제거
#ptotal_2019_2
ptotal_2019_3 <- data.frame(country_name = ptotal_2019_2$V1, '2019_Ptotal' = ptotal_2019_2$V64)
ptotal_2019_3 #2019년도 각 나라별 총인구수[나라이름, 총인구수]
###################################################
#(1) 총인구수 / 가장큰도시 인구수 / 총인구수 대비 가장큰도시 인구수의 비율
library(plyr)
csdata <- join(pdata_2019_3, ptotal_2019_3, by = 'country_name')
names(csdata) <- c('country_name', 'largest_city_population', 'total_population')
csdata$LC_div_total <- csdata$largest_city_population / csdata$total_population
csdata[sample(1:length(csdata$country_name), 5),] #sample 함수를 써서 랜덤하게 5개 데이터 출력
#largest_city_population = 가장큰도시 인구수 / total_population = 총인구수
#= LC_div_total = 총인구수 대비 가장큰도시 인구수의 비율

#(2) 비율이 가장 높은 상위 20개 나라 리스트
#install.packages("dplyr") #dplyr 패키지가 설치되어있다면 생략
library(dplyr)
cslist_sorted <- sort(csdata$LC_div_total, decreasing = T) #decreasing = TURE로 써서 내림차순정렬
head(cslist_sorted, 20) #상위 20개
csdata %>% filter(LC_div_total >= cslist_sorted[20]) #20번째 값보다 같거나 큰 나라들로 정렬 없이 출력

csdata_sorted1 <- csdata[order(csdata$LC_div_total, decreasing = T),] #order 함수로 내림차순정렬
head(csdata_sorted1, 20) #상위 20개 나라 출력

#(3) 비율이 가장 낮은 상위 20개 나라 리스트
cslist_sorted <- sort(csdata$LC_div_total, decreasing = F) #반대로 FALSE 를 써서 오름차순정렬 생략가능
head(cslist_sorted, 20) #리스트 중 상위 20개 = 비율값 중에 낮은 순으로 20개
csdata %>% filter(LC_div_total <= cslist_sorted[20]) #20번째 값보다 같거나 작은 나라들로 정렬 없이 출력

csdata_sorted2 <- csdata[order(csdata$LC_div_total),] #order 함수로 오름차순정렬
head(csdata_sorted2, 20) #하위 20개 나라 출력

#(4) 해당 항목에 데이터가 없는 나라 리스트
cs_na_data <- csdata %>% filter(is.na(LC_div_total)) #is.na 함수를 써서 NA값을 갖고 있는 데이터추출 
cs_na_list <- cs_na_data$country_name #나라의 이름들만 리스트로 추출
cs_na_list
