getwd()
population_c <- read.csv('world_bank_data_city.csv', header = F)
population_w <- read.csv('world_bank_data_city.csv', header = F)

#데이터 전처리
p_c <-  population_c[-c(1,218:223),c(3,64)]
p_w <-  population_w[-c(1,218:223),c(3,64)] 
names(p_c) <- c("Country Name", "City Data")
names(p_w) <- c("Country Name", "Country Data")

#통합 데이터셋 생성(나라별 총 인구, 나라별 가장 큰도시 인구)
dataset_P <- data.frame(p_c$`Country Name`, p_w$`Country Data`, p_c$`City Data`)
names(dataset_P) <- c("Country Name", "Country Data", "City Data")

#통합 데이터 셋 추가(나라별 가장 큰도시 거주 비율)
dataset_P$P_portion <- round((as.numeric(dataset_P$`City Data`)/
  as.numeric(dataset_P$`Country Data`))*100,2)

#결측치 0으로 수정
dataset_P$P_portion <- ifelse(
  !is.na(dataset_P$P_portion),dataset_P$P_portion,0)

#=========================================#

#(1)
dataset_P


#(2)
#비율 상위20개국
x <- sort(dataset_P$P_portion, decreasing = T)
head(x,20)

high20_P <- subset(dataset_P, dataset_P$P_portion >= 33.26)



#(3)
#비율 하위 20개국
y <- sort(dataset_P$P_portion)
head(y,20)

low20_P <- subset(dataset_P, 0 < dataset_P$P_portion & dataset_P$P_portion <= 7.09)



#(4)
#데이터가 없는 국가 -> 비율이 결측치로 나오고 이것을 위의 결측치 수정으로 바꿈
na_P <- subset(dataset_P, dataset_P$P_portion == 0)

