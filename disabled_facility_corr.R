# 주제탐구세미나 1 (007) 기말과제 발표 R script
# 권유리, 윤수민, 조진혁, 최윤희

# ======== Basic Info ====================
# IMPORTANT: Please open this document in UTF-8 format.

# This file serves the following purpose:
# Calculating the correlation between...
# (ratio of disabled in metro station usage) and (number of metro facilities for disabled)
# Testing the relevancy of correlation

# Data range: All of 2022

# Author of this script: 윤수민

# ======== Installing / Opening Dependencies ====================
install.packages("dplyr"); install.packages("ggplot2"); install.packages("ggrepel")
library(dplyr); library(ggplot2); library(ggrepel)


# ======== Importing Data ====================
# ticket.type: Num. of passengers of given station in 2022, organized by type of ticket
# Data Source: 서울교통공사 스마트정보처
# https://data.seoul.go.kr/dataList/OA-21720/F/1/datasetView.do
ticket.type <- read.csv("raw_data/서울교통공사_역별 권종별 우대권 승차현황_20221231.csv",
                        fileEncoding = "euc-kr", header = T)

# subway.all: Num. of total passengers of given station (will tidy)
# Data Source: 서울특별시 도시교통실 교통기획관 교통정책과
# https://data.seoul.go.kr/dataList/OA-12914/S/1/datasetView.do
subway.all <- read.csv("raw_data/CARD_SUBWAY_MONTH_2022.csv",
                       fileEncoding = "euc-kr", header = T, row.names = NULL)

# facility: Data about metro station facilities for the disabled
# Data Source: 서울교통공사 승강기사업소
# https://data.seoul.go.kr/dataList/OA-11573/S/1/datasetView.do
facility <- read.csv("raw_data/서울시 지하철 역사 노약자 장애인 편의시설 현황/facility_organized.csv",
                     fileEncoding = "utf-8", header = T)


# ======== Tidying Data ====================

# Tidying subway.all to desired format
colnames(subway.all) <- colnames(subway.all)[2:ncol(subway.all)]
subway.all <- subway.all[1:(ncol(subway.all)-1)]
# Aggregating pass. num. by station (calculating total pass. by station)
subway.all <- aggregate(승차총승객수 ~ 노선명 + 역명,
                        data = subway.all, sum)
colnames(subway.all)[1] <- "호선"
ticket.type$호선 <- paste(ticket.type$호선, "호선", sep = "")
# Filtering Stations (tgt dataset is line 1 ~ 8)
stations <- ticket.type[!duplicated(ticket.type$역번호),c(2, 4)]
subway.filtered <- subway.all[subway.all$호선 %in% stations$호선 & subway.all$역명 %in% stations$역명,]

# For some reason, 3호선 충무로 / 6호선 연신내 not included in ticket.type
# Filter out these variables
sub.sta <- subway.filtered[,c("호선", "역명")]
tic.sta <- ticket.type[,c("호선", "역명")]

(excl <- anti_join(sub.sta, tic.sta)) # 2 instances!!
(excl2 <- anti_join(tic.sta, sub.sta))
excl$index <- c(0, 0)
for (i in 1:2) {
  excl$index[i] <- which(sub.sta$호선 == excl[i,1] & sub.sta$역명 == excl[i,2])
}
subway.filtered <- subway.filtered[-c(excl$index),]

# For now, export subway.filtered
write.csv(subway.filtered, "raw_data/subway_total_2022.csv")

# For quick launch: open this file
subway.filtered <- read.csv("raw_data/subway_total_2022.csv")

# Calculating the ratio of disabled by station
subway.filtered <- subway.filtered[order(subway.filtered$호선, subway.filtered$역명),]
ticket.type <- ticket.type[order(ticket.type$호선, ticket.type$역명),]
ticket.type$total <- subway.filtered$승차총승객수
ticket.type$ratio <- ticket.type$장애 / ticket.type$total
ticket.type.byratio <- ticket.type[order(ticket.type$ratio, decreasing = T),]

# For now, export ratio data
write.csv(ticket.type.byratio, "ratio.csv")

# Organizing facility data
facility <- facility[order(facility$호선, facility$역명),]
facility$호선 <- paste(facility$호선, "호선", sep = "")
write.csv(facility, "raw_data/facility_new.csv")
facility$real.total <- facility$elevator + facility$moving.walk + facility$wheelchair.lift

# Because of the difference between the naming procedure of 'facility' data,
# We had to manually filter out the stations which do not exist in ticket.type ...
t.compare <- ticket.type[,c("호선", "역명")]
t.compare <- rbind(t.compare, c(0, 0), c(0, 0))
f.compare <- facility[,c("호선", "역명")]
compare <- cbind(t.compare, f.compare)
write.csv(compare, "raw_data/compare.csv", row.names = F)

# Differences are:
# 청구 (5호선) in tickets (row 169), 신내 (6호선) in facility (row 196), 연신내 (6호선) in facility (row 201)
# also Total row in facility

# Now delete the stations which do not exist on either side
excl3 <- rbind(c("5호선", "청구", 169, "t"),
              c("6호선", "신내", 196, "f"),
              c("6호선", "연신내", 201, "f"),
              c("NA호선", "total", 274, "f"))

ticket.new <- ticket.type
facility.new <- facility

ticket.new <- ticket.new[-as.numeric(excl3[excl3[,4] == "t"][3]),]
facility.new <- facility.new[-as.numeric(excl3[excl3[,4] == "f"][7:9]),]

# Combine data to comprehensive df
info <- cbind(ticket.new, elevator = facility.new$elevator, moving.walk = facility.new$moving.walk,
              wheelchair.lift = facility.new$wheelchair.lift, facility.total = facility.new$real.total)

# Export this comprehensive information
write.csv(info, "disabled_facility_information_by_station.csv", fileEncoding = "utf-8")

# For quick launch: open this file
info <- read.csv("disabled_facility_information_by_station.csv", fileEncoding = "utf-8")

# ======== Drawing Insights ====================

# ======== Examining Data ====================

# Examination 1. Examining proportion of disabled passengers ==========
boxplot(info$ratio,
        ylab = "전체 승차 승객 중 장애인 승차 승객 비율") # Detailed Visualization @ Plot 1-E-1-P
mean(info$ratio); sd(info$ratio) # mean: 2.6545%, sd: 0.9540%
summary(info$ratio)
head(info[order(info$ratio, decreasing = T),
          c("호선", "역명", "ratio")]) # Top stations: 동묘앞, 모란, 제기동, 청량리, 동대문 

# Does the ratio follow a normal distribution?
qqnorm(info$ratio) # Detailed Visualization @ Plot E-1-Q
qqline(info$ratio)
shapiro.test(info$ratio) # p-value < 0.001, can be said to follow normal dist


# Examination 2. Examining number of disabled passengers ==========
boxplot(info$장애,
        ylab = "전체 승차 승객 중 장애인 승차 승객 수") # Detailed Visualization @ Plot 1-E-2-P
mean(info$장애); sd(info$장애) # mean: 124485.7, sd: 81641.5
summary(info$장애)
head(info[order(info$장애, decreasing = T),
          c("호선", "역명", "장애")]) # Top stations: 종로3가, 서울역, 신림, 연신내

# Does the ratio follow a normal distribution?
qqnorm(info$장애) # Detailed Visualization @ Plot E-2-Q
qqline(info$장애)
shapiro.test(info$ratio) # p-value < 0.001, can be said to follow normal dist


# ======== Testing Data ====================

# Test 1. Relationship between (proportion of disabled) and (facilities in station) ==========
# Test 1-1. Basic Correlation Data
plot(info$ratio, info$facility.total) # Detailed Visualization @ Plot 1-T-1-1
cor(info$ratio, info$facility.total) # Corr: 0.03479202
cor.test(info$ratio, info$facility.total) # p-value: 0.5685, cannot rule out that corr is 0
# Conclusion: No relationship observed

# Test 1-2. Examining simple linear regression model
summary(lm(ratio ~ facility.total, data = info))
# p value of beta is 0.568: accept null hypothesis (regression is insignificant)

# Test 2. Examining multiple linear regression model of ratio ============
# how (proportion of disabled) is influenced based on (many components of 'facilities')
summary(lm(ratio ~ elevator + moving.walk + wheelchair.lift, data = info))
# all p values are > 0.05, no significant relationship to overall ratio

# Test 3. Relationship between (number of disabled) and (facilities in station) ==========
# Test 3-1. Basic Correlation Data
plot(info$장애, info$facility.total) # Detailed Visualization @ Plot 1-T-3-1
cor(info$장애, info$facility.total) # Corr: 0.01788375
cor.test(info$장애, info$facility.total) # p-value: 0.7695, cannot rule out that corr is 0
# Conclusion: No relationship observed

# Test 3-2. Examining simple linear regression model
summary(lm(장애 ~ facility.total, data = info))
# p value of beta is 0.769: accept null hypothesis (regression is insignificant)

# Test 4. Examining multiple linear regression model of ratio ============
# how (number of disabled) is influenced based on (many components of 'facilities')
summary(lm(장애 ~ elevator + moving.walk + wheelchair.lift, data = info))
# all p values are > 0.05, no significant relationship to overall ratio


# ======== General Conclusion ====================
# The installation of facilities for the disabled has no relationship with the number of disabled,
# neither does it have a relation with the proportion of disabled passengers (in a given metro station).
# This implies that the local government lacks consideration over the true status quo of disabled passengers,
# when installing transport facilities for the disabled.


# ======== Detailed Visualization w/ggplot2 ====================
# Plot 1-E-1-P: Boxplot of proportion of disabled passengers ============
plt.e.1.p <- ggplot(info, mapping = aes(x = 0, y = ratio * 100)) + geom_boxplot() +
  labs(title = "지하철역별 장애인 승객 비중",
       y = "장애인 승객 비율 (%)",
       caption = "출처: 서울교통공사, 서울시청 도시교통과") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Highlight outliers
find_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

info$outlier.ratio <- find_outlier(info$ratio)
plt.e.1.p <- plt.e.1.p + geom_text_repel(size = 4,
                                         data = info %>% filter(outlier.ratio == T),
                                         label = paste(info[info$outlier.ratio == T,]$역명,
                                                       ":", round(info[info$outlier.ratio == T,]$ratio * 100, 4), "%"))

print(plt.e.1.p)
ggsave(filename = "plot/1-E-1-P.png", plot = plt.e.1.p,
       height = 6, width = 4.5)

# Plot 1-E-1-Q: Q-Q Plot of proportion of disabled passengers ============
plt.e.1.q <- ggplot(info, mapping = aes(sample = ratio * 100)) + geom_qq() +
  geom_qq_line() +
  labs(title = "지하철역별 장애인 승객 비중의 Q-Q Plot",
       y = "장애인 승객 비율 (%)",
       caption = "출처: 서울교통공사, 서울시청 도시교통과")

print(plt.e.1.q)
ggsave(filename = "plot/1-E-1-Q.png", plot = plt.e.1.q,
       height = 5, width = 5)

# Plot 1-E-2-P: Boxplot of number of disabled passengers ============
plt.e.2.p <- ggplot(info, mapping = aes(x = 0, y = 장애 / 10000)) + geom_boxplot() +
  labs(title = "지하철역별 연간 장애인 승객 수",
       y = "장애인 승객 수 (만명)",
       caption = "출처: 서울교통공사, 서울시청 도시교통과") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Highlight outliers
find_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

info$outlier.num <- find_outlier(info$장애)
plt.e.2.p <- plt.e.2.p + geom_text_repel(size = 3,
                                         data = info %>% filter(outlier.num == T),
                                         label = paste(info[info$outlier.num == T,]$역명,
                                                       ":", round(info[info$outlier.num == T,]$장애 / 10000, 1), "만명"))

print(plt.e.2.p)
ggsave(filename = "plot/1-E-2-P.png", plot = plt.e.2.p,
       height = 6, width = 4.5)

# Plot 1-E-2-Q: Q-Q Plot of proportion of disabled passengers ============
plt.e.2.q <- ggplot(info, mapping = aes(sample = 장애 / 10000)) + geom_qq() +
  geom_qq_line() +
  labs(title = "지하철역별 장애인 승객 수의 Q-Q Plot",
       y = "장애인 승객 수 (만명)",
       caption = "출처: 서울교통공사, 서울시청 도시교통과")

print(plt.e.2.q)
ggsave(filename = "plot/1-E-2-Q.png", plot = plt.e.2.q,
       height = 5, width = 5)

# Plot 1-T-1-1: Scatter Plot of disabled proportion and total facilities ============
metro.palette <- c("#0052A4", "#00A84D", "#EF7C1C", "#00A5DE", "#996CAC", "#CD7C2F", "#747F00", "#E6186C")
plt.t.1.1 <- ggplot(info, mapping = aes(x = ratio * 100, y = facility.total, group = "호선")) +
  geom_point(aes(color = 호선)) +
  scale_color_manual(values = metro.palette) +
  labs(title = "장애인 승객 비율과 장애인 편의시설 개수와의 관계",
       caption = "출처: 서울교통공사, 서울시청 도시교통과",
       x = "장애인 승객 비율(%)",
       y = "장애인 편의시설 개수")

print(plt.t.1.1)
ggsave(filename = "plot/1-T-1-1.png", plot = plt.t.1.1,
       height = 6, width = 8)

# Plot 1-T-3-1: Scatter Plot of number of disabled and total facilities ============
plt.t.3.1 <- ggplot(info, mapping = aes(x = 장애 / 10000, y = facility.total)) +
  geom_point(aes(color = 호선)) +
  scale_color_manual(values = metro.palette) +
  labs(title = "연간 장애인 승객 수와 장애인 편의시설 개수와의 관계",
       caption = "출처: 서울교통공사, 서울시청 도시교통과",
       x = "장애인 승객 수(만명)",
       y = "장애인 편의시설 개수")

print(plt.t.3.1)
ggsave(filename = "plot/1-T-3-1.png", plot = plt.t.3.1,
       height = 6, width = 8)


# ======== End of Script ====================