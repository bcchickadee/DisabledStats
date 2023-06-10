# 주제탐구세미나 1 (007) 기말과제 발표 R script
# 권유리, 윤수민, 조진혁, 최윤희

# ======== Basic Info ====================
# IMPORTANT: Please open this document in UTF-8 format.

# This file serves the following purpose:
# Calculating the correlation between...
# (number / ratio of disabled in metro station usage) and (number / ratio of disabled by neighborhood (동))
# Testing the relevancy of correlation

# Data range: All of 2022

# Author of this script: 윤수민

# ======== Installing / Opening Dependencies ====================
install.packages("dplyr"); install.packages("ggplot2"); install.packages("geosphere"); install.packages("OpenStreetMap"); install.packages("ggrepel"); install.packages("MASS")
library(dplyr); library(ggplot2); library(geosphere); library(OpenStreetMap); library(ggrepel); library(MASS)


# ======== Importing Data ====================
# metro.info: Metro station usage data
# Calculated from disabled_facility_corr.R file (for more details refer to disabled_facility_corr.R)
metro.info <- read.csv("disabled_facility_information_by_station.csv",
                        fileEncoding = "utf-8", header = T)

# region: Number of disabled by neighborhood
# Data Source: 서울특별시청 복지정책실 복지기획관 장애인자립지원과
# https://data.seoul.go.kr/dataList/10577/S/2/datasetView.do
region <- read.csv("raw_data/장애인+현황(장애유형별_동별)_20230523173036.csv",
                   header = T)

# pop: Total population by neighborhood (2022-4th quarter)
# Data Source: 서울특별시청 디지털정책관 빅데이터담당관
# https://data.seoul.go.kr/dataList/10043/S/2/datasetView.do
pop <- read.csv("raw_data/주민등록인구_20230527140838.csv",
                header = T)

# Mapping metro stations w/ neighborhood
# We will map the metro station to neighborhoods in order to find the neighborhood closest to the station
# metro.coord: Metro station coordinates data
# Data source: 서울특별시 도시교통실 교통기획관 미래첨단교통과
# https://data.seoul.go.kr/dataList/OA-21232/S/1/datasetView.do
metro.coord <- read.csv("raw_data/서울시 역사마스터 정보.csv",
                        fileEncoding = "euc-kr", header = T)

# neigh.coord: 행정동 coordinate data
# Data source: Open Source Data from skyseven73, torrms blog
# https://torrms.tistory.com/55
# https://skyseven73.tistory.com/23
neigh.coord <- read.csv("raw_data/행정구역별_위경도_좌표.csv",
                        header = T)
neigh.coord.2 <- read.csv("raw_data/행정_법정동 중심좌표_refined.csv", header = T)

# ======== Tidying Data ====================

# Filtering neighborhood coordinate data to only 행정동 ==========
neigh.coord <- neigh.coord[neigh.coord$읍면동.구 != "",]
neigh.coord$real <- NA
for (i in 1:length(neigh.coord$시도)) {
  temp.ind <- which(neigh.coord.2$시도 == neigh.coord[i, "시도"] &
                      neigh.coord.2$시군구 == neigh.coord[i, "시군구"] &
                      neigh.coord.2$읍면동 == neigh.coord[i, "읍면동.구"])
  if (length(temp.ind) > 1) {
    neigh.coord[i, "real"] <- "H"
  } else {
    neigh.coord[i, "real"] <- neigh.coord.2[temp.ind, "코드종류"]
  }
}
neigh.coord <- neigh.coord[neigh.coord$real == "H",]

# Adding coordinate data in metro.info ==========
# Filter only coordinates that are present in metro.info
metro.coord <- metro.coord[paste(metro.coord$역사명, metro.coord$호선) %in% paste(metro.info$역명, metro.info$호선),]
metro.info <- metro.info[paste(metro.info$역명, metro.info$호선) %in% paste(metro.coord$역사명, metro.coord$호선),]
metro.coord <- metro.coord[metro.coord$역사_ID != 9995 & metro.coord$역사_ID != 9996,]
metro.coord <- metro.coord[order(metro.coord$역사_ID),]
metro.info <- metro.info[order(metro.info$역번호),]

metro.info$lat <- metro.coord$위도
metro.info$long <- metro.coord$경도


# Calculating nearest neighborhood to given station ==========
metro.info$district <- NA; metro.info$neighborhood <- NA; metro.info$neigh.lat <- NA; metro.info$neigh.long <- NA
for (i in 1:length(metro.info$X.1)) {
  temp.lat <- metro.info[i, "lat"]; temp.long <- metro.info[i, "long"]
  temp.dist <- distm(c(temp.long, temp.lat), neigh.coord[,c("경도", "위도")])
  temp.ind <- which.min(temp.dist)
  metro.info[i, "district"] <- neigh.coord[temp.ind, "시군구"]
  metro.info[i, "neighborhood"] <- neigh.coord[temp.ind, "읍면동.구"]
  metro.info[i, "neigh.lat"] <- neigh.coord[temp.ind, "위도"]
  metro.info[i, "neigh.long"] <- neigh.coord[temp.ind, "경도"]
}

# For now, export this file
write.csv(metro.info, "raw_data/metro_info_w_coord.csv")
# For quick open: open file directly
metro.info <- read.csv("raw_data/metro_info_w_coord.csv", header = T)

# Calculating ratio of disabled population by linking pop dataset and region dataset ==========
region <- region[region$동별.2.!= "소계" & region$동별.3.!= "기타",]
region <- region[order(region$동별.2., region$동별.3.),]
pop <- pop[order(pop$동별.2., pop$동별.3.),]

region$pop <- pop$소계
region$ratio <- region$계 / region$pop

# Adding coordinate data to region dataset ==========
region.wcoord <- region
region.wcoord$lat <- NA
region.wcoord$long <- NA
for (i in 1:length(region.wcoord$lat)) {
  temp.ind <- which(neigh.coord$시군구 == region.wcoord[i, "동별.2."] & neigh.coord$읍면동.구 == region.wcoord[i, "동별.3."])
  if (length(neigh.coord[temp.ind, 1]) != 0) {
    region.wcoord[i, "lat"] <- neigh.coord[temp.ind, "위도"]
    region.wcoord[i, "long"] <- neigh.coord[temp.ind, "경도"] 
  }
}
region.wcoord <- region.wcoord[which(!is.na(region.wcoord$lat)),]

# For now, export this file
write.csv(region, "raw_data/number_ratio_of_disables_by_neighborhood.csv")
write.csv(region.wcoord, "raw_data/number_ratio_of_disables_by_neighborhood_w_coord.csv")
# For quick open: open file directly
region <- read.csv("raw_data/number_ratio_of_disables_by_neighborhood.csv", header = T)
region.wcoord <- read.csv("raw_data/number_ratio_of_disables_by_neighborhood_w_coord.csv", header = T)

# Connecting region dataset to metro stations ==========
metro.region <- metro.info
metro.region$neigh.ratio <- NA
metro.region$neigh.disabled <- NA

# Remove non-Seoul data
metro.region.seoul <- metro.region[
  paste(metro.region$district, metro.region$neighborhood) %in% paste(region$동별.2., region$동별.3.),]

for (i in 1:length(metro.region.seoul$total)) {
  temp.ind <- which(region$동별.2. == metro.region.seoul[i, "district"] &
                      region$동별.3. == metro.region.seoul[i, "neighborhood"])
  temp.neigh.ratio <- region[temp.ind, "ratio"]
  metro.region.seoul[i, "neigh.ratio"] <- temp.neigh.ratio
  temp.neigh.disabled <- region[temp.ind, "계"]
  metro.region.seoul[i, "neigh.disabled"] <- temp.neigh.disabled
  }

# For now, export this data
write.csv(metro.region.seoul, "seoul_metro_disabled_info.csv")
# For quick open: open file directly
metro.region.seoul <- read.csv("seoul_metro_disabled_info.csv", header = T)


# ======== Drawing Insights ====================

# ======== Examining Data ====================

# Examination 1. Examining proportion of disabled by neighborhood ==========
region.neigh <- region[region$동별.3. != "소계",]
boxplot(region.neigh$ratio,
        ylab = "동별 장애인 거주 비율") # Detailed Visualization @ Plot 2-E-1-P
mean(region.neigh$ratio); sd(region.neigh$ratio) # mean: 4.110959%, sd: 1.671389%
summary(region.neigh$ratio)
head(region.neigh[order(region.neigh$ratio, decreasing = T),
          c("동별.2.", "동별.3.", "ratio")]) # Top neighborhoods: 가양2동, 수서동, 등촌3동, 번3동

# Does the ratio follow a normal distribution?
qqnorm(region.neigh$ratio) # Detailed Visualization @ Plot 2-E-1-Q
qqline(region.neigh$ratio)
shapiro.test(region.neigh$ratio) # p-value < 0.001, doesn't follow normal dist


# Examination 2. Examining number of disabled passengers ==========
# Exclude totals
boxplot(region.neigh$계,
        ylab = "동별 장애인 승차 승객 수") # Detailed Visualization @ Plot 2-E-2-P
mean(region.neigh$계); sd(region.neigh$계) # mean: 919.8239, sd: 490.4686
summary(region.neigh$계)
head(region.neigh[order(region.neigh$계, decreasing = T),
          c("동별.2.", "동별.3.", "계")]) # Top neighborhoods: 등촌3동, 중계2.3동, 가양2동

# Does the ratio follow a normal distribution?
qqnorm(region.neigh$계) # Detailed Visualization @ Plot 2-E-2-Q
qqline(region.neigh$계)
shapiro.test(region.neigh$계) # p-value < 0.001, doesn't follow normal dist


# ======== Testing Data ====================

# Test 1. Relationship between (proportion of disabled in neighborhood) and (proportion of disabled in metro) ==========
# Test 1-1. Basic Correlation Data
plot(metro.region.seoul$neigh.ratio, metro.region.seoul$ratio) # Detailed Visualization @ Plot 2-T-1
cor(metro.region.seoul$neigh.ratio, metro.region.seoul$ratio) # Corr: 0.5403225
cor.test(metro.region.seoul$neigh.ratio, metro.region.seoul$ratio) # p-value < 2.2-e16***, can rule out cor is 0
# Conclusion: Relationship observed (reject null hypothesis that cor is 0)

# Test 1-2. Examining simple linear regression model
summary(lm(ratio ~ neigh.ratio, data = metro.region.seoul))
# p value of beta is < 2e-16 : reject null hypothesis (regression is significant)
# Regression Line: 0.35579 x (neigh.ratio) + 0.01311

# Test 2. Relationship between (number of disabled in neighborhood) and (number of disabled in metro) ==========
# Test 2-1. Basic Correlation Data
plot(metro.region.seoul$neigh.disabled, metro.region.seoul$장애 / 10000) # Detailed Visualization @ Plot 2-T-2
cor(metro.region.seoul$neigh.disabled, metro.region.seoul$장애) # Corr: 0.02632843
cor.test(metro.region.seoul$neigh.disabled, metro.region.seoul$장애) # p-value: 0.6714, cannot rule out that corr is 0
# Conclusion: No relationship observed
# May be due to big transfer stations

# Test 2-2. Examining simple linear regression model
summary(lm(장애 ~ neigh.disabled, data = metro.region.seoul))
# p value of beta is 0.671: accept null hypothesis (regression is insignificant)

# Test 3. Relationship between (number of disabled in neighborhood) and (ratio of disabled in metro) ==========
# Test 3-1. Basic Correlation Data
plot(metro.region.seoul$neigh.disabled, metro.region.seoul$ratio * 100) # Detailed Visualization @ Plot 2-T-3
cor(metro.region.seoul$neigh.disabled, metro.region.seoul$ratio) # Corr: 0.4295865
cor.test(metro.region.seoul$neigh.disabled, metro.region.seoul$ratio) # p-value: 3.435e-13, can rule out that corr is 0
# Conclusion: Relationship observed (reject null hypothesis that cor is 0)

# Test 3-2. Examining simple linear regression model
summary(lm(ratio*100 ~ neigh.disabled, data = metro.region.seoul))
# p value of beta is 3.44e-13: reject null hypothesis (regression is significant)
# Regression Line: (0.0008058) x (neigh.disabled) + 1.9634008 (slope very flat)

# Test 4. Relationship between (ratio of disabled in neighborhood) and (number of disabled in metro) ==========
# Test 3-1. Basic Correlation Data
plot(metro.region.seoul$neigh.ratio * 100, metro.region.seoul$장애 / 10000) # Detailed Visualization @ Plot 2-T-4
cor(metro.region.seoul$neigh.ratio, metro.region.seoul$장애) # Corr: 0.1242039
cor.test(metro.region.seoul$neigh.ratio, metro.region.seoul$장애) # p-value: 0.04458, can rule out that corr is 0
# Conclusion: Relationship observed (reject null hypothesis that cor is 0)

# Test 4-2. Examining simple linear regression model
summary(lm(장애/100 ~ neigh.ratio, data = metro.region.seoul))
# p value of beta is 0.0446: reject null hypothesis (regression is significant)
# Regression Line: (6989.8) x (neigh.ratio(%)) + 996.3



# ======== General Conclusion ====================
# The installation of facilities for the disabled has no relationship with the number of disabled,
# neither does it have a relation with the proportion of disabled passengers (in a given metro station).
# This implies that the local government lacks consideration over the true status quo of disabled passengers,
# when installing transport facilities for the disabled.


# ======== Detailed Visualization w/ggplot2 ====================
# Plot 2-E-1-P: Boxplot of proportion of disabled in neighborhood ============
plt.e.1.p <- ggplot(region.neigh, mapping = aes(x = 0, y = ratio * 100)) + geom_boxplot() +
  labs(title = "동별 장애인 거주 비율",
       y = "거주민 중 장애인 비율 (%)",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Highlight outliers
find_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

region.neigh$outlier.ratio <- find_outlier(region.neigh$ratio)
plt.e.1.p <- plt.e.1.p + geom_text_repel(size = 4,
                                         data = region.neigh %>% filter(outlier.ratio == T),
                                         label = paste(region.neigh[region.neigh$outlier.ratio == T,]$동별.3.,
                                                       ":", round(region.neigh[region.neigh$outlier.ratio == T,]$ratio * 100, 4), "%"))

print(plt.e.1.p)
ggsave(filename = "plot/2-E-1-P.png", plot = plt.e.1.p,
       height = 6, width = 4.5)
# TODO: Highlight outliers with neighborhood name and ratio

# Plot 2-E-1-Q: Q-Q Plot of proportion of disabled in neighborhood ============
plt.e.1.q <- ggplot(region.neigh, mapping = aes(sample = ratio * 100)) + geom_qq() +
  geom_qq_line() +
  labs(title = "동별 장애인 비율의 Q-Q Plot",
       y = "거주민 중 장애인 비율 (%)",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관")

print(plt.e.1.q)
ggsave(filename = "plot/2-E-1-Q.png", plot = plt.e.1.q,
       height = 5, width = 5)

# Plot 2-E-2-P: Boxplot of number of disabled in neighborhood ============
plt.e.2.p <- ggplot(region.neigh, mapping = aes(x = 0, y = 계 / 1000)) + geom_boxplot() +
  labs(title = "동별 장애인 거주민 수",
       y = "거주 장애인 수 (천명)",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Highlight outliers
find_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

region.neigh$outlier.num <- find_outlier(region.neigh$계)
plt.e.2.p <- plt.e.2.p + geom_text_repel(size = 3,
                                         data = region.neigh %>% filter(outlier.num == T),
                                         label = paste(region.neigh[region.neigh$outlier.num == T,]$동별.3.,
                                                       ":", round(region.neigh[region.neigh$outlier.num == T,]$계 / 1000, 1), "천명"))

print(plt.e.2.p)
ggsave(filename = "plot/2-E-2-P.png", plot = plt.e.2.p,
       height = 6, width = 4.5)

# Plot 2-E-2-Q: Q-Q Plot of proportion of disabled passengers ============
plt.e.2.q <- ggplot(region.neigh, mapping = aes(sample = 계 / 1000)) + geom_qq() +
  geom_qq_line() +
  labs(title = "동별 장애인 거주민 수의 Q-Q Plot",
       y = "거주 장애인 수 (천명)",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관")

print(plt.e.2.q)
ggsave(filename = "plot/2-E-2-Q.png", plot = plt.e.2.q,
       height = 5, width = 5)

metro.palette <- c("#0052A4", "#00A84D", "#EF7C1C", "#00A5DE", "#996CAC", "#CD7C2F", "#747F00", "#E6186C")
# Plot 2-T-1: Scatter Plot of (proportion of disabled in neighborhood) and (proportion of disabled in metro) ============
plt.t.1 <- ggplot(metro.region.seoul, mapping = aes(x = neigh.ratio * 100, y = ratio * 100, group = 호선)) +
  geom_point(aes(color = 호선)) +
  scale_color_manual(values = metro.palette) +
  labs(title = "장애인 거주 비율과 인접 지하철역 장애인 승차 비율의 관계",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관,
       서울교통공사, 서울시청 도시교통과",
       x = "장애인 거주 비율(%)",
       y = "장애인 승차 비율(%)") +
  xlim(NA, 7.5) + ylim(NA, 6)

print(plt.t.1)
ggsave(filename = "plot/2-T-1.png", plot = plt.t.1,
       height = 6, width = 8)

# Plot 2-T-1-LM: Plot w/ linear regression ======
plt.t.1.wlm <- plt.t.1 + geom_smooth(aes(group = NA), method = "lm")
print(plt.t.1.wlm)
ggsave(filename = "plot/2-T-1-LM.png", plot = plt.t.1.wlm,
       height = 6, width = 8)


# Testing Linear Regression ========
# Plot 2-T-1-RES: Examine significance of linear regression by residual plot ====
plt.t.1.lm <- lm(ratio ~ neigh.ratio, data = metro.region.seoul)
plt.t.1.lm.studres <- data.frame(studres(plt.t.1.lm))

plt.t.1.res <- ggplot(data = plt.t.1.lm, mapping = aes(x = .fitted * 100, y = .resid / sd(.resid))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residual Plot of aforementioned Regression",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관,
       서울교통공사, 서울시청 도시교통과",
       x = "Fitted: 장애인 거주 비율(%)",
       y = "Studentized Residuals")

print(plt.t.1.res)
ggsave(filename = "plot/2-T-1-res.png", plot = plt.t.1.res,
       height = 6, width = 8)

# Plot 2-T-1-RES-Q: Q-Q Plot of residuals ====
plt.t.1.res.q <- ggplot(plt.t.1.lm.studres, mapping = aes(sample = studres.plt.t.1.lm.)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Q-Q Plot of Studentized Residuals",
       y = "Studentized Residuals")

print(plt.t.1.res.q)
ggsave(filename = "plot/2-T-1-res-Q.png", plot = plt.t.1.res.q,
       height = 5, width = 5)

# Shapiro-Wilk Test of residuals
shapiro.test(plt.t.1.lm.studres$studres.plt.t.1.lm.)
# p-value = 1.968e-11, does not follow normal dist

# Residuals do not follow normal dist: Inconclusive if linear regression fits data well
# R squared: 0.2919... moderate correlation
# Regression not strong.


# Plot 2-T-2: Scatter Plot of (number of disabled in neighborhood) and (number of disabled in metro) ============
plt.t.2 <- ggplot(metro.region.seoul, mapping = aes(x = neigh.disabled / 10000, y = 장애 / 10000, group = 호선)) +
  geom_point(aes(color = 호선)) +
  scale_color_manual(values = metro.palette) +
  labs(title = "장애인 거주자 수와 장애인 승객 수와의 관계",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관,
       서울교통공사, 서울시청 도시교통과",
       x = "장애인 거주자 수(만명)",
       y = "장애인 승객 수(만명)")

print(plt.t.2)
ggsave(filename = "plot/2-T-2.png", plot = plt.t.2,
       height = 6, width = 8)

# Plot 2-T-3: Scatter Plot of (number of disabled in neighborhood) and (ratio of disabled in metro) ============
plt.t.3 <- ggplot(metro.region.seoul, mapping = aes(x = neigh.disabled / 1000, y = ratio * 100, group = 호선)) +
  geom_point(aes(color = 호선)) +
  scale_color_manual(values = metro.palette) +
  labs(title = "장애인 거주자 수와 인접 지하철역 장애인 승차 비율의 관계",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관,
       서울교통공사, 서울시청 도시교통과",
       x = "장애인 거주자 수(천명)",
       y = "장애인 승차 비율(%)")

print(plt.t.3)
ggsave(filename = "plot/2-T-3.png", plot = plt.t.3,
       height = 6, width = 8)

# Plot 2-T-4: Scatter Plot of (ratio of disabled in neighborhood) and (number of disabled in metro) ============
plt.t.4 <- ggplot(metro.region.seoul, mapping = aes(x = neigh.ratio * 100, y = 장애 / 10000, group = 호선)) +
  geom_point(aes(color = 호선)) +
  scale_color_manual(values = metro.palette) +
  scale_y_log10() +
  labs(title = "장애인 거주자 비율과 인접 지하철역 장애인 승객 수와의 관계",
       subtitle = "Log scale",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관,
       서울교통공사, 서울시청 도시교통과",
       x = "장애인 거주 비율(%)",
       y = "장애인 승객 수(만명)")

print(plt.t.4)
ggsave(filename = "plot/2-T-4.png", plot = plt.t.4,
       height = 6, width = 8)

# BONUS Plot 1: Creating a Seoul Metro Map containing disabled passenger ratio / number data ============
corner.left.1 <- c(37.72, 126.77); corner.right.1 <- c(37.41, 127.22)
map.seoul.1 <- openmap(corner.left.1, corner.right.1, type = "osm")
map.metro <- openproj(map.seoul.1)

# Ratio Data
plt.metro.ratio <- autoplot.OpenStreetMap(map.metro) +
  geom_point(data = metro.region.seoul,
             mapping = aes(x = long, y = lat, color = 호선, size = ratio * 100)) +
  scale_color_manual(values = metro.palette) +
  labs(title = "역별 장애인 승객 비율",
       subtitle = "범위: 서울특별시 내 서울교통공사 관할 역 (1호선 ~ 8호선)",
       caption = "출처: 서울교통공사") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 6)),
         size = guide_legend(title = "장애인\n승객 비율(%)"))

print(plt.metro.ratio)
ggsave(filename = "plot/seoulmetro_ratio.png", plot = plt.metro.ratio,
       height = 6, width = 8)

# Number Data
plt.metro.num <- autoplot.OpenStreetMap(map.metro) +
  geom_point(data = metro.region.seoul,
             mapping = aes(x = long, y = lat, color = 호선, size = 장애 / 10000)) +
  scale_color_manual(values = metro.palette) +
  labs(title = "역별 장애인 승객 수",
       subtitle = "범위: 서울특별시 내 서울교통공사 관할 역 (1호선 ~ 8호선)",
       caption = "출처: 서울교통공사") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 6)),
         size = guide_legend(title = "장애인\n승객 수(만명)"))

print(plt.metro.num)
ggsave(filename = "plot/seoulmetro_num.png", plot = plt.metro.num,
       height = 6, width = 8)

# BONUS Plot 2: Creating a Seoul Map containing disabled ratio by neighborhood ============
corner.left.2 <- c(37.70, 126.77); corner.right.2 <- c(37.42, 127.19)
map.seoul.2 <- openmap(corner.left.2, corner.right.2, type = "osm")
map.seoul <- openproj(map.seoul.2)

# Ratio Data
plt.neigh.ratio <- autoplot.OpenStreetMap(map.seoul) +
  geom_point(data = region.wcoord,
             mapping = aes(x = long, y = lat, color = 동별.2., size = ratio * 100)) +
  labs(title = "동별 장애인 거주 비율",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5), title = "구"),
         size = guide_legend(title = "장애인\n거주 비율(%)"))

print(plt.neigh.ratio)
ggsave(filename = "plot/neighborhood_ratio.png", plot = plt.neigh.ratio,
       height = 6, width = 8)

# Ratio Data w/o districts
plt.neigh.ratio.wodist <- autoplot.OpenStreetMap(map.seoul) +
  geom_point(data = region.wcoord,
             mapping = aes(x = long, y = lat, color = ratio * 100, size = ratio * 100)) +
  labs(title = "동별 장애인 거주 비율",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  scale_color_gradient(low = "red", high = "yellow") +
  guides(color = guide_legend(override.aes = list(size = 5), title = "장애인\n거주 비율(%)"),
         size = guide_legend(title = "장애인\n거주 비율(%)"))

print(plt.neigh.ratio.wodist)
ggsave(filename = "plot/neighborhood_ratio_wodist.png", plot = plt.neigh.ratio.wodist,
       height = 6, width = 8)

# Number Data
plt.neigh.num <- autoplot.OpenStreetMap(map.seoul) +
  geom_point(data = region.wcoord,
             mapping = aes(x = long, y = lat, color = 동별.2., size = 계)) +
  labs(title = "동별 장애인 인구",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5), title = "구"),
         size = guide_legend(title = "장애인 수"))

print(plt.neigh.num)
ggsave(filename = "plot/neighborhood_num.png", plot = plt.neigh.num,
       height = 6, width = 8)

# Number Data w/o districts
plt.neigh.num.wodist <- autoplot.OpenStreetMap(map.seoul) +
  geom_point(data = region.wcoord,
             mapping = aes(x = long, y = lat, color = 계, size = 계)) +
  labs(title = "동별 장애인 인구",
       caption = "출처: 서울특별시청 복지정책실 복지기획관 장애인자립지원과,\n서울특별시청 디지털정책관 빅데이터담당관") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  scale_color_gradient(low = "red", high = "yellow") +
  guides(color = guide_legend(override.aes = list(size = 5), title = "장애인 수"),
         size = guide_legend(title = "장애인 수"))

print(plt.neigh.num.wodist)
ggsave(filename = "plot/neighborhood_num_wodist.png", plot = plt.neigh.num.wodist,
       height = 6, width = 8)

# ======== End of Script ====================