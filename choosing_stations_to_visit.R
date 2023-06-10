a <- read.csv("seoul_metro_disabled_info.csv")
a$hap <- a$ratio + a$neigh.ratio
a$divide <- a$ratio / a$neigh.ratio
head(a[order(a$hap, decreasing = T),])
head(a[order(a$divide),])