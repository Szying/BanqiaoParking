# install.packages("leaflet")
# install.packages("dplyr")
# install.packages("jsonlite")
library("dplyr")
library("jsonlite")
library("leaflet")


setwd("d:/Dropbox/R/avaPark/")

dtp <- c("p1708", "p1712", "p1716", "p1720", "p1723",
         "p1808", "p1812", "p1816", "p1820", "p1823",
         "p1908", "p1912", "p1916", "p1920", "p1923")
temp = list.files(pattern = "*.csv")

for(i in 1:length(temp)) 
  assign(dtp[i], read.csv(temp[i],
                          fileEncoding = "BIG-5",
                          stringsAsFactors = F))

# 新北市路外公共停車場資訊
url <- 'http://data.ntpc.gov.tw/api/v1/rest/datastore/382000000A-000225-002'
park <- fromJSON(url)
park <- as.data.frame(park[[2]]$records)
# 重新進行排序
park <- park[with(park, order(ID)), ]
# 過濾掉重複的ID車場
park <- park[!duplicated(park[, 'ID']), ]
# park <- park[which(park$AREA == "板橋區"), ]
park$ID <- as.integer(park$ID)

# 過濾掉重複的ID車場以及未提供即時剩餘車位的資訊
p1708 <- p1708[!duplicated(p1708[, 'ID']), ]
p1708 <- p1708[which(p1708$X1708 != -9),]

p1712 <- p1712[!duplicated(p1712[, 'ID']), ]
p1712 <- p1712[which(p1712$X1712 != -9),]

p1716 <- p1716[!duplicated(p1716[, 'ID']), ]
p1716 <- p1716[which(p1716$X1716 != -9),]

p1720 <- p1720[!duplicated(p1720[, 'ID']), ]
p1720 <- p1720[which(p1720$X1720 != -9),]

p1723 <- p1723[!duplicated(p1723[, 'ID']), ]
p1723 <- p1723[which(p1723$X1723 != -9),]

p1808 <- p1808[!duplicated(p1808[, 'ID']), ]
p1808 <- p1808[which(p1808$X1808 != -9),]

p1812 <- p1812[!duplicated(p1812[, 'ID']), ]
p1812 <- p1812[which(p1812$X1812 != -9),]

p1816 <- p1816[!duplicated(p1816[, 'ID']), ]
p1816 <- p1816[which(p1816$X1816 != -9),]

p1820 <- p1820[!duplicated(p1820[, 'ID']), ]
p1820 <- p1820[which(p1820$X1820 != -9),]

p1823 <- p1823[!duplicated(p1823[, 'ID']), ]
p1823 <- p1823[which(p1823$X1823 != -9),]

p1908 <- p1908[!duplicated(p1908[, 'ID']), ]
p1908 <- p1908[which(p1908$X1908 != -9),]

p1912 <- p1912[!duplicated(p1912[, 'ID']), ]
p1912 <- p1912[which(p1912$X1912 != -9),]

p1916 <- p1916[!duplicated(p1916[, 'ID']), ]
p1916 <- p1916[which(p1916$X1916 != -9),]

p1920 <- p1920[!duplicated(p1920[, 'ID']), ]
p1920 <- p1920[which(p1920$X1920 != -9),]

p1923 <- p1923[!duplicated(p1923[, 'ID']), ]
p1923 <- p1923[which(p1923$X1923 != -9),]

source("TWD97TM2toWGS84.R")
WGS84 <- TWD97TM2toWGS84(park$TW97X, park$TW97Y)

ptable <- 
  park %>%
  mutate(WGS84X = WGS84$lat) %>%
  mutate(WGS84Y = WGS84$lon) %>%
  merge(p1708, by = "ID", all = FALSE) %>%
  merge(p1712, by = "ID", all = FALSE) %>%
  merge(p1716, by = "ID", all = FALSE) %>%
  merge(p1720, by = "ID", all = FALSE) %>%
  merge(p1723, by = "ID", all = FALSE) %>%
  merge(p1808, by = "ID", all = FALSE) %>%
  merge(p1812, by = "ID", all = FALSE) %>%
  merge(p1816, by = "ID", all = FALSE) %>%
  merge(p1820, by = "ID", all = FALSE) %>%
  merge(p1823, by = "ID", all = FALSE) %>%
  merge(p1908, by = "ID", all = FALSE) %>%
  merge(p1912, by = "ID", all = FALSE) %>%
  merge(p1916, by = "ID", all = FALSE) %>%
  merge(p1920, by = "ID", all = FALSE) %>%
  merge(p1923, by = "ID", all = FALSE) 


# 整併後的資料寫到CSV檔中
# write.csv(ptable, file = 'ptable.csv', row.names = F)
ptc <- as.numeric(ifelse(is.na(ptable$TOTALCAR), 0, ptable$TOTALCAR))

pal <- colorBin(palette = c("blue", "pink", "red", "purple"),
                domain = ptc, 
                bin = c(0, 100, 200, 300, 100000),
                pretty = TRUE,
                na.color = "white", 
                alpha = F)

map <- 
  leaflet() %>%
  addTiles() %>%  # 加上預設的地圖資料
  addCircles(lat = ptable$WGS84X, 
                   lng = ptable$WGS84Y, 
                   radius = ifelse(ptable$TOTALCAR < 100, 10,
                            ifelse(ptable$TOTALCAR < 200, 15,
                            ifelse(ptable$TOTALCAR < 300, 20, 25))),
                   popup = paste("名稱", ptable$NAME, "<br>",
                                 "車位數量:", ptable$TOTALCAR, "<br>",
                                 "地址:", ptable$ADDRESS),
                   color = pal(ptc))
map  # 繪製地圖

