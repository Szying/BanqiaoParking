setwd("D:/Dropbox/R/park")
getwd()
park <- read.csv(file="ptable.csv", fileEncoding = "BIG-5",stringsAsFactors = F)

library(dplyr)

colnames(park)
park1 <- park[which(park$TOTALCAR > park$X1708&
                    park$TOTALCAR > park$X1712& 
                    park$TOTALCAR > park$X1716& 
                    park$TOTALCAR > park$X1720& 
                    park$TOTALCAR > park$X1723& 
                    park$TOTALCAR > park$X1808& 
                    park$TOTALCAR > park$X1812& 
                    park$TOTALCAR > park$X1816& 
                    park$TOTALCAR > park$X1820& 
                    park$TOTALCAR > park$X1823& 
                    park$TOTALCAR > park$X1908& 
                    park$TOTALCAR > park$X1912& 
                    park$TOTALCAR > park$X1916& 
                    park$TOTALCAR > park$X1920&
                    park$TOTALCAR > park$X1923
                    ),]

for(i in 1 : nrow(park1)){
  park1$used_x1708[i] = park1$TOTALCAR[i] - park1$X1708[i]
  park1$used_x1712[i] = park1$TOTALCAR[i] - park1$X1712[i]
  park1$used_x1716[i] = park1$TOTALCAR[i] - park1$X1716[i]
  park1$used_x1720[i] = park1$TOTALCAR[i] - park1$X1720[i]
  park1$used_x1723[i] = park1$TOTALCAR[i] - park1$X1723[i]
  park1$used_x1808[i] = park1$TOTALCAR[i] - park1$X1808[i]
  park1$used_x1812[i] = park1$TOTALCAR[i] - park1$X1812[i]
  park1$used_x1816[i] = park1$TOTALCAR[i] - park1$X1816[i]
  park1$used_x1820[i] = park1$TOTALCAR[i] - park1$X1820[i]
  park1$used_x1823[i] = park1$TOTALCAR[i] - park1$X1823[i]
  park1$used_x1908[i] = park1$TOTALCAR[i] - park1$X1908[i]
  park1$used_x1912[i] = park1$TOTALCAR[i] - park1$X1912[i]
  park1$used_x1916[i] = park1$TOTALCAR[i] - park1$X1916[i]
  park1$used_x1920[i] = park1$TOTALCAR[i] - park1$X1920[i]
  park1$used_x1923[i] = park1$TOTALCAR[i] - park1$X1923[i]
  
  park1$UsedRate_x1708[i] = park1$used_x1708[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1712[i] = park1$used_x1712[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1716[i] = park1$used_x1716[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1720[i] = park1$used_x1720[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1723[i] = park1$used_x1723[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1808[i] = park1$used_x1808[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1812[i] = park1$used_x1812[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1816[i] = park1$used_x1816[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1820[i] = park1$used_x1820[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1823[i] = park1$used_x1823[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1908[i] = park1$used_x1908[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1912[i] = park1$used_x1912[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1916[i] = park1$used_x1916[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1920[i] = park1$used_x1920[i] / park1$TOTALCAR[i]
  park1$UsedRate_x1923[i] = park1$used_x1923[i] / park1$TOTALCAR[i]
}

colnames(park1)

#2.1比較板橋區不同類型停車場的平均使用率
park1 %>% 
  filter(AREA == "板橋區")%>%
  select(
    SUMMARY, UsedRate_x1708, UsedRate_x1712, UsedRate_x1716, UsedRate_x1720, UsedRate_x1723, 
    UsedRate_x1808, UsedRate_x1812, UsedRate_x1816, UsedRate_x1820, UsedRate_x1823, 
    UsedRate_x1908, UsedRate_x1912, UsedRate_x1916, UsedRate_x1920, UsedRate_x1923
  ) %>%
  group_by(SUMMARY) %>%
  summarise(
    "AVRUR" = mean(c(UsedRate_x1708, UsedRate_x1712, UsedRate_x1716, UsedRate_x1720, UsedRate_x1723, 
                     UsedRate_x1808, UsedRate_x1812, UsedRate_x1816, UsedRate_x1820, UsedRate_x1823, 
                     UsedRate_x1908, UsedRate_x1912, UsedRate_x1916, UsedRate_x1920, UsedRate_x1923
                     )
                  )
  )

#2.2板橋區不同時間點(0800, 1200, 1600, 2000, 2300)的平均使用率
park1 %>% 
  filter(AREA == "板橋區")%>%
  select(UsedRate_x1708, UsedRate_x1712, UsedRate_x1716, UsedRate_x1720, UsedRate_x1723, 
         UsedRate_x1808, UsedRate_x1812, UsedRate_x1816, UsedRate_x1820, UsedRate_x1823, 
         UsedRate_x1908, UsedRate_x1912, UsedRate_x1916, UsedRate_x1920, UsedRate_x1923
  ) %>%
  summarise(
    "0800_AVRUR" = mean(c(UsedRate_x1708,UsedRate_x1808,UsedRate_x1908)),
    "1200_AVRUR" = mean(c(UsedRate_x1712,UsedRate_x1812,UsedRate_x1912)),
    "1600_AVRUR" = mean(c(UsedRate_x1716,UsedRate_x1816,UsedRate_x1916)),
    "2000_AVRUR" = mean(c(UsedRate_x1720,UsedRate_x1820,UsedRate_x1920)),
    "2300_AVRUR" = mean(c(UsedRate_x1723,UsedRate_x1823,UsedRate_x1923))
  )

#2.3比較板橋區不同類型停車場 & 不同時間點的平均使用率
park1 %>% 
  filter(AREA == "板橋區")%>%
  select(SUMMARY, UsedRate_x1708, UsedRate_x1712, UsedRate_x1716, UsedRate_x1720, UsedRate_x1723, 
         UsedRate_x1808, UsedRate_x1812, UsedRate_x1816, UsedRate_x1820, UsedRate_x1823, 
         UsedRate_x1908, UsedRate_x1912, UsedRate_x1916, UsedRate_x1920, UsedRate_x1923
  ) %>%
  group_by(SUMMARY) %>%
  summarise("AllTime_AVRTU" = mean(c(
    UsedRate_x1708, UsedRate_x1712, UsedRate_x1716, UsedRate_x1720, UsedRate_x1723, 
    UsedRate_x1808, UsedRate_x1812, UsedRate_x1816, UsedRate_x1820, UsedRate_x1823, 
    UsedRate_x1908, UsedRate_x1912, UsedRate_x1916, UsedRate_x1920, UsedRate_x1923
  )
  ),
  "0800_AVRTU" = mean(c(UsedRate_x1708,UsedRate_x1808,UsedRate_x1908)),
  "1200_AVRTU" = mean(c(UsedRate_x1712,UsedRate_x1812,UsedRate_x1912)),
  "1600_AVRTU" = mean(c(UsedRate_x1716,UsedRate_x1816,UsedRate_x1916)),
  "2000_AVRTU" = mean(c(UsedRate_x1720,UsedRate_x1820,UsedRate_x1920)),
  "2300_AVRTU" = mean(c(UsedRate_x1723,UsedRate_x1823,UsedRate_x1923))
  )

# 整併後的資料寫到CSV檔中
write.csv(park1, file = 'ptable_usedRate.csv', row.names = F)
