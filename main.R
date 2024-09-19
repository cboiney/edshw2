#install.packages(c("dplyr", "lubridate"))
library(dplyr)
library(lubridate)

#CLASSWORK

streamH <- read.csv("/cloud/project/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/site_info.csv")

#parsing our data (P2)
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz="America/New_York")

#join site info to stream gauge height (P1)
floods <- full_join(streamH, siteinfo, by="siteID")

#testing if type of join matters
floods1 <- left_join(streamH, siteinfo, by="siteID")
floods2 <- right_join(streamH, siteinfo, by="siteID")
#floods and both of these data frames are identical, type of join doesn't matter
#because streamH and siteinfo line up perfectly (no missing values and siteID
#matches row by row)

peace <- floods %>% 
  filter(siteID == 2295637)

example <- floods %>%
  filter(gheight.ft >= 10)

plot(peace$dateF, peace$gheight.ft, type ="l")

max.ht <- floods %>%
  group_by(names) %>%
  summarize(max_ht_ft=max(gheight.ft, na.rm = TRUE),
            mean_ht_ft=mean(gheight.ft, na.rm = TRUE))

#(P3)
earliest_days <- floods %>%
  filter(gheight.ft > flood.ft) %>%
  group_by(names) %>%
  summarize(date=min(dateF, na.rm = TRUE))


##HOMEWORK

streamH <- read.csv("/cloud/project/stream_gauge.csv")
siteinfo <- read.csv("/cloud/project/site_info.csv")

#parsing our data
streamH$dateF <- ymd_hm(streamH$datetime,
                        tz="America/New_York")

#join site info to stream gauge height
floods <- full_join(streamH, siteinfo, by="siteID")

#q1

palmdale <- floods %>%
  filter(siteID == 2256500)

plot(palmdale$dateF, palmdale$gheight.ft, type ="l", main = "Palmdale Heights",
     xlab = "Date", ylab = "Height (ft)")

peace <- floods %>%
  filter(siteID == 2295637)

plot(peace$dateF, peace$gheight.ft, type ="l", main = "Peace Heights",
     xlab = "Date", ylab = "Height (ft)")

santa_fe <- floods %>%
  filter(siteID == 2322500)

plot(santa_fe$dateF, santa_fe$gheight.ft, type ="l", main = "Santa Fe Heights",
     xlab = "Date", ylab = "Height (ft)")

trilby <- floods %>%
  filter(siteID == 2312000)

plot(trilby$dateF, trilby$gheight.ft, type ="l", main = "Trilby Heights",
     xlab = "Date", ylab = "Height (ft)")

#q2

#for each flood category, taking earliest date at which that height was exceeded
action_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft > action.ft) %>%
  summarize(date=min(dateF, na.rm = TRUE))

flood_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft > flood.ft) %>%
  summarize(date=min(dateF, na.rm = TRUE))

moderate_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft > moderate.ft) %>%
  summarize(date=min(dateF, na.rm = TRUE))

major_date <- floods %>%
  group_by(names) %>%
  filter(gheight.ft > major.ft) %>%
  summarize(date=min(dateF, na.rm = TRUE))


turning_points1 <- full_join(action_date, flood_date,
                             by="names")

turning_points2 <- full_join(moderate_date, major_date, 
                             by="names")

#combining all 4 first occurrences (turning points) into one data frame
turning_points <- full_join(turning_points1, turning_points2, 
                            by="names")

#Cite: https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/#google_vignette
#syntax to change the name of a column

names(turning_points)[1] = "names"
names(turning_points)[2] = "action"
names(turning_points)[3] = "flood"
names(turning_points)[4] = "moderate"
names(turning_points)[5] = "major"

#q3

#Looking at the rows for each river where stream gauge exceeded the requirements
#for a major flood
most_severe <- floods %>%
  group_by(names) %>%
  filter(gheight.ft > major.ft) %>%
  summarize(peak = max(gheight.ft, na.rm = TRUE),
            major = max(major.ft, na.rm = TRUE))

most_severe$diff <- most_severe$peak - most_severe$major


