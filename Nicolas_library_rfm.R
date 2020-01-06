library(rfm)
library(knitr)
library(kableExtra)
library(magrittr)
library(dplyr)
library(ggplot2)
library(DT)
library(grDevices)
library(RColorBrewer)
options(knitr.table.format = 'html')
options(tibble.width = Inf)


csvAll<-read.csv("V:/Commercial/ID_MARKET/DAM - Data/Analyses Ponctuelles/GRETA/dfAmount.csv",encoding="UTF-8",sep=";", stringsAsFactors = FALSE, dec=".")
csvAll  <- csvAll%>%filter(New.Customer==1)


csvAllSe<-csvAll%>%select(User.id,Order.ID,Timestamp,Amount)%>%group_by(Order.ID,User.id,Timestamp)%>%
  summarise(Amount=mean(Amount))%>%ungroup()%>%select(User.id,Timestamp,Amount)
csvAllSe$Timestamp<-as.Date(csvAllSe$Timestamp)
analysis_date <- lubridate::as_date("2019-12-01", tz = "UTC")
rfm_result <-rfm_table_order(csvAllSe, User.id, Timestamp, Amount, analysis_date, frequency_bins = 5)
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")

recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)

segments<-rfm_segment(rfm_result, segment_names, recency_lower, recency_upper,
                      frequency_lower, frequency_upper, monetary_lower, monetary_upper)

segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

rfm_heatmap(rfm_result)
rfm_bar_chart(rfm_result)
rfm_histograms(rfm_result)
rfm_order_dist(rfm_result)
rfm_rm_plot(rfm_result)

