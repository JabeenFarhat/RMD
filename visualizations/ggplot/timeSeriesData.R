#
#Install and load the following packages
library(quantmod)
library(tidyr)
library(ggplot2)

#Decide the start and end date for stock prices.
start = as.Date("2021-01-01")
end = as.Date("2022-01-25")

#Get data: company ticker, source of data, period
getSymbols(c("CM", "VB", "GLS", "DB"), src = "yahoo", from = start, to = end)

head(CM)
head(VB)
head(GLS)
head(DB)


#create dataframe with adjusted price column for each bank. xts to transform data into time series
stocks <- as.xts(data.frame(CM = CM[, "CM.Adjusted"], VB = VB[, "VB.Adjusted"], GLS = GLS[, "GLS.Adjusted"], DB = DB[, "DB.Adjusted"]))

head(stocks)
names(stocks) = c("Kommerzbank", "Volksbank", "GLS", "Deutsche Bank")
index(stocks) = as.Date(index(stocks))#read the index column as dates}

#Check that everything looks alright.
head(stocks)

#Create visualization using ggplot.
stocks_series = tidy(stocks) %>%
  ggplot(aes(x = index, y = value, color = series)) + geom_line() + facet_grid(series ~., scales = "free") + theme_bw() + labs(title = "German Banks: Daily Stock exchange prices (Jan. 2021 - Jan. 2022)", subtitle = "Adjusted Prices") + xlab("Date") + ylab("Price") +scale_color_manual(values = c("#cd1409", "azure4", "#000000", "003B70"))

stocks_series





















