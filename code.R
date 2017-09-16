install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library("ggplot2")

#Function that will handle loading of the data from the 30 csv files in the given folder and return the compiled dataset
loadStockData <- function(directory){
  file_list <- list.files(path=directory, pattern="*.csv")
  for (file in file_list){
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read.table(paste(directory,file,sep=''), header=TRUE, sep=",")
      #Insert the ticker into the dataset using the file name
      dataset <- cbind(Ticker = sym <- strsplit(file,"\\.")[[1]][1], dataset)
    }
    # if the merged dataset does exist, append to it
    else{ 
      temp_dataset <-read.table(paste(directory,file,sep=''), header=TRUE, sep=",")
      #Insert the ticker into the dataset using the file name
      temp_dataset <- cbind(Ticker = sym <- strsplit(file,"\\.")[[1]][1], temp_dataset)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  return(dataset)
}

#R Function 1: Summary of Stock Return Info
summaryInfo <- function(directory){
  stockData <- loadStockData(directory)
  stockData <- 
    stockData %>%
    group_by(Ticker) %>%
    mutate(lagClose = dplyr::lag(Adj.Close, n = 1, default = NA))
  stockData$Return <- (stockData$Adj.Close/stockData$lagClose)-1
  stockData <- stockData %>%
    group_by(Ticker) %>%
    summarise(numRows = n(),
              meanRet_pct = round(mean(Return,na.rm=TRUE)*100,2),
              medRet_pct = round(median(Return,na.rm=TRUE)*100,2),
              sdRet_pct = round(sd(Return,na.rm=TRUE)*100,2),
              maxRet_pct = round(max(Return, na.rm=TRUE)*100,2),
              minRet_pct = round(min(Return, na.rm=TRUE)*100,2),
              totCumRet_pct = round(((last(Adj.Close)/first(Adj.Close))-1)*100,2))
    return(stockData)
}

#R Function 2: Heatmap of Return Data
heatmapStockRets <- function(directory){
  #Get data and prep it for the heatmap
  prepData <- loadStockData(directory)
  prepData$Date <- as.Date(prepData$Date)
  prepData <- 
    prepData %>%
    group_by(Ticker) %>%
    mutate(lagClose = dplyr::lag(Adj.Close, n = 1, default = NA))
  prepData$Return <- (prepData$Adj.Close/prepData$lagClose)-1
  prepData <- 
    prepData %>% 
    mutate(Month = format(Date,"%m"))
  heatmapData <-
    prepData %>%
    group_by(Ticker, Month) %>%
    summarize(avgRet = mean(Return,na.rm=TRUE)) %>%
    arrange(Ticker,Month)
  heatmapData$MonthName <- month.abb[as.integer(heatmapData$Month)]
  heatmapData$MonthName <- factor(heatmapData$MonthName, levels=unique(heatmapData$MonthName))
  #Now that heatmapData is ready, actually plot the heatmap using ggplot2
  p <- ggplot(data = heatmapData, aes(x = MonthName, y = Ticker)) +
    geom_tile(aes(fill = avgRet),colour = "black") +
    scale_fill_gradient(low = "red", high = "green")
  return(p)
}

#Test functions:
# Alternatively, install just dplyr:
directory = paste(getwd(), "/Stock Price Data/", sep='')
View(summaryInfo(directory))
heatmapStockRets(directory)

