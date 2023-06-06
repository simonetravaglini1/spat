library(corrplot)
library(tidyr)
library(plyr)
library(dplyr)
library(psych)
rm(list = ls())

#import
setwd("/Users/trava/OneDrive/Desktop/")
data <-rio::import( "ZA7600_v3-0-0.dta")

#pulizia dati
data1 <- data.frame(data$v30, data$v31, data$v22, data$v23, data$v24)
colnames(data1) <- c("v30", "v31", "v22", "v23", "v24")

#vedere distribuzione di frequenze
descr::freq(data1$v30, plot = F)
descr::freq(data1$v31, plot = F)
descr::freq(data1$v22, plot = F)
descr::freq(data1$v23, plot = F)
descr::freq(data1$v28, plot = F)

#ricodifica delle variabili 
data1 <- data1 %>%
  mutate(v30=mapvalues(v30, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v31=mapvalues(v31, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v22=mapvalues(v22, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v23=mapvalues(v23, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))%>%
  mutate(v24=mapvalues(v24, from=c(-9,-8,1,2,3,4,5),
                       to=c(NA,3,5,4,3,2,1)))

#correlation matrix
cor(data1, use = "complete.obs")
corrplot(cor(data1, use = "complete.obs"),method = "shade")

#na rm

data1 <- na.omit(data1)


#factor analysis

#1 check the number of possible factors
fa.parallel(data1, fa="fa")#2

factanal(data1, factors = 2, rotation = "oblimin")

