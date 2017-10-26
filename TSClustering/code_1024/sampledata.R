#### Load Data ####
files  <- list.files("FreshData",pattern = '*.csv')

myfiles = lapply(files, function(x) read.csv(paste("FreshData", x, sep="/"), header=TRUE))
# length(myfiles) # 506
name = sapply(files, function(x) unlist(strsplit(x[1], split='.', fixed=TRUE))[1])
names(myfiles) = name
# only choose stocks whose data length are >= 251 days
drop = as.numeric(paste(lapply(myfiles, function(x) dim(x)[1]))) >= 251
complete = myfiles[drop]
# length(complete) # 497
bigfile = do.call(rbind, complete)   
bigfile$price = apply(bigfile[c("Open", "High", "Low", "Close")], 1, mean)
bigfile = rownames_to_column(bigfile)
bigfile$rowname = sapply(bigfile$rowname, function(x) 
  unlist(strsplit(x[1], split='.', fixed=TRUE))[1]) # 124771      8
bigfile = bigfile[complete.cases(bigfile),]

#### Sample one stock in each Industry ####
library(XLConnect)
wb <- loadWorkbook("SP500CompaniesInSectors.xlsx")
lst = readWorksheet(wb, sheet = getSheets(wb))

library(tidyverse)
list = do.call(rbind, lst) # 505   4

d_all = merge(bigfile, list, all.x = TRUE, by.x = "rowname", by.y = "Ticker")
# "BF", "BRK", "SP500"
# 124771     11
# length(unique((d_all$stock))) # 497
names(d_all)[1] = "stock" 

list_s = d_all %>%
         group_by(stock,Industry) %>%
         summarise(n = n()) %>%
         subset(.,!is.na(Industry)) 
list_s = subset(list_s, Industry != "Hotels, Resorts & Cruise Lines" & 
                        Industry != "IT Consulting & Other Services" )
# 494   3 
list_s = data.frame(list_s[,c("stock", "Industry")])
list_ind = split(list_s, list_s$Industry)
set.seed(111)
list_byind = lapply(list_ind, function(x) x[sample(nrow(x), 1), ])
sample = do.call(rbind, list_byind)
sample_name = sample$stock

d = subset(d_all, stock %in% sample_name | stock == "SP500" | 
             stock == "HLT" | stock == "IBM") # 30143    11
length(unique(d$stock)) # 124
d[is.na(d)] = "SP500"

#### Data Cleaning ####
library(lubridate)
dd_all = d[,c("Date", "stock", "price", "Company.Name", "Sector", "Industry")]
dd_all[,c("stock", "Company.Name", "Sector", "Industry")] = 
  lapply(dd_all[,c("stock", "Company.Name", "Sector", "Industry")], as.factor)
dd_all$Date = parse_date_time(dd_all$Date, c("%m/%d/%Y", "%Y-%m-%d", "%m/%d/%y"))
dd_all = subset(dd_all, Date >= "2016-10-03" & Date <= "2017-10-02") # 31039     6
write.csv(dd_all, "stocks_124.csv")

