rm(list = ls(all=T))
gc()
library(RODBC)
library(reshape2)
library(dplyr)
library(RMySQL)
library(stringr)
library(e1071) 
library(tcltk)
library(lubridate)
library(truncnorm)
library(cluster)
library(Rtsne)
library(tidyr)
library(xlsx)
price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
source(paste0(price_model_loc,"\\function\\fun_model_price.R"),echo=FALSE,encoding="utf-8")
#############################数据输入：########################################
select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst.csv"),header = T)
# #########临时调用
#select_input$select_mile<-3*as.numeric(round(difftime(as_datetime(select_input$select_partition_month),as_datetime(select_input$select_regDate),units="days")/365,2))
#########
#select_input<-select_input[1:1,]
for (i in 1:nrow(select_input)) {
  main_fun_main_union(select_input[i,])
}
