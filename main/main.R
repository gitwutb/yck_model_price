rm(list = ls(all=T))
gc()
library(RODBC)
library(reshape2)
library(dplyr)
library(ggplot2)
library(RMySQL)
library(stringr)
library(e1071) 
library(tcltk)
library(lubridate)
##########数据输入
###########加载自定义函数###########paste0(price_model_loc,"\\function")
price_model_loc<-gsub("\\/main","",dirname(rstudioapi::getActiveDocumentContext()$path))
local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
source(paste0(price_model_loc,"\\function\\fun_model_price.R"),echo=FALSE,encoding="utf-8")
############################模型链条完善##############################
#############################测试数据1：########################################
select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst.csv"),header = T)
#select_input<-select_input[1:1,]
# #########临时调用
# aa<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst7.csv"),header = T)
# select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst6.csv"),header = T)
# select_input<-data.frame(select_input[rep(1:16,each=15),1:3],select_partition_month=aa[,4])
# select_input$select_mile<-3*as.numeric(round(difftime(as_datetime(select_input$select_partition_month),as_datetime(select_input$select_regDate),units="days")/365,2))
# #########

start_time<-Sys.time()
pb <- tkProgressBar("进度","已完成 %", 0, 100) #开启进度条 
u<-1:nrow(select_input)
output_pre<-NULL
output_sample<-NULL
for (i in 1:nrow(select_input)) {
  linshi<-fun_pred(select_input[i,])
  linshi1<-linshi[[1]]
  output_pre<-rbind(output_pre,linshi1)
  info<- sprintf("已完成 %d%%", round(i*100/length(u)))   #进度条
  setTkProgressBar(pb, i*100/length(u),sprintf("进度 (%s)  耗时:(%ss)",info,round(unclass(as.POSIXct(Sys.time()))-unclass(as.POSIXct(start_time)))),info)  #进度条
}
close(pb)  #关闭进度条
result_tag<-summarise(group_by(output_pre,select_model_id,select_model_name,select_model_price,select_regDate,
                               select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm))%>%
  ungroup()%>%as.data.frame()%>%mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))


result_tag<-summarise(group_by(result_tag,select_model_id,select_model_name,select_model_price,select_mile),
                      fb_price=mean(fb_price),pm_price=mean(pm_price),fb_monitor=mean(fb_monitor),pm_monitor=mean(pm_monitor))
