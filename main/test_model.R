#主要用于测试模型输出
rm(list = ls(all=T))
gc()
library(reshape2)
library(dplyr)
library(ggplot2)
library(RMySQL)
library(stringr)
library(e1071) 
library(tcltk)
library(lubridate)
library(xlsx)
###########加载自定义函数###########paste0(price_model_loc,"\\function")
price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
source(paste0(price_model_loc,"\\function\\fun_model_price_test.R"),echo=FALSE,encoding="utf-8")
#####数据载入######
#select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst.csv"),header = T)
loc_channel<-dbConnect(MySQL(),user = "yckdc",host="47.106.189.86",password= "YckDC888",dbname="yck-data-center")
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_query WHERE user_query_id in (",paste0(216:216,collapse = ','),')')),-1)%>%
  dplyr::select(select_model_id,select_regDate,select_mile,select_partition_month)
dbDisconnect(loc_channel)
###-----------第一部分：单一输出--------####
result_tag<-apply(select_input,1,fun_pred)
result_tag<-list.rbind(list.rbind(result_tag))
result_tag<-summarise(group_by(result_tag,select_model_id,select_model_name,select_model_price,select_regDate,
                               select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm))%>%
  ungroup()%>%as.data.frame()%>%mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))


###-----------第二部分：多输出--------####
for (i in 1:nrow(select_input)) {
  main_fun_main_union(select_input[i,])
}


###-----------第三部分：测试产品模型--------####
rm(list = ls(all=T))
gc()
price_model_loc<-gsub("\\/main","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")
loc_channel<-dbConnect(MySQL(),user = "yckdc",host="47.106.189.86",password= "YckDC888",dbname="yck-data-center")
dbSendQuery(loc_channel,'SET NAMES gbk')
input_tra<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_query WHERE user_query_id=",223)),-1)%>%
  dplyr::select(-user_query_id,-query_statue)
dbDisconnect(loc_channel)
return_datatest<-model_interface_datatest(input_tra)
if(return_datatest=='N'){
  model_interface_train(input_tra)
}else{
  print(return_datatest)
}

###output test2###
parameter_user_query_id=4
parameter_classification_operational='营运'
parameter_model_number=1
parameter_province='广东'
interface_out1_model_mprice(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out2_detail_th(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out3_model_fprice(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out4_model_diprice(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out5_sh_mprice(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out6_display(parameter_user_query_id,parameter_classification_operational,parameter_model_number,parameter_province)
interface_out7_detail_mth(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out8_match_price(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out9_sales_new(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out10_prate(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
