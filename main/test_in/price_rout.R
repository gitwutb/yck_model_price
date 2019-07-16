#####对于非系统车源报价处理
rm(list = ls(all=T))
gc()
price_model_loc<-gsub("\\/main","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")
select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst.csv"),header = T)
input_tra<-select_input %>% mutate(user_id=1,select_classification_operational='非营运',select_classification_car='现车',add_time=Sys.time())
for (i in 1:nrow(input_tra)) {
  model_interface_train_yck(input_tra[i,])
}



#####对于系统车源（快进快出项目）报价处理过程####
rm(list = ls(all=T))
gc()
library(reshape2)
library(dplyr)
library(RMySQL)
library(stringr)
library(tcltk)
library(lubridate)
library(truncnorm)
library(Rtsne)
options(scipen = 200)
price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
local_defin_yun<-data.frame(user = 'yckdctab',host='47.106.189.86',password= 'YckDCtemp2019',dbname='yck-data-center',stringsAsFactors = F)
local_defin_yunl<-data.frame(user = "yckdctab",host="172.18.215.178",password= "YckDCtemp2019",dbname="yck-data-center",stringsAsFactors = F)
local_defin_yun_it<-data.frame(user = 'data',host='youcku.com',password= 'ibfIh28',dbname='yck',stringsAsFactors = F)
source(paste0(price_model_loc,"\\main\\project_it\\fun_project_it_user.R"),echo=FALSE,encoding="utf-8")
#re_fun_yckdc_query_config_id<-fun_yckdc_query_config_id()
##*****报价******#
belong_project=108
loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input_pre<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT a.project_name,a.car_id,a.autohome_id,a.license_reg_date,round(a.kilometre/10000,2) select_mile,b.id_che300 FROM yck_tableau_it_regular a 
                               LEFT JOIN yck_it_query_config_id b ON a.autohome_id=b.id_autohome
                               WHERE a.belong_project=",belong_project,";")),-1)
dbDisconnect(loc_channel)
if(anyNA(select_input_pre$id_che300)==F){
  select_input_pre<-select_input_pre %>% mutate(select_partition_month=Sys.Date()) %>%
    dplyr::select(select_model_id=id_che300,select_regDate=license_reg_date,select_mile,select_partition_month,select_tname=project_name,yck_car_id=car_id)
  select_input_pre$select_mile[which(select_input_pre$select_mile==0)]<-
    4*as.numeric(difftime(as_datetime(select_input_pre$select_partition_month[which(select_input_pre$select_mile==0)]),as_datetime(select_input_pre$select_regDate[which(select_input_pre$select_mile==0)]),units="days")/365)
  select_input<-select_input_pre %>% filter(select_regDate!='1999-01-01')
  #估值
  source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")
  input_tra<-select_input %>% mutate(user_id=1,select_classification_operational='非营运',select_classification_car='现车',add_time=Sys.time())
  for (i in 1:nrow(input_tra)) {
    model_interface_train_yck(input_tra[i,])
  }
}else{
  print("请核查汽车之家与车300的ID匹配是否存在重复")
}