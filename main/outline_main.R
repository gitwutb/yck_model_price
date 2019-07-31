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
library(parallel)
library(rlist)
price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
lf<-list.files(paste0(price_model_loc,"/model_net",sep=""), full.names = T,pattern = ".RData")
file.remove(lf[grep(paste0(format(as.Date(Sys.Date()-7),"%Y"),week(Sys.Date()-7),"CASE"),lf)])
source(paste0(price_model_loc,"/function/fun_model_price_test.R"),echo=FALSE,encoding="utf-8")
source(paste0(price_model_loc,"/function/yck_base_function.R"),echo=FALSE,encoding="utf-8")
local_defin<-fun_mysql_config_up()
##########数据输入
input_orig<-outline_all_fun_input()

#临时
analysis_wide_table_cous<-dplyr::summarise(group_by(input_orig,yck_seriesid),count_s=n()) %>% as.data.frame()
write.csv(analysis_wide_table_cous,paste0(price_model_loc,"\\file\\analysis_wide_table_cous.csv",sep=""),
          row.names = F,fileEncoding = "UTF-8",quote = F)
rm(analysis_wide_table_cous)
#################*********录入本地*********#################
loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE '",price_model_loc,"/file/analysis_wide_table_cous.csv'",
                               " REPLACE INTO TABLE analysis_wide_table_cous CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;",sep=""))
dbDisconnect(loc_channel)


############################模型链条完善##############################
###寻找数据量最大的车型
loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input1<-dbFetch(dbSendQuery(loc_channel,"SELECT a.brand_name,a.yck_seriesid,MIN(a.model_id) select_model_id 
                                   FROM config_vdatabase_yck_major_info a GROUP BY a.yck_seriesid;"),-1)
select_inputout<-dbFetch(dbSendQuery(loc_channel,"SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_series 
   WHERE car_level in('豪华车','跑车','微型车','小型车','中型车','中大型车','紧凑型车','小型SUV','紧凑型SUV','中型SUV','中大型SUV','大型SUV','全尺寸SUV','MPV');"),-1)
dbDisconnect(loc_channel)
###剔除非训练样本#######
#测试离线代码
#select_input1<-data.frame(series='凯越',select_model_id='813')
input_orig<-input_orig%>%filter(yck_seriesid %in% select_inputout$yck_seriesid & brand %in% select_input1$brand_name)
select_input1<-data.frame(select_model_id=select_input1$select_model_id,select_regDate='2017/6/1',select_mile='4',select_partition_month='2018/6/1',yck_seriesid=select_input1$yck_seriesid)
#############################测试数据1：########################################
#select_input1<-read.csv(paste0(price_model_loc,"\\file\\","outline_train.csv"),header = T)
select_input_org<-select_input1
select_input_org$select_mile<-3*as.numeric(round(difftime(as_datetime(select_input_org$select_partition_month),as_datetime(select_input_org$select_regDate),units="days")/365,2))
#######################模型results<-lapply(x,fun_pred)######################
x<-1:nrow(select_input_org)
cl<-makeCluster(4)
clusterExport(cl,c("select_input_org","price_model_loc","input_orig","local_defin"))
clusterEvalQ(cl,c(library(RODBC),
                  library(reshape2),
                  library(dplyr),
                  library(ggplot2),
                  library(RMySQL),
                  library(stringr),
                  library(e1071) ,
                  library(tcltk),
                  library(lubridate),
                  source(paste0(price_model_loc,"/function/fun_model_price_test.R"),echo=FALSE,encoding="utf-8")))
results<-tryCatch(
  {parLapply(cl,x,outline_series_fun_pred)}, 
  warning = function(w) {"出警告啦"}, 
  error = function(e) { "出错啦"})
#results<-list.rbind(list.rbind(results))
stopCluster(cl)


#清除历史模型缓存文件
lf<-list.files(paste0(price_model_loc,"/model_net",sep=""), full.names = T,pattern = ".RData")
lf_list<-lf[grep(paste0(format(as.Date(Sys.Date()-7),"%Y"),week(Sys.Date()-7),'CASE'),lf)]
file.remove(lf_list)

# list_matchfile<-list.files(paste0(price_model_loc,"\\output\\relation"), full.names = T,pattern = ".csv")
# list_matchfile<-data.frame(model_id=gsub(".*output\\\\relation\\/|.csv","",list_matchfile))
# write.csv(list_matchfile,paste0(price_model_loc,"/output/relation/list_matchfile.csv"),
#           row.names = F,fileEncoding = "UTF-8",quote = F,append = T)