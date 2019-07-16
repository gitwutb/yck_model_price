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
source(paste0(price_model_loc,"\\function\\fun_model_price.R"),echo=FALSE,encoding="utf-8")
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
select_input1<-dbFetch(dbSendQuery(loc_channel,"SELECT brand_name,a.yck_seriesid,MIN(model_id) select_model_id FROM config_vdatabase_yck_major_info a
                                   INNER JOIN (SELECT yck_seriesid FROM analysis_wide_table_cous ORDER BY count_s DESC LIMIT 400) b ON a.yck_seriesid=b.yck_seriesid
                                   GROUP BY a.yck_seriesid;"),-1)
select_inputout<-dbFetch(dbSendQuery(loc_channel,"SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_series 
   WHERE car_level in('豪华车','小型车','中型车','中大型车','紧凑型车','小型SUV','紧凑型SUV','中型SUV','中大型SUV','大型SUV','MPV');"),-1)
dbDisconnect(loc_channel)
###剔除非训练样本#######
#测试离线代码
#select_input1<-data.frame(series='凯越',select_model_id='813')
input_orig<-input_orig%>%filter(yck_seriesid %in% select_inputout$yck_seriesid & brand %in% select_input1$brand_name)
select_input1<-data.frame(select_model_id=select_input1$select_model_id,select_regDate='2017/6/1',select_mile='4',select_partition_month='2018/6/1',yck_seriesid=select_input1$yck_seriesid)
#############################测试数据1：########################################
#select_input1<-read.csv(paste0(price_model_loc,"\\file\\","outline_train.csv"),header = T)
select_input_org<-NULL
# #########临时调用
for (i in 1:15) {
  linshi_input<-select_input1
  linshi_input$select_regDate<-as.character(today()-365*3)
  linshi_input$select_partition_month<-as.character(today()-365*3+150*i)
  select_input_org<-rbind(select_input_org,linshi_input)
}
#20190610添加
ll_unique<-NULL
for (i in 1:nrow(select_input_org)) {
  case<-fun_parameter_ym(select_input_org$select_partition_month[i],select_input_org$select_regDate[i])$case
  model_code<-paste0(select_input_org$yck_seriesid[i],"T",paste0(format(as.Date(Sys.Date()),"%Y"),week(Sys.Date())),"CASE",case,sep="")%>%toupper()
  ll_unique<-c(ll_unique,model_code)
}
select_input_org<-select_input_org %>% dplyr::mutate(ll_unique=ll_unique) %>% 
  group_by(ll_unique) %>% dplyr::filter(select_partition_month==max(select_partition_month)) %>% 
  as.data.frame() %>% dplyr::select(-yck_seriesid,-ll_unique)
rm(select_input1,linshi_input)
gc()
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
                  source(paste0(price_model_loc,"\\function\\fun_model_price.R"),echo=FALSE,encoding="utf-8")))
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

# ##########################寻找数据量最大的车型####################
# loc_channel<-dbConnect(MySQL(),user = "root",host="192.168.0.111",password= "000000",dbname="yck-data-center")
# dbSendQuery(loc_channel,'SET NAMES gbk')
# test<-dbFetch(dbSendQuery(loc_channel,"SELECT model_name,model_id select_model_id FROM config_che300_major_info;"),-1)
# dbDisconnect(loc_channel)
# #######自动
# select_input_cou<-input_orig%>%filter(auto=='自动')%>%group_by(car_level,series,model_name)%>%summarise(model_name_c=n())%>%ungroup()%>%
#   group_by(series)%>%mutate(series_c=sum(model_name_c))%>%as.data.frame()
# select_input_cou1<-select_input_cou[,c("car_level","series","series_c")]%>%unique()%>%group_by(car_level)%>%top_n(20,series_c)%>%ungroup()%>%filter(series_c>5000)
# select_input_cou<-select_input_cou%>%filter(series %in% select_input_cou1$series)%>%group_by(series)%>%top_n(2,model_name_c)%>%as.data.frame()
# select_input1<-inner_join(select_input_cou,test,by="model_name")%>%dplyr::select(select_model_id)
# 
# ##手动
# select_input_cou<-input_orig%>%filter(auto=='手动')%>%group_by(car_level,series,model_name)%>%summarise(model_name_c=n())%>%ungroup()%>%
#   group_by(series)%>%mutate(series_c=sum(model_name_c))%>%as.data.frame()
# select_input_cou1<-select_input_cou[,c("car_level","series","series_c")]%>%unique()%>%group_by(car_level)%>%top_n(20,series_c)%>%ungroup()%>%filter(series_c>5000)
# select_input_cou<-select_input_cou%>%filter(series %in% select_input_cou1$series)%>%group_by(series)%>%top_n(2,model_name_c)%>%as.data.frame()
# select_input2<-inner_join(select_input_cou,test,by="model_name")%>%dplyr::select(select_model_id)
# select_input1<-rbind(select_input1,select_input2)
# select_input1<-data.frame(select_input1,select_regDate='2017/6/1',select_mile='4',select_partition_month='2018/6/1')
# #############################测试数据1：########################################
# select_input_org1<-NULL
# # #########临时调用
# for (i in 1:5) {
#   for (j in 1:6) {
#     linshi_input<-select_input1
#     linshi_input$select_regDate<-as.character(today())
#     linshi_input$select_partition_month<-as.character(today()+320*i)
#     linshi_input$select_mile<-j*as.numeric(round(difftime(as_datetime(linshi_input$select_partition_month),as_datetime(linshi_input$select_regDate),units="days")/365,2))
#     select_input_org1<-rbind(select_input_org1,linshi_input)
#   }
# }
# rm(select_input_cou1,select_input1,linshi_input)
# # #########
# 
# #######################模型######################
# select_input_org<-select_input_org1
# start_time<-Sys.time()
# x<-1:nrow(select_input_org)
# cl<-makeCluster(4)
# clusterExport(cl,c("select_input_org","price_model_loc","input_orig"))
# clusterEvalQ(cl,c(library(RODBC),
#                   library(reshape2),
#                   library(dplyr),
#                   library(ggplot2),
#                   library(RMySQL),
#                   library(stringr),
#                   library(e1071) ,
#                   library(tcltk),
#                   library(lubridate),
#                   source(paste0(price_model_loc,"\\function\\outline_all_fun_input.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\outline_series_fun_input.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\outline_series_fun_pred.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_input.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_input_train.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_input_test.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_factor_standar.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_model_test.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_model_train.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_parameter_ym.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_pred.R"),echo=TRUE,encoding="utf-8"),
#                   source(paste0(price_model_loc,"\\function\\fun_select_transfor.R"),echo=TRUE,encoding="utf-8")))
# output_pre<-tryCatch(
#   {parLapply(cl,x,outline_series_fun_pred)}, 
#   warning = function(w) {"出警告啦"}, 
#   error = function(e) { "出错啦"})
# stopCluster(cl)
# output_pre<-list.rbind(list.rbind(output_pre))
# 
# result_tag<-summarise(group_by(output_pre,select_model_id,select_model_name,select_model_price,select_regDate,
#                                select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm))%>%
#   ungroup()%>%as.data.frame()%>%mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))
# write.csv(result_tag,paste0(price_model_loc,"re.csv"))