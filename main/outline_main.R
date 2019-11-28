rm(list = ls(all=T))
gc()
library(reshape2)
library(dplyr)
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
#local_defin<-data.frame(user = 'yckdctab',host='47.106.189.86',password= 'YckDCtemp2019',dbname='yck-data-center',stringsAsFactors = F)

############################训练车型选取-根据时间选取##############################
parameter_rn<-300
parameter_limit<-case_when(
  weekdays(Sys.Date()) ==  '星期一' ~ paste0(parameter_rn*0,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期二' ~ paste0(parameter_rn*1,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期三' ~ paste0(parameter_rn*2,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期四' ~ paste0(parameter_rn*3,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期五' ~ paste0(parameter_rn*4,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期六' ~ paste0(parameter_rn*5,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期日' ~ paste0(parameter_rn*6,',',1000),
)
loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input_org<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT MIN(a.model_id) select_model_id FROM config_vdatabase_yck_major_info a
      INNER JOIN (SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_series LIMIT ",parameter_limit,") b ON a.yck_seriesid=b.yck_seriesid
      GROUP BY a.yck_seriesid")),-1)
dbDisconnect(loc_channel)
select_input_org<-data.frame(select_model_id=select_input_org$select_model_id,select_regDate='2017/6/1',select_mile=4,select_partition_month='2018/6/1')
#######################模型results<-lapply(x,fun_pred)######################
x<-1:nrow(select_input_org)
cl<-makeCluster(2)
clusterExport(cl,c("select_input_org","price_model_loc","local_defin"))
clusterEvalQ(cl,c(library(RODBC),
                  library(reshape2),
                  library(dplyr),
                  library(RMySQL),
                  library(stringr),
                  library(e1071) ,
                  library(tcltk),
                  library(lubridate),
                  source(paste0(price_model_loc,"/function/fun_model_price_test.R"),echo=FALSE,encoding="utf-8")))
results<-parLapply(cl,x,fun_pred_round)
stopCluster(cl)

##日志
train_id<-as.character(list.rbind(results))
log_modelTrainDaily<-data.frame(trainNum=length(train_id),trainDate=Sys.time())
write.table(log_modelTrainDaily,paste0(price_model_loc,'/Log/log_modelTrainDaily.txt'),append = T, quote = TRUE, sep = " ", row.names = F)
#错误日志price_model_loc(p)
log_modelTrainDaily_ERR<-data.frame(trainModelID=train_id[train_id!='ok'],trainDate=Sys.Date())
if(nrow(log_modelTrainDaily_ERR)>0){
  fun_mailsend("YCK_ModelTrain模型训练",'模型训练告警，请查看日志log_modelTrainDaily_ERR')
  write.table(log_modelTrainDaily_ERR,paste0(price_model_loc,'/Log/log_modelTrainDaily_ERR.txt'),append = T, quote = TRUE, sep = " ",row.names = F)
}

# list_matchfile<-list.files(paste0(price_model_loc,"\\output\\relation"), full.names = T,pattern = ".csv")
# list_matchfile<-data.frame(model_id=gsub(".*output\\\\relation\\/|.csv","",list_matchfile))
# write.csv(list_matchfile,paste0(price_model_loc,"/output/relation/list_matchfile.csv"),
#           row.names = F,fileEncoding = "UTF-8",quote = F,append = T)