print("ML model start:")
options(warn =-1)
library(RODBC)
library(reshape2)
library(dplyr,warn.conflicts =F)
library(RMySQL)
library(stringr)
library(e1071)
#library(tcltk)
library(lubridate)
library(truncnorm)
library(cluster)
library(Rtsne)
library(tidyr)
price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"\\function\\fun_mysql_config_up.R"),echo=FALSE,encoding="utf-8")
source(paste0(price_model_loc,"\\function\\fun_model_price_test.R"),echo=FALSE,encoding="utf-8")
local_defin<-fun_mysql_config_up()
###接口调用函数
model_interface_datatest<-function(input_tra){
  ############################数据输入：########################################
  before_query <-input_tra
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  test_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_query WHERE select_model_id=",before_query$select_model_id,
                                                     " AND user_id=",before_query$user_id,
                                                     " AND select_classification_operational='",enc2native(as.character(before_query$select_classification_operational)),"'",
                                                     " AND select_classification_car='",enc2native(as.character(before_query$select_classification_car)),"'",
                                                     " AND ABS(select_mile-",before_query$select_mile,")<0.5",
                                                     " AND ABS(DATEDIFF(select_regDate,'",before_query$select_regDate,"'))<2",
                                                     " AND ABS(DATEDIFF(select_partition_month,'",before_query$select_partition_month,"'))<2")),-1)
  max_user_query_id<-dbFetch(dbSendQuery(loc_channel,"select MAX(user_query_id) user_query_id from yck_project_model_query"),-1)%>%as.integer()
  dbDisconnect(loc_channel)
  if(nrow(test_query)>0){
    return_post_model='Y'   ##1表示“请勿重复提交”
  }else{
    return_post_model='N'
  }
  return(return_post_model)
}

model_interface_train<-function(input_tra){
  ############################数据输入：########################################
  before_query <-input_tra
  max_user_query_id<-read.table(paste0(price_model_loc,"/file/max_user_query_id.txt")) %>% as.character() %>% as.integer()
  write.table(max_user_query_id+1,paste0(price_model_loc,"/file/max_user_query_id.txt"),row.names = F,col.names = F,append = F)
  if(before_query$select_classification_car=='期车'){before_query$select_mile<-3*as.numeric(round(difftime(as_datetime(before_query$select_partition_month),as_datetime(before_query$select_regDate),units="days")/365,2))}
  if(length(grep('select_tname',names(before_query)))==0){before_query<-before_query %>% mutate(select_tname='PC')}
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  ##保存本地
  yck_project_model_query<-data.frame(user_query_id=max_user_query_id+1,before_query,query_statue=1)
  yck_project_model_query$add_time<-as.character(format(Sys.time(),"%Y/%m/%d %H:%M:%S"))
  yck_project_model_query<-yck_project_model_query %>% dplyr::select(user_query_id,user_id,select_tname,select_model_id,select_regDate,select_mile,select_partition_month,
                                                                     select_classification_operational,select_classification_car,add_time,query_statue)
  write.csv(yck_project_model_query,paste0(price_model_loc,"/output/result/yck_project_model_query",".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_query",".csv"),"'",
                                 " INTO TABLE yck_project_model_query CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  #yck_project_model_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_query WHERE user_query_id=",yck_project_model_query$user_query_id)),-1)
  dbDisconnect(loc_channel)
  select_input<-yck_project_model_query%>%dplyr::select(user_query_id,select_model_id,select_regDate,select_mile,select_partition_month)
  select_input$select_mile<-as.numeric(select_input$select_mile)
  return_post_model<-tryCatch({model_main(select_input)},
                              error=function(e){3},
                              finally={4})
  if(return_post_model==T){return_post_model=0}
  if(return_post_model!=0){
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    dbSendQuery(loc_channel,paste0("UPDATE yck_project_model_query SET query_statue=",return_post_model," WHERE user_query_id=",unique(select_input$user_query_id),";"))
    dbDisconnect(loc_channel)
  }
  return(return_post_model)
}