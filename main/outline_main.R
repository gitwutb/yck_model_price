rm(list = ls(all=T))
gc()
library(parallel)
price_model_loc<-gsub("(\\/main|\\/bat).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")
price_model_loc
############################训练车型选取-根据时间选取##############################
parameter_rn<-300
parameter_limit<-case_when(
  weekdays(Sys.Date()) ==  '星期一' ~ paste0(parameter_rn*0,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期二' ~ paste0(parameter_rn*1,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期三' ~ paste0(parameter_rn*2,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期四' ~ paste0(parameter_rn*3,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期五' ~ paste0(parameter_rn*4,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期六' ~ paste0(parameter_rn*5,',',parameter_rn),
  weekdays(Sys.Date()) ==  '星期日' ~ paste0(parameter_rn*6,',',1000)
)
parameter_limit<-paste0(0,',',1)
select_input_org<-fun_mysqlload_query(local_defin,paste0("SELECT MIN(a.model_id) select_model_id FROM config_vdatabase_yck_major_info a
                                                         INNER JOIN (SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_series LIMIT ",parameter_limit,") b ON a.yck_seriesid=b.yck_seriesid
                                                         GROUP BY a.yck_seriesid"))
select_input_org<-data.frame(select_model_id=select_input_org$select_model_id,select_regDate='2013/6/1',select_mile=4,select_partition_month='2018/6/1')
#######################模型results<-lapply(x,fun_pred)######################
x<-1:nrow(select_input_org)
cl<-makeCluster(2)
clusterExport(cl,c("select_input_org","price_model_loc","local_defin"))
clusterEvalQ(cl,c(source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")))
results<-parLapply(cl,x,fun_pred_round)
stopCluster(cl)


# lf_list1<-gsub("E:/yck_model_train/model_net/|TIMECASE.*",'',
#                list.files(paste0(price_model_loc,"/model_net",sep=""), full.names = T,pattern = ".RData")) %>% unique()
# lf_list2<-fun_mysqlload_query(local_defin,"SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_series") %>% .[[1]]
# lf_diff<-paste0("('",paste0(setdiff(lf_list2,lf_list1),collapse = "','"),"')")
# diff_series<-fun_mysqlload_query(local_defin,paste0("SELECT * FROM config_vdatabase_yck_series where yck_seriesid in",lf_diff))