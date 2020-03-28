###模型每日离线训练intersect函数
rm(list = ls(all=T))
gc()
library(parallel)
price_model_loc<-gsub("(\\/main|\\/bat).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"/main/model_interface.R"),echo=FALSE,encoding="utf-8")
############################训练车型选取-根据时间选取##############################
outlineFun_modeltrain<-function(input_path,input_ip){
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
  parameter_limit<-paste0(0,',',2)
  select_input_org<-fun_mysqlload_query(input_ip,paste0("SELECT MIN(a.model_id) select_model_id FROM config_vdatabase_yck_major_info a
                                                         INNER JOIN (SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_series LIMIT ",parameter_limit,") b ON a.yck_seriesid=b.yck_seriesid
                                                         GROUP BY a.yck_seriesid"))
  select_input_org<<-data.frame(select_model_id=select_input_org$select_model_id,select_regDate='2013/6/1',select_mile=4,select_partition_month='2018/6/1')
  #######################模型results<-lapply(x,fun_pred)######################
  x<-1:nrow(select_input_org)
  cl<-makeCluster(2)
  clusterExport(cl,c("select_input_org","input_path"))
  clusterEvalQ(cl,c(source(paste0(input_path,"/main/model_interface.R"),echo=FALSE,encoding="utf-8")))
  results<-parLapply(cl,x,fun_pred_round)
  stopCluster(cl)
  return(results)
}
##车系对标计算-车型库/详细配置有更新才执行：保持逻辑一致
outlineFun_seriestandard<-function(input_path,input_ip){
  sql_where<-ifelse(day(Sys.Date())==1,"bd_model_id!=model_id","bd_model_id=model_id")
  input_id<-fun_mysqlload_query(local_defin,paste0("SELECT a.model_id FROM config_vdatabase_yck_major_info a
                           LEFT JOIN (SELECT DISTINCT bd_model_id FROM config_match_modelstandard WHERE ",sql_where,") b ON a.model_id=b.bd_model_id
                           WHERE b.bd_model_id IS NULL")) %>% .[[1]]
  if(length(input_id)>0){
    for (i in 1:length(input_id)) {
      tryCatch({main_fun_series_standard(input_id[i])},
               error=function(e){3},
               finally={4})
    }
    ##更新刚运行的车系对标量
    result_seriestandard<-fun_mysqlload_query(input_ip,paste0("SELECT b.yck_seriesid bd_yck_seriesid,b.car_country bd_car_country,
                    b.is_green bd_is_green,c.yck_seriesid,c.car_country,count(*) count_p FROM config_match_modelstandard a
                    INNER JOIN (SELECT m.model_id,m.yck_seriesid,m.is_green,n.car_country FROM config_vdatabase_yck_major_info m
                                  INNER JOIN config_vdatabase_yck_brand n ON m.brandid=n.brandid) b ON a.bd_model_id=b.model_id
                    INNER JOIN (SELECT m.model_id,m.yck_seriesid,m.is_green,n.car_country FROM config_vdatabase_yck_major_info m
                                 INNER JOIN config_vdatabase_yck_brand n ON m.brandid=n.brandid) c ON a.model_id=c.model_id
                    INNER JOIN (SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_major_info WHERE model_id in(",paste0(input_id,collapse = ","),")) d ON b.yck_seriesid=d.yck_seriesid
                    WHERE b.is_green=c.is_green
                       GROUP BY b.yck_seriesid,b.is_green,b.car_country,c.yck_seriesid,c.car_country"))
    fun_mysqlload_add_upd(input_path,input_ip,result_seriestandard,'config_match_seriestandard')
    ##将未纳入的车系补充完整
    fun_mysqlload_query(input_ip,"INSERT INTO config_match_seriestandard SELECT DISTINCT yck_seriesid bd_yck_seriesid,
                      car_country bd_car_country,yck_seriesid,car_country,a.is_green bd_is_green,1 count_p FROM config_vdatabase_yck_major_info a 
                      INNER JOIN config_vdatabase_yck_brand b ON a.brandid=b.brandid 
            WHERE a.yck_seriesid NOT in(SELECT DISTINCT bd_yck_seriesid FROM config_match_seriestandard)")
  }
}
outlineFun_seriestandard(price_model_loc,local_defin)
outlineFun_modeltrain(price_model_loc,local_defin)


# lf_list1<-gsub("E:/yck_model_train/model_net/|TIMECASE.*",'',
#                list.files(paste0(price_model_loc,"/model_net",sep=""), full.names = T,pattern = ".RData")) %>% unique()
# lf_list2<-fun_mysqlload_query(local_defin,"SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_series") %>% .[[1]]
# lf_diff<-paste0("('",paste0(setdiff(lf_list2,lf_list1),collapse = "','"),"')")
# diff_series<-fun_mysqlload_query(local_defin,paste0("SELECT * FROM config_vdatabase_yck_series where yck_seriesid in",lf_diff))