###模型每日离线训练
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
outlineFun_seriestandard<-function(input_ip){
  input_id<-fun_mysqlload_query(input_ip,"SELECT DISTINCT model_id from config_vdatabase_yck_major_info a 
                    WHERE a.yck_seriesid NOT in(SELECT bd_yck_seriesid FROM (SELECT bd_yck_seriesid,COUNT(*) bd_n 
                                                                             FROM config_match_seriestandard GROUP BY bd_yck_seriesid)s WHERE bd_n!=1)") %>% .[[1]]
  input_id_series<-fun_mysqlload_query(input_ip,"SELECT model_id model_id1,yck_seriesid,series_name,car_country from config_vdatabase_yck_major_info a
  INNER JOIN (SELECT DISTINCT yck_brandid,car_country FROM config_vdatabase_yck_brand) b ON a.yck_brandid=b.yck_brandid")
  
  if(day(Sys.Date())!=1){
    fname_csv <- as.numeric(gsub('.csv','',list.files(paste0(price_model_loc,'/output/relation'))))
    input_id<-setdiff(input_id,fname_csv)
  }
  for (i in 1:length(input_id)) {
    tryCatch({main_fun_series_standard(input_id[i])},
             error=function(e){3},
             finally={4})
  }
  fname_csv <- as.numeric(gsub('.csv','',list.files(paste0(price_model_loc,'/output/relation'))))
  fname_csv <- intersect(fname_csv,input_id)
  if(length(fname_csv)>0){
    return_bd<-NULL
    for (i in 1:length(fname_csv)) {
      temp_file<-read.csv(paste0(price_model_loc,'/output/relation/',fname_csv[i],'.csv'),stringsAsFactors = F) %>% 
        dplyr::mutate(bd_model_id=as.numeric(gsub('.csv','',fname_csv[i])))
      return_bd<-rbind(return_bd,temp_file)
    }
    config_match_seriestandard<-return_bd %>% dplyr::select(bd_model_id,model_id)
    config_match_seriestandard<-inner_join(config_match_seriestandard,input_id_series,by=c('bd_model_id'='model_id1')) %>% 
      dplyr::select(bd_model_id,model_id,bd_yck_seriesid=yck_seriesid,bd_car_country=car_country)
    config_match_seriestandard<-inner_join(config_match_seriestandard,input_id_series,by=c('model_id'='model_id1'))
    config_match_seriestandard<-config_match_seriestandard %>% dplyr::group_by(bd_yck_seriesid,bd_car_country,yck_seriesid,car_country) %>% 
      dplyr::summarise(count_p=n()) %>% as.data.frame()
    fun_mysqlload_add_upd(price_model_loc,input_ip,config_match_seriestandard,'config_match_seriestandard')
  }
  ##将未纳入的车系补充完整
  fun_mysqlload_query(input_ip,"INSERT INTO config_match_seriestandard SELECT DISTINCT yck_seriesid bd_yck_seriesid,
                      car_country bd_car_country,yck_seriesid,car_country,1 count_p FROM config_vdatabase_yck_major_info a 
                      INNER JOIN config_vdatabase_yck_brand b ON a.brandid=b.brandid 
            WHERE a.yck_seriesid NOT in(SELECT DISTINCT bd_yck_seriesid FROM config_match_seriestandard)")
}
outlineFun_seriestandard(local_defin)
outlineFun_modeltrain(price_model_loc,local_defin)


# lf_list1<-gsub("E:/yck_model_train/model_net/|TIMECASE.*",'',
#                list.files(paste0(price_model_loc,"/model_net",sep=""), full.names = T,pattern = ".RData")) %>% unique()
# lf_list2<-fun_mysqlload_query(local_defin,"SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_series") %>% .[[1]]
# lf_diff<-paste0("('",paste0(setdiff(lf_list2,lf_list1),collapse = "','"),"')")
# diff_series<-fun_mysqlload_query(local_defin,paste0("SELECT * FROM config_vdatabase_yck_series where yck_seriesid in",lf_diff))