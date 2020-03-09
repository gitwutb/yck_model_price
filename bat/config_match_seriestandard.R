#构建配置表config_match_seriestandard（用于对标），跟随车型库的更新而更新
rm(list = ls(all=T))
gc()
options(warn =-1)
library(dplyr,warn.conflicts =F)
library(RMySQL)
library(stringr)
#library(tcltk)
library(lubridate)
library(Rtsne)
library(mailR)
price_model_loc<-gsub("(\\/main|\\/bat).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
#local_defin<-data.frame(user = 'yckdc',host='47.106.189.86',password= 'fHvEsMynAy',dbname='yck-data-center',stringsAsFactors = F)
source(paste0(price_model_loc,"/function/yck_base_function.R"),echo=FALSE,encoding="utf-8")
source(paste0(price_model_loc,"/function/SerieStandardFun.R"),echo=FALSE,encoding="utf-8")
local_defin<-fun_mysql_config_up()
loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
input_id<-dbFetch(dbSendQuery(loc_channel,"SELECT DISTINCT model_id from config_vdatabase_yck_major_info a 
                              WHERE a.yck_seriesid NOT in(SELECT bd_yck_seriesid FROM (SELECT bd_yck_seriesid,COUNT(*) bd_n 
                                                            FROM config_match_seriestandard GROUP BY bd_yck_seriesid)s WHERE bd_n!=1)"),-1) %>% .[[1]]
input_id_series<-dbFetch(dbSendQuery(loc_channel,"SELECT model_id model_id1,yck_seriesid,series_name,car_country from config_vdatabase_yck_major_info a
  INNER JOIN (SELECT DISTINCT yck_brandid,car_country FROM config_vdatabase_yck_brand) b ON a.yck_brandid=b.yck_brandid"),-1)
dbDisconnect(loc_channel)
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
  fun_mysqlload_add_upd(price_model_loc,local_defin,config_match_seriestandard,'config_match_seriestandard')
}
##将未纳入的车系补充完整
loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
dbSendQuery(loc_channel,"INSERT INTO config_match_seriestandard SELECT DISTINCT yck_seriesid bd_yck_seriesid,
                      car_country bd_car_country,yck_seriesid,car_country,1 count_p FROM config_vdatabase_yck_major_info a 
                      INNER JOIN config_vdatabase_yck_brand b ON a.brandid=b.brandid 
            WHERE a.yck_seriesid NOT in(SELECT DISTINCT bd_yck_seriesid FROM config_match_seriestandard)")
dbDisconnect(loc_channel)
