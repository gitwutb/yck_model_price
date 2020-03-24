##*******************可售不可售计算/5分钟一次************##
rm(list = ls(all=T))
gc()
library(parallel)
price_model_loc<-gsub("(\\/main|\\/bat).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"/main/model_interface.R"),echo=FALSE,encoding="utf-8")

source(paste0(price_model_loc,"/main/main_elsefun_bat/evalFun_inout_file.R"),echo=FALSE,encoding="utf-8")
#####数鼎数据一三五更新
if(weekdays(Sys.time()) %in% c("星期一","星期三","星期五") & as.numeric(format(Sys.time(),"%H"))==7 & as.numeric(format(Sys.time(),"%M"))>=50){
  evalCompare_fun_sdInterface(price_model_loc,local_defin)
}

##第一部分：autohome的ID每周更新匹配表
if(as.numeric(format(Sys.time(),"%H"))==23 & as.numeric(format(Sys.time(),"%M"))>=40){
  charSql_add_match<-paste0("SELECT a.autohome_id,c.type_name model_name_a,c.recommend_price price_auto
             FROM (SELECT DISTINCT ac.autohome_id FROM ods_erp.ods_car_basic_config ac) a
                  LEFT JOIN yckdc_da2_ucar.yck_it_query_config_id b ON a.autohome_id=b.id_autohome
                                                        INNER JOIN ods_erp.ods_car_basic_config c ON a.autohome_id=c.autohome_id
                                                        WHERE id_che300 IS NULL;")
  addFun_yckit_query_configid(price_model_loc,local_defin,charSql_add_match)
}

##第二部分：autohome的ID每5分钟更新一次/之后执行估值
if(1==1){
  charSql_add_match_yck<-paste0("SELECT a.autohome_id,c.type_name model_name_a,c.recommend_price price_auto
          FROM (SELECT DISTINCT ac.autohome_id FROM erp_dw.dwd_cars aa INNER JOIN ods_erp.ods_car_basic_config ac ON aa.config_id=ac.id) a
                  LEFT JOIN yckdc_da2_ucar.yck_it_query_config_id b ON a.autohome_id=b.id_autohome
                                                        INNER JOIN ods_erp.ods_car_basic_config c ON a.autohome_id=c.autohome_id
                                                        WHERE id_che300 IS NULL;")
  addFun_yckit_query_configid(price_model_loc,local_defin,charSql_add_match_yck)
}
##更新估值5分钟更新没有的，每天更新一次进行中的2/7分启动
if(as.numeric(format(Sys.time(),"%H"))==23 & as.numeric(format(Sys.time(),"%M"))>=54){
  # char_sql_selling<-c("SELECT a.car_id,d.id_che300,a.license_reg_date,a.kilometre/10000 select_mile,
  #                     IF(is_sold=0,DATE_FORMAT(NOW(),'%Y-%m-%d'),b.sell_time) select_partition_month,a.province,a.is_sold FROM erp_dw.dwd_cars a 
  #                         LEFT JOIN erp_dw.dwd_car_order_cars b ON a.car_id=b.car_id                                     
  #                         INNER JOIN ods_erp.ods_car_basic_config c ON a.config_id=c.id
  #                         INNER JOIN yckdc_da2_ucar.yck_it_query_config_id d ON c.autohome_id=d.id_autohome 
  #                         LEFT JOIN (SELECT car_id FROM yckdc_da2_ucar.evalmonitor_yckit_selling WHERE is_sold=1 OR eval_date>DATE_sub(NOW(),INTERVAL 7 day)) e ON a.car_id=e.car_id 
  #                         WHERE e.car_id IS NULL AND a.license_reg_date>'1990-01-01' AND a.kilometre>=0 AND a.operate_status!=1")
  char_sql_selling<-c("SELECT a.car_id FROM erp_dw.dwd_cars a 
           LEFT JOIN (SELECT car_id FROM yckdc_da2_ucar.evalmonitor_yckit_selling WHERE is_sold=1 OR eval_date>DATE_sub(NOW(),INTERVAL 7 day)) e ON a.car_id=e.car_id 
     WHERE e.car_id IS NULL AND a.license_reg_date>'1990-01-01' AND a.kilometre>=0 AND a.operate_status!=1")
}else{
  char_sql_selling<-c("SELECT a.car_id FROM erp_dw.dwd_cars a 
           LEFT JOIN yckdc_da2_ucar.evalmonitor_yckit_selling e ON a.car_id=e.car_id 
     WHERE e.car_id IS NULL AND a.license_reg_date>'1990-01-01' AND a.kilometre>=0 AND a.operate_status!=1")
}
evalMonitor_yckit_selling(price_model_loc,local_defin,char_sql_selling)
