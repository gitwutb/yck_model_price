##*******************增量数据：此程序为优车库匹配汽车之家详细配置文件************##
rm(list = ls(all=T))
gc()
library(dplyr)
library(RMySQL)
library(mailR)
local_file<-paste0(gsub("(\\/bat|\\/main|\\/main_local).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()})))
#local_file<-'//User-20170720ma/yck_wash_model_new'
source(paste0(local_file,"/function/yck_base_function.R"),echo=FALSE,encoding="utf-8")
source(paste0(local_file,"/main/main_dctoit_prj/cfun_yck_itd_cars_salable.R"),echo=FALSE,encoding="utf-8")
sqllogin_salarable_dc<-fun_mysql_config_up("10")
sqllogin_salarable_yck<<-fun_mysql_config_up('yckt')
###IT云操作第一部分：IT云取数######
ip_out<-sqllogin_salarable_yck
ip_in<-sqllogin_salarable_dc
##内部函数集合
##全局变量函数input_type为1代表非IT接口
fun_global_variable<-function(){
  ###取数第二部分：本地云取数###
  #yck库取数
  sql_violation_license<<-paste0("SELECT car_id,deal_status,update_type,violation_status,is_over,record_id,FROM_UNIXTIME(query_time) violation_query_time,violation_info2 FROM yck_cars_violation 
                                 INNER JOIN yck_cars ON yck_cars.id=yck_cars_violation.car_id
                                 WHERE (FROM_UNIXTIME(query_time)>'",as.character(Sys.time()-2*60*60),"' OR FROM_UNIXTIME(update_time)>'",as.character(Sys.time()-2*60*60),"')")
  sql_assets<<-paste0("SELECT a.car_id,driving_license_status,register_license_status,mortgage_status,
                      insurance_status,FROM_UNIXTIME(operator_time) license_operator_time,operator_id,compulsory_insurance_label,yearly_inspection_label FROM yck_assets a
                      INNER JOIN yck_cars ON yck_cars.id=a.car_id
                      WHERE FROM_UNIXTIME(operator_time)>'",as.character(Sys.time()-2*60*60),"'")
  sql_cars<<-paste0("SELECT a.id car_id,is_sold,is_stop,auction_time_id,pm_transfer,operate_status,c.type project_type,is_confirm,inspection_valid_date FROM yck_cars a 
                    INNER JOIN yck_project c ON a.belong_project=c.id
                    WHERE operate_status IN (3,4) AND is_sold =0 ")
  sql_car_application_ing<<-paste0("SELECT car_id,1 is_application FROM yck_application a 
                                   INNER JOIN yck_application_cars b ON a.id=b.apply_id WHERE b.car_status NOT IN (3,4) AND (a.status!=5 AND a.is_delete=0)")
  
  #link库操作
  sqlu_yckdclink_cars_statustype<<-paste0("UPDATE link.yck_dti_cars_salable SET status_type=0 WHERE status_type=1")
  sqld_yckdclink_cars_statustype<<-paste0("DELETE FROM link.yck_dti_cars_salable WHERE status_type=0")
  ###取数第二部分：本地云取数###
  sql_yckdc_itd_all<<-paste0("SELECT a.car_id,b.violation_status,b.is_over,b.sum_degree,b.max_degree,c.driving_license_status,c.mortgage_status FROM yckdc_interactive_it.yck_itd_cars_tempid_5 a
                             LEFT JOIN yckdc_interactive_it.yck_itd_violation_license_5 b ON a.car_id=b.car_id
                             LEFT JOIN yckdc_interactive_it.yck_itd_assets_5 c ON a.car_id=c.car_id")
}

#车务-详情
fun_query2_assets<-function(ip_out,ip_in,sql_inputstr){
  query2_assets<-fun_mysqlload_query(ip_out,sql_inputstr)
  fun_mysqlload_add_upd(local_file,ip_in,query2_assets,'yckdc_interactive_it.yck_itd_assets_5')
}

###****************调用1：定时任务主函数（每三分钟一次）********##ip_out为It输出；ip_in为数据中心
MainFun_yck_itd_cars_salable<-function(ip_out,ip_in){
  fun_global_variable()
  ##%%计算%%##
  query1_violation_license<-fun_mysqlload_query(ip_out,sql_violation_license)
  if(nrow(query1_violation_license)>0){
    query1_violation_license<-fun_query1_violation_license(query1_violation_license)
    fun_mysqlload_add_upd(local_file,ip_in,query1_violation_license,'yckdc_interactive_it.yck_itd_violation_license_5')
  }
  fun_query2_assets(ip_out,ip_in,sql_assets)
  ###第二部分：IT云yck提取增量数据（保存car_id临时）
  #temp_ycklink_carstatus<-fun_mysqlload_query(ip_out,sqlq_ycklink_carstatus)
  yckit1_query_cars_application_ing<-fun_mysqlload_query(ip_out,sql_car_application_ing)
  yckit2_query_cars<-fun_mysqlload_query(ip_out,sql_cars)
  yckdc_itd_cars_tempid<-yckit2_query_cars %>% dplyr::select(car_id)
  fun_mysqlload_all(local_file,ip_in,yckdc_itd_cars_tempid,'yckdc_interactive_it.yck_itd_cars_tempid_5')
  ###第三部分：DC云中选取需要计算的部分数据计算。并与ITyck数据糅合
  yckdc_itd_va_all<-fun_mysqlload_query(ip_in,sql_yckdc_itd_all)
  all_yck_cars<-left_join(yckit2_query_cars,yckit1_query_cars_application_ing,by='car_id')
  all_yck_cars<-inner_join(all_yck_cars,yckdc_itd_va_all,by='car_id')
  temp_out<-dealFun_cars_salable_reason(all_yck_cars)
  all_yck_cars_out<-temp_out$all_yck_cars_out
  temp_out_r<-temp_out$temp_out_r
  # ##**************输出写入1*************##
  # all_yck_cars_out$car_id<-as.numeric(all_yck_cars_out$car_id)
  # result_output<-inner_join(all_yck_cars_out,all_yck_cars[,c('car_id','is_sold')],by='car_id') %>% dplyr::filter(is_sold==0) %>% dplyr::select(-is_sold)
  # fun_mysqlload_query(ip_out,sqlu_yckdclink_cars_statustype)
  # fun_mysqlload_add_upd(local_file,ip_out,result_output,'yck_dti_cars_salable')
  # fun_mysqlload_query(ip_out,sqld_yckdclink_cars_statustype)
  # ##**************输出写入2*************##
  # result_output2<-inner_join(all_yck_cars_out,all_yck_cars[,c('car_id','is_sold')],by='car_id') %>% dplyr::filter(is_sold==1) %>% dplyr::select(-is_sold)
  # fun_mysqlload_all(local_file,ip_out,result_output2,'yck_dti_cars_salable_sold')
  ##**************输出写入1*************##
  fun_mysqlload_query(ip_out,sqlu_yckdclink_cars_statustype)
  fun_mysqlload_add_upd(local_file,ip_out,all_yck_cars_out,'link.yck_dti_cars_salable')
  fun_mysqlload_query(ip_out,sqld_yckdclink_cars_statustype)
  ##**************输出写入3*************##
  ##第六部分：数据历史状态留存（保存异常变动记录）--缺陷如果时间1  状态1是A  时间2变成B（可记录） 时间3又变成A（记录不了）
  fun_mysqlload_add(local_file,ip_in,temp_out_r,'yckdc_interactive_it.yck_dti_cars_salable_history')
  fun_mysqlload_all(local_file,ip_in,temp_out_r,'yckdc_interactive_it.yck_dti_cars_salable_detail')
  #fun_mysqlload_all(local_file,ip_in,all_yck_cars_out,'yck_dti_cars_salable')
}

# t1<-Sys.time()
# tryCatch(
#   {MainFun_yck_itd_cars_salable(sqllogin_salarable_yck,sqllogin_salarable_dc)},
#   error = function(e) {fun_mailsend("YCK_ITD_DAILY",paste0('可售不可售计算失败-及时关注处理:',e))})
# Sys.time()-t1
t1<-Sys.time()
tryCatch(
  {MainFun_yck_itd_cars_salable(sqllogin_salarable_yck,sqllogin_salarable_dc)},
  error = function(e) {fun_mailsend("YCK_ITD_DAILY-test",paste0('可售不可售计算失败-及时关注处理:',e))})
Sys.time()-t1
