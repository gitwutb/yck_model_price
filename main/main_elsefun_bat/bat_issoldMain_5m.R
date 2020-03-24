##*******************可售不可售计算/5分钟一次************##
rm(list = ls(all=T))
gc()
library(parallel)
price_model_loc<-gsub("(\\/main|\\/bat).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"/main/model_interface.R"),echo=FALSE,encoding="utf-8")
###IT云操作第一部分：IT云取数######
# ip_out<-sqllogin_salarable_yck
# ip_in<-sqllogin_salarable_dc
##内部函数集合


t1<-Sys.time()
tryCatch(
  {MainFun_yck_itd_cars_salable(sqllogin_salarable_yck,sqllogin_salarable_dc)},
  error = function(e) {fun_mailsend("YCK_ITD_DAILY-test",paste0('可售不可售计算失败-及时关注处理:',e))})
Sys.time()-t1
