print("ML model start:")
options(warn =-1)
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
library(mailR)
library(rlist)
library(RJSONIO)
#library(plyr)
price_model_loc<-gsub("(\\/main|\\/bat).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"/function/yck_base_function.R"),echo=FALSE,encoding="utf-8")
source(paste0(price_model_loc,"/function/fun_model_price_test.R"),echo=FALSE,encoding="utf-8")
source(paste0(price_model_loc,"/function/SerieStandardFun.R"),echo=FALSE,encoding="utf-8")
source(paste0(price_model_loc,"/main/main_eval_interface/yckit_project_user.R"),echo=FALSE,encoding="utf-8")
source(paste0(price_model_loc,"/main/main_eval_interface/f_model_interface.R"),echo=FALSE,encoding="utf-8")
local_defin<-fun_mysql_config_up("47")

##可售/不可售接口调用
sqllogin_salarable_dc<-fun_mysql_config_up("10")
sqllogin_salarable_yck<<-fun_mysql_config_up('yckt')
source(paste0(price_model_loc,"/main/main_elsefun_bat/issoldFun_cars_salable.R"),echo=FALSE,encoding="utf-8")
