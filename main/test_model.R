#主要用于测试模型输出选取折扣率还是残值
rm(list = ls(all=T))
gc()
library(RODBC)
library(reshape2)
library(dplyr)
library(ggplot2)
library(RMySQL)
library(stringr)
library(e1071) 
library(tcltk)
library(lubridate)
##########数据输入
###########加载自定义函数###########paste0(price_model_loc,"\\function")
price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
source(paste0(price_model_loc,"\\function\\fun_model_price_test.R"),echo=FALSE,encoding="utf-8")
############################模型链条完善##############################
#############################测试数据1：########################################
select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst.csv"),header = T)
loc_channel<-dbConnect(MySQL(),user = "yckdc",host="47.106.189.86",password= "YckDC888",dbname="yck-data-center")
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_query WHERE user_query_id in (",paste0(150:200,collapse = ','),')')),-1)%>%
  dplyr::select(select_model_id,select_regDate,select_mile,select_partition_month)
dbDisconnect(loc_channel)


#函数训练结果比对
test_train<-function(select_input){
  select_input<-data.frame(select_model_id=select_input[1],select_regDate=select_input[2],select_mile=select_input[3],select_partition_month=select_input[4])
  select_input_transfor<-fun_select_transfor(select_input)
  case<-fun_parameter_ym(select_input_transfor$select_partition_month,select_input_transfor$select_regDate)$case
  model_code<-paste0(select_input_transfor$select_series,"T",paste0(format(as.Date(Sys.Date()),"%Y"),week(Sys.Date())),"CASE",case,sep="")%>%toupper()
  #模型高效处理方法
  input_test<-fun_input_test(select_input_transfor)
  input_analysis<-fun_input(select_input_transfor)
  input_train<-fun_input_train(input_analysis,select_input_transfor)
  model.svm<-fun_model_train(input_train,price_model_loc,model_code)
  output_pre<-fun_model_test(select_input_transfor,input_test,model.svm)
  #20180713增加（输出辅助信息-训练样本量）(0905修改)
  sample_size<-input_train%>%dplyr::mutate(zb=ifelse(car_platform=="czb","pm_n",ifelse(car_platform=="csp","pm_n","fb_n")))%>%
    group_by(zb)%>%dplyr::summarise(sample_size=n())%>%as.data.frame()%>%dcast(.~zb)%>%dplyr::select(-.)
  
  fun_input_train1<-function(input_analysis,select_input_transfor){
    select_brand<-select_input_transfor$select_brand
    select_series<-select_input_transfor$select_series
    select_model_year<-select_input_transfor$select_model_year
    select_model_price<- select_input_transfor$select_model_price
    select_auto<-select_input_transfor$select_auto
    select_car_level<-select_input_transfor$select_car_level
    select_regDate<-as.Date(select_input_transfor$select_regDate)
    select_mile<-select_input_transfor$select_mile
    select_partition_month<-as.Date(select_input_transfor$select_partition_month)
    ##变换参数
    parameter_ym_value<-fun_parameter_ym(select_partition_month,select_regDate)
    user_years_plower=parameter_ym_value$user_years_plower
    user_years_pupper=parameter_ym_value$user_years_pupper
    mile_plower=parameter_ym_value$mile_plower
    mile_pupper=parameter_ym_value$mile_pupper
    case=parameter_ym_value$case
    
    ###训练数据选取--
    input_analysis$quotes<-input_analysis$quotes_p
    input_train<-input_analysis%>%dplyr::filter(user_years>user_years_plower&user_years<user_years_pupper&mile>mile_plower&mile<mile_pupper)%>%
      dplyr::select(-quotes_p,-regDate,-partition_month,-brand,-series,-parti_year,-reg_month,-car_level) 
    ##20180720##
    if(nrow(input_train)<500){
      input_train<-input_analysis%>%
        dplyr::select(-quotes_p,-regDate,-partition_month,-brand,-series,-parti_year,-reg_month,-car_level)
    }else{
      print(paste0(select_series,"初始样本量为",nrow(input_train)))
    }
    
    #2018年4月27日##
    parameter_residuals<-0.09
    input_train_linshi<-NULL
    for (i in unique(input_train$car_platform)) {
      input_train_one<-input_train%>%dplyr::filter(car_platform==i)
      aa1<-lm(input_train_one$quotes~input_train_one$mile+input_train_one$user_years)
      input_train_one<-input_train_one%>%dplyr::mutate(residuals=aa1$residuals,fit=aa1$fitted.values)%>%dplyr::filter(abs(residuals)<parameter_residuals)
      #----20180711增加（当训练样本过多则选取精度最高的样本）
      parameter_res=parameter_residuals
      while (nrow(input_train_one)>2500) {
        parameter_res=parameter_res-0.002
        input_train_one<-input_train_one%>%dplyr::filter(abs(residuals)<parameter_res)
      }
      #----20180711增加######print(paste0(i,parameter_res))
      input_train_linshi<-rbind(input_train_linshi,input_train_one)
    }
    
    #拼接##
    input_train<-input_train_linshi%>%dplyr::select(-residuals,-fit)
    #因子变量因子水平（使训练及测试数据样式归一化）
    input_train<-fun_factor_standar(input_train)
    return(input_train=input_train)
  }
  input_train<-fun_input_train1(input_analysis,select_input_transfor)
  model.svm<-fun_model_train(input_train,price_model_loc,model_code)
  output_pre1<-fun_model_test(select_input_transfor,input_test,model.svm)
  output_pre1$fb<-output_pre1$fb*output_pre1$select_model_price
  output_pre1$pm<-output_pre1$pm*output_pre1$select_model_price
  output_pre<-data.frame(output_pre,output_pre1[,13:14])
  result_tag<-summarise(group_by(output_pre,select_model_id,select_model_name,select_model_price,select_regDate,
                                 select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm),fb_price1=mean(fb.1),pm_price1=mean(pm.1))%>%
    ungroup()%>%as.data.frame()
  return(result_tag)
}
result_tag<-apply(select_input,1,test_train)
result_tag<-list.rbind(result_tag)

# result_tag<-NULL
# for (i in 1:nrow(select_input)) {
#   linshi<-test_train(select_input[i,])
#   linshi1<-linshi
#   result_tag<-rbind(result_tag,linshi1)
# }
# result_tag<-apply(select_input,1,test_train)
