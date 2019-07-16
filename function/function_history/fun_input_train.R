##################################本函数为建模数据选取及处理过程############################################
######最新修改日期：2018年4月27日（25）#######
fun_input_train<-function(input_analysis,select_input_transfor){
  ###############需要建模数据处理----------
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
  
  ###############训练数据选取----------
  input_train<-input_analysis%>%dplyr::filter(user_years>user_years_plower&user_years<user_years_pupper&mile>mile_plower&mile<mile_pupper)%>%
    dplyr::select(-quotes_p,-model_name,-regDate,-partition_month,-brand,-series,-parti_year,-reg_month,-date_add,-car_level) 
  ###############20180720############
  if(nrow(input_train)<100){
    input_train<-input_analysis%>%
      dplyr::select(-quotes_p,-model_name,-regDate,-partition_month,-brand,-series,-parti_year,-reg_month,-date_add,-car_level)
  }else{
    print(paste0(select_series,"初始样本量为",nrow(input_train)))
  }
  ###################################
  
  ###################2018年4月27日##
  parameter_residuals<-0.09
  input_train_linshi<-NULL
  for (i in unique(input_train$car_platform)) {
    input_train_one<-input_train%>%dplyr::filter(car_platform==i)
    aa1<-lm((input_train_one$quotes/input_train_one$model_price)~input_train_one$mile+input_train_one$user_years)
    input_train_one<-input_train_one%>%dplyr::mutate(residuals=aa1$residuals,fit=aa1$fitted.values)%>%dplyr::filter(abs(residuals)<parameter_residuals)
    #########----20180711增加（当训练样本过多则选取精度最高的样本）###########
    parameter_res=parameter_residuals
    while (nrow(input_train_one)>2500) {
      parameter_res=parameter_res-0.002
      input_train_one<-input_train_one%>%dplyr::filter(abs(residuals)<parameter_res)
    }
    #########----20180711增加######print(paste0(i,parameter_res))
    input_train_linshi<-rbind(input_train_linshi,input_train_one)
  }
  
  ##########拼接######
  input_train<-input_train_linshi%>%dplyr::select(-residuals,-fit)
  ###################
  
  #因子变量因子水平（使训练及测试数据样式归一化）
  input_train<-fun_factor_standar(input_train)
  return(input_train=input_train)
}