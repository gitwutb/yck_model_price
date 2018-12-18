##############fun_factor_transform用于将一些变量转化为因子变量
##20180117--14:00修改model_price为数值型
fun_factor_standar<-function(input_value){
  ##factor的level水平（因子选取的宽度对于模型输出有影响）
  level_month<-c("01","02","03","04","05","06","07","08","09","10","11","12")
  level_year<-c(1997:2030)
  level_platform<-c("che168","rrc","youxin","guazi","yiche","che58","souche","czb","csp")
  level_province<-c("安徽","陕西","上海","湖南","广东","湖北","江苏","广西","浙江","辽宁","北京",
                    "山东","河南","四川","河北","福建","黑龙江","山西","重庆","新疆","贵州","吉林",
                    "云南","海南","天津","江西","甘肃","宁夏","内蒙古","青海","西藏" )
  level_auto<-c("自动","手动","电动")
  level_car_level<-c("中大型SUV","中大型车","中型车","中型SUV","紧凑型车","跑车","MPV","紧凑型SUV","小型SUV","小型车",
                     "皮卡","全尺寸SUV","微型车","豪华车","微面","客车","其它","卡车")
  input_value$car_platform<-factor(input_value$car_platform,levels=level_platform)
  ####20180117修改（价格只有一个） 
  # if(length(unique(input_value$model_price))==1){
  #   input_value$model_price<-factor(input_value$model_price,levels = c(unique(input_value$model_price),unique(input_value$model_price)-0.01))
  # }else{
  #   input_value$model_price<-factor(input_value$model_price,levels = unique(input_value$model_price))
  # }
 #######20171214修改#########
  input_value$province<-factor(input_value$province,levels = level_province)
  input_value$reg_year<-factor(input_value$reg_year,levels = level_year)
  #input_value$reg_month<-factor(input_value$reg_month,levels = level_month)
  #input_value$parti_year<-factor(input_value$parti_year,levels = level_year)
  input_value$parti_month<-factor(input_value$parti_month,levels = level_month)
  #######20180711修改#########
  input_value$auto<-factor(input_value$auto,levels = level_auto)
  #input_value$car_level<-factor(input_value$car_level,levels = level_car_level)
  input_value$model_year<-factor(input_value$model_year,levels = level_year)
  input_value$mile<-as.numeric(as.character(input_value$mile))
  return(input_value)
}