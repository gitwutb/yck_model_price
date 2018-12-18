##################################本函数为建模数据选取及处理过程############################################
#2018/8/27更改select_partition_month的parti_year为reg_year
fun_input_test<-function(select_input_transfor){
  select_brand<-select_input_transfor$select_brand
  select_series<-select_input_transfor$select_series
  select_model_year<-select_input_transfor$select_model_year
  select_model_price<- select_input_transfor$select_model_price
  select_auto<-select_input_transfor$select_auto
  select_car_level<-select_input_transfor$select_car_level
  select_regDate<-as.Date(select_input_transfor$select_regDate)
  select_mile<-select_input_transfor$select_mile
  select_partition_month<-as.Date(select_input_transfor$select_partition_month)
  
  ##############测试数据输入-test_input---------0711修改剔除auto和car_level
  level_province<-c("安徽","陕西","上海","湖南","广东","湖北","江苏","广西","浙江","辽宁","北京",
                    "山东","河南","四川","河北","福建","黑龙江","山西","重庆","新疆","贵州","吉林",
                    "云南","海南","天津","江西","甘肃","宁夏","内蒙古","青海","西藏")
  level_platform<-c("che168","rrc","youxin","guazi","yiche","che58","souche","czb","csp")
  test_input<-data.frame(car_platform=level_platform,
                         model_year=select_model_year,
                         auto=select_auto,
                         #car_level=select_car_level,
                         model_price=select_model_price,
                         ##1-4万公里rep(1:5,each=6)
                         mile=select_mile,
                         ##30多个省份rep(unique(select_input_transfor$province),each=6)
                         province=rep(level_province,each=length(level_platform)),
                         user_years=as.numeric(as.character(round((select_partition_month-select_regDate)/365,2))),
                         reg_year=str_sub(select_regDate,1,4),
                         #reg_month=str_sub(select_regDate,6,7),
                         #parti_year=str_sub(select_partition_month,1,4),
                         parti_month=str_sub(select_partition_month,6,7))
  #因子变量因子水平（使训练及测试数据样式归一化）
  test_input<-fun_factor_standar(test_input)
  #test_input$model_price<-factor(test_input$model_price,levels = levels(ana1$model_price))
  return(test_input=test_input)
}