###############fun_model_input模型使用数据选取########
outline_all_fun_input<-function(){
  part_select<-c("SELECT car_platform,model_year,brand,series,model_name,auto,car_level,regDate,quotes,model_price,mile,province,user_years,a.partition_month,a.date_add FROM analysis_wide_table a")
  ######数据库查询
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  input_orig<-dbFetch(dbSendQuery(loc_channel,part_select),-1)
  dbDisconnect(loc_channel)
  
  ##############数据处理##############
  input_orig<-data.frame(input_orig,quotes_p=round(input_orig$quotes/input_orig$model_price,2))
  input_orig<-input_orig%>%dplyr::filter(quotes_p>0.05&quotes_p<0.95)
  input_orig<-input_orig%>%dplyr::filter(user_years<1|quotes_p<1)
  input_orig<-input_orig%>%dplyr::filter(regDate>'2000-01-01')
  input_orig<-data.frame(input_orig,reg_year=str_sub(input_orig$regDate,1,4),
                         reg_month=str_sub(input_orig$regDate,6,7),
                         parti_year=str_sub(input_orig$partition_month,1,4),
                         parti_month=str_sub(input_orig$partition_month,5,6))
  return(input_orig)
}
