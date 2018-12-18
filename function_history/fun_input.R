###############fun_model_input模型使用数据选取########
fun_input<-function(select_input_transfor){
  part_select<-c("SELECT car_platform,model_year,brand,series,model_name,auto,car_level,regDate,quotes,model_price,mile,province,user_years,a.partition_month,a.date_add FROM analysis_wide_table a WHERE ")
  part_condition<-c("series =")
  part_value<-paste0("'",as.character(select_input_transfor$select_series),"'")
  part_all<-paste0(part_select,part_condition,part_value)
  ######数据库查询
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  input_orig<-dbFetch(dbSendQuery(loc_channel,part_all),-1)
  dbDisconnect(loc_channel)
  #input_analysis<-input_orig%>%dplyr::filter(car_platform!='czb',car_platform!='souche')
  input_analysis<-input_orig
  
  ##############数据处理##############
  input_analysis<-data.frame(input_analysis,quotes_p=round(input_analysis$quotes/input_analysis$model_price,2))
  input_analysis<-input_analysis%>%dplyr::filter(quotes_p>0.05&quotes_p<1.01)
  input_analysis<-input_analysis%>%dplyr::filter(user_years<1|quotes_p<1)
  input_analysis<-input_analysis%>%dplyr::filter(regDate>'2000-01-01')
  input_analysis<-data.frame(input_analysis,reg_year=str_sub(input_analysis$regDate,1,4),
                             reg_month=str_sub(input_analysis$regDate,6,7),
                             parti_year=str_sub(input_analysis$partition_month,1,4),
                             parti_month=str_sub(input_analysis$partition_month,5,6))
  return(input_analysis)
}
