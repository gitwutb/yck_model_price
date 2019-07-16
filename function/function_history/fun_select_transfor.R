###############fun_select_transfor模型使用数据选取########0711修改
fun_select_transfor<-function(select_input){
  part_select<-c("SELECT car_name select_brand,car_series1 select_series,model_year select_model_year,model_name select_model_name,model_price select_model_price,auto select_auto,car_level select_car_level FROM analysis_che300_cofig_info a INNER JOIN config_che300_major_info b ON a.car_id=b.model_id WHERE ")
  part_condition<-c("model_id =")
  part_value<-select_input$select_model_id
  part_all<-paste0(part_select,part_condition,part_value)
  ######数据库查询
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  input_orig<-dbFetch(dbSendQuery(loc_channel,part_all),-1)
  dbDisconnect(loc_channel)
  select_input_tansfor<-data.frame(input_orig,select_input)
  return(select_input_tansfor)
}
