###############fun_model_input模型使用数据选取########
outline_series_fun_input<-function(select_input_transfor){
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  part_value<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT car_series1 FROM analysis_che300_cofig_info WHERE car_id = ",select_input_transfor$select_model_id)),-1)%>%as.character()
  dbDisconnect(loc_channel)
  input_analysis<-input_orig%>%dplyr::filter(series==part_value)
  return(input_analysis)
}
