#####model test
rm(list = ls(all=T))
gc()
price_model_loc<-gsub("\\/main","",dirname(rstudioapi::getActiveDocumentContext()$path))
source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")
loc_channel<-dbConnect(MySQL(),user = "yckdc",host="47.106.189.86",password= "YckDC888",dbname="yck-data-center")
dbSendQuery(loc_channel,'SET NAMES gbk')
input_tra<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_query WHERE user_query_id=",50)),-1)%>%
  dplyr::select(-user_query_id,-query_statue)
dbDisconnect(loc_channel)
return_datatest<-model_interface_datatest(input_tra)
if(return_datatest=='N'){
  model_interface_train(input_tra)
}else{
  print(return_datatest)
}


######output test2##########
parameter_user_query_id=4
parameter_classification_operational='营运'
parameter_model_number=1
parameter_province='广东'
interface_out1_model_mprice(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out2_detail_th(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out3_model_fprice(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out4_model_diprice(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out5_sh_mprice(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out6_display(parameter_user_query_id,parameter_classification_operational,parameter_model_number,parameter_province)
interface_out7_detail_mth(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out8_match_price(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out9_sales_new(parameter_user_query_id,parameter_classification_operational,parameter_model_number)
interface_out10_prate(parameter_user_query_id,parameter_classification_operational,parameter_model_number)