loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id,select_regDate,select_mile,select_partition_month,yck_query_id  
                                                     FROM yck_it_query_mresult WHERE yck_query_id in (",paste0(c(450),collapse = ','),')')),-1)%>%
  dplyr::select(select_model_id,select_regDate,select_mile,select_partition_month,yck_query_id) %>% unique()
dbDisconnect(loc_channel)

select_input<-select_input[1,]
select_input_transfor<-fun_select_transfor(select_input)
case<-fun_parameter_ym(select_input_transfor$select_partition_month,select_input_transfor$select_regDate)$case
model_code<-paste0(select_input_transfor$yck_seriesid,"T","IME","CASE",case,sep="")%>%toupper()
#模型高效处理方法
input_test<-fun_input_test(select_input_transfor)
list_model<-list.files(paste0(price_model_loc,"\\model_net"), full.names = T,pattern = "CASE11.RData")
list_model<-gsub(".*model_net\\/|.RData","",list_model)

if(length(grep(model_code,list_model))==0){
  input_analysis<-fun_input(select_input_transfor)
  input_train<-fun_input_train(input_analysis,select_input_transfor)
  model.svm<-fun_model_train(input_train,price_model_loc,model_code)
  output_pre<-fun_model_test(select_input_transfor,input_test,model.svm)
  return_pred_out<-fun_pred_out(input_train,model.svm,select_input)
  output_pre<-data.frame(output_pre,return_pred_out)
}else{
  load(paste0(paste0(price_model_loc,"\\model_net"),"\\",model_code,".RData"))
  load(paste0(paste0(price_model_loc,"\\model_net"),"\\",model_code,"input_train.RData"))
  output_pre<-fun_model_test(select_input_transfor,input_test,model.svm)
  return_pred_out<-fun_pred_out(input_train,model.svm,select_input)
  output_pre<-data.frame(output_pre,return_pred_out)
}




lf<-list.files(paste0(price_model_loc,"/model_net",sep=""), full.names = T,pattern = ".RData")
lf_list<-gsub('.*/model_net/|TIMECASE.*','',lf) %>% unique()

loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input_org1<-dbFetch(dbSendQuery(loc_channel,"SELECT MIN(a.model_id) select_model_id,yck_seriesid 
                                      FROM config_vdatabase_yck_major_info a GROUP BY a.yck_seriesid;"),-1)
dbDisconnect(loc_channel)

select_input_org<-inner_join(select_input_org,select_input_org1,by='select_model_id')

select_input_org<-select_input_org %>% dplyr::filter(!(yck_seriesid %in%lf_list)& select_model_id!=1182769) %>% dplyr::select(-yck_seriesid)

select_input_org<-select_input_org %>% dplyr::filter((yck_seriesid %in%lf_list)& select_model_id!=1182769) %>% dplyr::select(-yck_seriesid)



future_test<-function(i){
  select_input<-select_input_org[i,]
  select_input_future<-NULL
  for (i in 1:6) {
    linshi_input<-select_input
    linshi_input$select_partition_month<-as.character(as.Date(select_input$select_partition_month)+31*(i-1))
    select_input_future<-rbind(select_input_future,linshi_input)
  }
  select_input_future$select_mile<-select_input$select_mile/as.numeric(round(difftime(as_datetime(select_input$select_partition_month),as_datetime(select_input$select_regDate),units="days")/365,2))*
    as.numeric(round(difftime(as_datetime(select_input_future$select_partition_month),as_datetime(select_input_future$select_regDate),units="days")/365,2))
  
  match_output_future<-NULL
  for (i in 1:nrow(select_input_future)) {
    linshi1<-fun_pred(select_input_future[i,])
    match_output_future<-rbind(match_output_future,linshi1)
  }
  re<-dplyr::summarise(group_by(match_output_future,select_model_id,select_regDate,select_partition_month),fb_price=round(mean(fb),2))%>%
    ungroup()%>%as.data.frame()%>%dcast(select_model_id~select_partition_month)
  return(re)
}
finale_ttt1<-NULL
for (i in 1:948) {
  ttt<-future_test(i)
  finale_ttt1<-rbind(finale_ttt1,ttt)
}
