##估值接口
#产品调用
model_interface_train<-function(input_tra){
  # 数据输入：#
  before_query <-input_tra
  max_user_query_id<-read.table(paste0(price_model_loc,"/file/max_user_query_id.txt")) %>% as.character() %>% as.integer()
  write.table(max_user_query_id+1,paste0(price_model_loc,"/file/max_user_query_id.txt"),row.names = F,col.names = F,append = F)
  if(before_query$select_classification_car=='期车'){before_query$select_mile<-4*as.numeric(round(difftime(as_datetime(before_query$select_partition_month),as_datetime(before_query$select_regDate),units="days")/365,2))}
  yck_project_model_query<-data.frame(user_query_id=max_user_query_id+1,before_query,query_statue=1)
  yck_project_model_query$add_time<-as.character(format(Sys.time(),"%Y/%m/%d %H:%M:%S"))
  yck_project_model_query<-yck_project_model_query %>% dplyr::select(user_query_id,user_id,select_model_id,select_regDate,select_mile,select_partition_month,
                                                                     select_classification_operational,select_classification_car,add_time,query_statue)
  fun_mysqlload_add(price_model_loc,local_defin,yck_project_model_query,'yck_project_model_query',max_user_query_id)
  select_input<-yck_project_model_query%>%dplyr::select(user_query_id,select_model_id,select_regDate,select_mile,select_partition_month)
  
  select_input<-select_input%>%dplyr::select(user_query_id,select_model_id,select_regDate,select_mile,select_partition_month)
  select_input$select_mile<-as.numeric(select_input$select_mile)
  return_post_model<-tryCatch({model_main(select_input,"")},
                              error=function(e){3},
                              finally={4})
  if(return_post_model!=0){
    fun_mysqlload_query(local_defin,paste0("UPDATE yck_project_model_query SET query_statue=",return_post_model," WHERE user_query_id=",unique(select_input$user_query_id),";"))
  }
  return(return_post_model)
}

#后台性调用:车源估值调用完整版（手工车源）
model_interface_train_yck<-function(input_tra){
  #数据输入：#
  before_query <-input_tra
  max_user_query_id_n<-read.table(paste0(price_model_loc,"/file/max_user_query_id_n.txt")) %>% as.character() %>% as.integer()
  write.table(max_user_query_id_n+1,paste0(price_model_loc,"/file/max_user_query_id_n.txt"),row.names = F,col.names = F,append = F)
  if(before_query$select_classification_car=='期车'){before_query$select_mile<-4*as.numeric(round(difftime(as_datetime(before_query$select_partition_month),as_datetime(before_query$select_regDate),units="days")/365,2))}
  if(length(grep('select_tname',names(before_query)))==0){before_query<-before_query %>% dplyr::mutate(select_tname='PC')}
  if(length(grep('yck_car_id',names(before_query)))==0){before_query<-before_query %>% dplyr::mutate(yck_car_id=0)}
  if(length(grep('query_number',names(before_query)))==0){before_query<-before_query %>% dplyr::mutate(query_number=1)}
  yck_project_model_n_query<-data.frame(user_query_id=max_user_query_id_n+1,before_query,query_statue=1)
  yck_project_model_n_query$add_time<-as.character(format(Sys.time(),"%Y/%m/%d %H:%M:%S"))
  yck_project_model_n_query<-yck_project_model_n_query %>% dplyr::select(user_query_id,user_id,select_tname,yck_car_id,query_number,select_model_id,select_regDate,select_mile,select_partition_month,
                                                                         select_classification_operational,select_classification_car,add_time,query_statue)
  fun_mysqlload_add(price_model_loc,local_defin,yck_project_model_n_query,'yck_project_model_n_query',max_user_query_id_n)
  select_input<-yck_project_model_n_query%>%dplyr::select(user_query_id,select_model_id,select_regDate,select_mile,select_partition_month)
  select_input$select_mile<-as.numeric(select_input$select_mile)
  return_post_model<-tryCatch({model_main(select_input,"_n")},
                              error=function(e){3},
                              finally={4})
  if(return_post_model!=0){
    fun_mysqlload_query(local_defin,paste0("UPDATE yck_project_model_n_query SET query_statue=",return_post_model," WHERE user_query_id=",unique(select_input$user_query_id),";"))
  }
  return(return_post_model)
}

#后台性调用:车源估值调用(手工车源-参数输入调用接口)
model_interface_train_yckdc<-function(input_parameter){
 if(input_parameter==0){
   select_input<-fun_mysqlload_query(local_defin,'SELECT * FROM yck_project_temp_input')
   select_input<-select_input %>% mutate(user_id=1,add_time=Sys.time())
   for (i in 1:nrow(select_input)) {
     model_interface_train_yck(select_input[i,])
   }
   fun_mailsend("YCK手工车源车源估值专属",'项目估值完成，请查看报告')
 }else{fun_mailsend("YCK手工车源车源估值专属",'重新输入正确的参数：0')}
}
