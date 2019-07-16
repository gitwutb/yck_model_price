##估值接口
###接口调用函数
model_interface_datatest<-function(input_tra){
  ############################数据输入：########################################
  before_query <-input_tra
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  test_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_query WHERE select_model_id=",before_query$select_model_id,
                                                     " AND user_id=",before_query$user_id,
                                                     " AND select_classification_operational='",enc2native(as.character(before_query$select_classification_operational)),"'",
                                                     " AND select_classification_car='",enc2native(as.character(before_query$select_classification_car)),"'",
                                                     " AND ABS(select_mile-",before_query$select_mile,")<0.5",
                                                     " AND ABS(DATEDIFF(select_regDate,'",before_query$select_regDate,"'))<2",
                                                     " AND ABS(DATEDIFF(select_partition_month,'",before_query$select_partition_month,"'))<2")),-1)
  max_user_query_id<-dbFetch(dbSendQuery(loc_channel,"select MAX(user_query_id) user_query_id from yck_project_model_query"),-1)%>%as.integer()
  dbDisconnect(loc_channel)
  if(nrow(test_query)>0){
    return_post_model='Y'   ##1表示“请勿重复提交”
  }else{
    return_post_model='N'
  }
  return(return_post_model)
}

#产品调用
model_interface_train<-function(input_tra){
  #数据输入：#
  before_query <-input_tra
  max_user_query_id<-read.table(paste0(price_model_loc,"/file/max_user_query_id.txt")) %>% as.character() %>% as.integer()
  write.table(max_user_query_id+1,paste0(price_model_loc,"/file/max_user_query_id.txt"),row.names = F,col.names = F,append = F)
  if(before_query$select_classification_car=='期车'){before_query$select_mile<-4*as.numeric(round(difftime(as_datetime(before_query$select_partition_month),as_datetime(before_query$select_regDate),units="days")/365,2))}
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  yck_project_model_query<-data.frame(user_query_id=max_user_query_id+1,before_query,query_statue=1)
  yck_project_model_query$add_time<-as.character(format(Sys.time(),"%Y/%m/%d %H:%M:%S"))
  yck_project_model_query<-yck_project_model_query %>% dplyr::select(user_query_id,user_id,select_model_id,select_regDate,select_mile,select_partition_month,
                                                                     select_classification_operational,select_classification_car,add_time,query_statue)
  write.csv(yck_project_model_query,paste0(price_model_loc,"/output/result/yck_project_model_query",max_user_query_id,".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_query",max_user_query_id,".csv"),"'",
                                 " INTO TABLE yck_project_model_query CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  dbDisconnect(loc_channel)
  select_input<-yck_project_model_query%>%dplyr::select(user_query_id,select_model_id,select_regDate,select_mile,select_partition_month)
  select_input$select_mile<-as.numeric(select_input$select_mile)
  return_post_model<-tryCatch({model_main(select_input)},
                              error=function(e){3},
                              finally={4})
  if(length(return_post_model)==3){
    #存入数据库
    fun_mysqlload_add(price_model_loc,local_defin,return_post_model$result_bd,'yck_project_model_result_tag',max_user_query_id)
    fun_mysqlload_add(price_model_loc,local_defin,return_post_model$result_compare,'yck_project_model_result_match',max_user_query_id)
    fun_mysqlload_add(price_model_loc,local_defin,return_post_model$result_future,'yck_project_model_result_future',max_user_query_id)
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    dbSendQuery(loc_channel,paste0("UPDATE yck_project_model_query SET query_statue=2 WHERE user_query_id=",unique(select_input$user_query_id),";"))
    dbDisconnect(loc_channel)
    return_post_model=0}else{return_post_model=return_post_model}
  if(return_post_model!=0){
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    dbSendQuery(loc_channel,paste0("UPDATE yck_project_model_query SET query_statue=",return_post_model," WHERE user_query_id=",unique(select_input$user_query_id),";"))
    dbDisconnect(loc_channel)
  }
  return(return_post_model)
}

#后台性调用:车源估值调用完整版（系统车源+手工车源）
model_interface_train_yck<-function(input_tra){
  #数据输入：#
  before_query <-input_tra
  max_user_query_id_n<-read.table(paste0(price_model_loc,"/file/max_user_query_id_n.txt")) %>% as.character() %>% as.integer()
  write.table(max_user_query_id_n+1,paste0(price_model_loc,"/file/max_user_query_id_n.txt"),row.names = F,col.names = F,append = F)
  if(before_query$select_classification_car=='期车'){before_query$select_mile<-4*as.numeric(round(difftime(as_datetime(before_query$select_partition_month),as_datetime(before_query$select_regDate),units="days")/365,2))}
  if(length(grep('select_tname',names(before_query)))==0){before_query<-before_query %>% dplyr::mutate(select_tname='PC')}
  if(length(grep('yck_car_id',names(before_query)))==0){before_query<-before_query %>% dplyr::mutate(yck_car_id=0)}
  if(length(grep('query_number',names(before_query)))==0){before_query<-before_query %>% dplyr::mutate(query_number=1)}
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  yck_project_model_n_query<-data.frame(user_query_id=max_user_query_id_n+1,before_query,query_statue=1)
  yck_project_model_n_query$add_time<-as.character(format(Sys.time(),"%Y/%m/%d %H:%M:%S"))
  yck_project_model_n_query<-yck_project_model_n_query %>% dplyr::select(user_query_id,user_id,select_tname,yck_car_id,query_number,select_model_id,select_regDate,select_mile,select_partition_month,
                                                                     select_classification_operational,select_classification_car,add_time,query_statue)
  write.csv(yck_project_model_n_query,paste0(price_model_loc,"/output/result/yck_project_model_n_query",".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_n_query",".csv"),"'",
                                 " INTO TABLE yck_project_model_n_query CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  dbDisconnect(loc_channel)
  select_input<-yck_project_model_n_query%>%dplyr::select(user_query_id,select_model_id,select_regDate,select_mile,select_partition_month)
  select_input$select_mile<-as.numeric(select_input$select_mile)
  return_post_model<-tryCatch({model_main(select_input)},
                              error=function(e){3},
                              finally={4})
  if(length(return_post_model)==3){
    #存入数据库
    fun_mysqlload_add(price_model_loc,local_defin,return_post_model$result_bd,'yck_project_model_n_result_tag',max_user_query_id_n)
    fun_mysqlload_add(price_model_loc,local_defin,return_post_model$result_compare,'yck_project_model_n_result_match',max_user_query_id_n)
    fun_mysqlload_add(price_model_loc,local_defin,return_post_model$result_future,'yck_project_model_n_result_future',max_user_query_id_n)
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    dbSendQuery(loc_channel,paste0("UPDATE yck_project_model_n_query SET query_statue=2 WHERE user_query_id=",unique(select_input$user_query_id),";"))
    dbDisconnect(loc_channel)
    return_post_model=0}else{return_post_model=return_post_model}
  if(return_post_model!=0){
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    dbSendQuery(loc_channel,paste0("UPDATE yck_project_model_n_query SET query_statue=",return_post_model," WHERE user_query_id=",unique(select_input$user_query_id),";"))
    dbDisconnect(loc_channel)
  }
  return(return_post_model)
}

#后台性调用:车源估值调用(手工车源-参数输入调用接口)
model_interface_train_yckdc<-function(input_parameter){
 if(input_parameter==0){
   loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
   dbSendQuery(loc_channel,'SET NAMES gbk')
   select_input<-dbFetch(dbSendQuery(loc_channel,'SELECT * FROM yck_project_temp_input'),-1)
   dbDisconnect(loc_channel)
   select_input<-select_input %>% mutate(user_id=1,add_time=Sys.time())
   for (i in 1:nrow(select_input)) {
     model_interface_train_yck(select_input[i,])
   }
   fun_mailsend("YCK手工车源车源估值专属",'项目估值完成，请查看报告')
 }else{fun_mailsend("YCK手工车源车源估值专属",'重新输入正确的参数：0')}
}

#后台性调用:车源估值调用(系统车源-参数输入调用接口)
model_interface_train_yckdc2<-function(belong_project){
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  select_input_pre<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT a.project_name,a.car_id,a.autohome_id,a.license_reg_date,round(a.kilometre/10000,2) select_mile,b.id_che300 FROM yck_tableau_it_regular a 
                               LEFT JOIN yck_it_query_config_id b ON a.autohome_id=b.id_autohome
                               WHERE a.belong_project=",belong_project,";")),-1)
  dbDisconnect(loc_channel)
  if(anyNA(select_input_pre$id_che300)==F){
    select_input_pre<-select_input_pre %>% mutate(select_partition_month=Sys.Date()) %>%
      dplyr::select(select_model_id=id_che300,select_regDate=license_reg_date,select_mile,select_partition_month,select_tname=project_name,yck_car_id=car_id)
    select_input_pre$select_mile[which(select_input_pre$select_mile==0)]<-
      4*as.numeric(difftime(as_datetime(select_input_pre$select_partition_month[which(select_input_pre$select_mile==0)]),as_datetime(select_input_pre$select_regDate[which(select_input_pre$select_mile==0)]),units="days")/365)
    select_input<-select_input_pre %>% filter(select_regDate!='1999-01-01') %>% 
      mutate(user_id=1,select_classification_operational='非营运',select_classification_car='现车',add_time=Sys.time())
    for (i in 1:nrow(select_input)) {
      model_interface_train_yck(select_input[i,])
    }
  }else{
    fun_mailsend("YCK检测系统车源估值专属",paste0('请检查',belong_project,'号项目的汽车之家-车300ID匹配是否正确'))
  }
  fun_mailsend("YCK检测系统车源估值专属",paste0(belong_project,'号项目估值完成，请查看报告'))
}