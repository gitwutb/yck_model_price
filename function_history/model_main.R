###########综合输出（产品）######
model_main<-function(select_input){
  car_id<-select_input$select_model_id
  ################第一部分：对标车型###############
  ####对标车型输出（提升效率：查询是否有过对标车型）（注意：为保证对标车型时效性，定期清理对标结果output的csv文件）
  list_matchfile<-list.files(paste0(price_model_loc,"\\output\\relation"), full.names = T,pattern = ".csv")
  list_matchfile<-gsub(".*output\\\\relation\\/|.csv","",list_matchfile)
  if(length(which(list_matchfile==car_id))==0){
    main_fun_series_standard(car_id)
  }else{
    print("已有对标")
  }
  car_match<- read.csv(paste0(price_model_loc,"\\output\\relation\\",car_id,".csv"),header = T,sep = ",")%>%
    dplyr::filter(model_id!=car_id)
  ###############去掉量少的车id
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  series_max<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT series_name,sum(count_s) cou FROM analysis_wide_table_cous
  WHERE series_name in (","'",paste0(car_match$series_name,collapse = "','",sep=''),"'",") GROUP BY series_name",sep='')),-1)
  series_max<-series_max%>%top_n(4,cou)%>%dplyr::filter(cou>1000)
  dbDisconnect(loc_channel)
  car_match<-car_match%>%dplyr::filter(series_name %in% series_max$series)

  
  ################第二部分：对应价格预测###############
  ######1.价格预测（当样本量可信度较低时调用对标车）######
  linshi<-fun_pred(select_input)
  output_pre<-linshi[[1]]%>%dplyr::mutate(query_lab="T")
  result_tag<-dplyr::summarise(group_by(output_pre,user_query_id,select_model_id,select_model_name,select_model_price,select_regDate,
                                 select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm))%>%
    ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))
  ########################################
  ########2.市场范围数据规范性校验#######
  sql_config<-paste0("SELECT platform_class car_platform_class,parmeter_y,parmeter_m,parmeter_v FROM config_quotes_class
                     WHERE car_level= (SELECT car_level FROM config_che300_major_info WHERE model_id =",car_id,")
                     AND auto= (SELECT auto FROM config_che300_major_info WHERE model_id =",car_id,")",sep='')
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  config_reg<-dbFetch(dbSendQuery(loc_channel,sql_config),-1)
  if(nrow(config_reg)>0){
    config_reg<-config_reg
  }else{
    config_reg<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT platform_class car_platform_class,parmeter_y,parmeter_m,parmeter_v FROM config_quotes_class
                                                       WHERE car_level= '全级别'
                                                       AND auto= (SELECT auto FROM config_che300_major_info WHERE model_id =",car_id,")",sep='')),-1)
  }
  dbDisconnect(loc_channel)
  select_year<-round((as.Date(result_tag$select_partition_month)-as.Date(result_tag$select_regDate))/365,2)%>%as.character()%>%as.numeric()
  select_mile<-result_tag$select_mile
  config_reg<-config_reg%>%dplyr::mutate(ss=parmeter_v+select_year*parmeter_y+select_mile*parmeter_m)%>%dcast(.~car_platform_class)%>%.[,-1]
  names(config_reg)[names(config_reg) == "fb"] = c("fb_index")
  names(config_reg)[names(config_reg) == "pm"] = c("pm_index")
  result_tag<-data.frame(result_tag,config_reg)
  

  ################第三部分：对标车型价格预测###############
  ########3.当样本量过少启动(对标车型误差)############
  #acount_sample<-sum(output_sample$sample_size[which(output_sample$car_platform %in% c("czb","csp"))])
  if(nrow(car_match)>0){
    select_input_db<-data.frame(user_query_id=select_input$user_query_id,select_model_id=car_match$model_id,select_input[,-c(1:2)])
    match_output_pre<-NULL
    for (i in 1:nrow(select_input_db)) {
      linshi<-fun_pred(select_input_db[i,])
      linshi1<-linshi[[1]]%>%dplyr::mutate(query_lab="F")
      match_output_pre<-rbind(match_output_pre,linshi1)
    }
  }else{match_output_pre<-NULL}
  ##输出处理（对标车型）
  match_output_pre<-rbind(output_pre,match_output_pre)
  result_compare<-dplyr::summarise(group_by(match_output_pre,user_query_id,query_lab,select_model_id,select_model_name,select_model_price,select_regDate,
                                     select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm))%>%
    ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))%>%
    cbind(result_tag[,c("fb_index","pm_index")])
  select_input_id<-match_output_pre%>%dplyr::filter(select_model_id!=select_input$select_model_id)%>%
    group_by(select_model_id)%>%dplyr::summarise(sum_size=mean(fb_n+pm_n))%>%
    top_n(3,sum_size)%>%dplyr::filter(sum_size>2000)%>%dplyr::select(select_model_id)%>%as.data.frame()
  result_compare<-result_compare<-merge(result_compare,rbind(select_input_id,data.frame(select_model_id=car_id)))
  result_compare$fb_index<-cut(abs(result_compare$fb_monitor-result_compare$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_compare$pm_index<-cut(abs(result_compare$pm_monitor-result_compare$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_compare<-result_compare%>%dplyr::mutate(recep_price=pm_price*0.94-0.25)
  ##输出处理（标的车型）
  result_bd<-dplyr::summarise(group_by(output_pre,user_query_id,select_model_name,select_model_price,select_regDate,
                                     select_partition_month,select_mile,province),fb_price=mean(fb),pm_price=mean(pm))%>%
    ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))%>%
    cbind(result_tag[,c("fb_index","pm_index")])
  result_bd$fb_index<-cut(abs(result_bd$fb_monitor-result_bd$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_bd$pm_index<-cut(abs(result_bd$pm_monitor-result_bd$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_bd<-result_bd%>%dplyr::mutate(recep_price=pm_price*0.94-0.25)

  #######********输出未来五个月预测值********#######
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
    linshi<-fun_pred(select_input_future[i,])
    linshi1<-linshi[[1]]
    match_output_future<-rbind(match_output_future,linshi1)
  }
  result_future<-dplyr::summarise(group_by(match_output_future,user_query_id,select_partition_month),fb_price=round(mean(fb),2))%>%
    ungroup()%>%as.data.frame()%>%dcast(user_query_id~select_partition_month)
  names(result_future)<-c("user_query_id","now_price","future1","future2","future3","future4","future5")
  #######********输出未来五个月预测值********#######
  
  #存入数据库
  write.csv(result_bd,paste0(price_model_loc,"/output/result/yck_project_model_result_tag",".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  write.csv(result_compare,paste0(price_model_loc,"/output/result/yck_project_model_result_match",".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  write.csv(result_future,paste0(price_model_loc,"/output/result/yck_project_model_result_future",".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)

  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_result_match",".csv"),"'",
                                 " INTO TABLE yck_project_model_result_match CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_result_tag",".csv"),"'",
                                 " INTO TABLE yck_project_model_result_tag CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_result_future",".csv"),"'",
                                 " INTO TABLE yck_project_model_result_future CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  dbSendQuery(loc_channel,paste0("UPDATE yck_project_model_query SET query_statue=2 WHERE user_query_id=",unique(result_compare$user_query_id),";"))
  dbDisconnect(loc_channel)
  return(1)
}