main_fun_main_union<-function(select_input){
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
  output_pre<-linshi[[1]]
  result_tag<-dplyr::summarise(group_by(output_pre,select_model_id,select_model_name,select_model_price,select_regDate,
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
  if(nrow(car_match)>0){
    select_input_db<-data.frame(select_model_id=car_match$model_id,select_input[,-1])
    start_time<-Sys.time()
    pb <- tkProgressBar("进度","已完成 %", 0, 100) #开启进度条 
    u<-1:nrow(select_input_db)
    match_output_pre<-NULL
    for (i in 1:nrow(select_input_db)) {
      linshi<-fun_pred(select_input_db[i,])
      linshi1<-linshi[[1]]
      match_output_pre<-rbind(match_output_pre,linshi1)
      info<- sprintf("已完成 %d%%", round(i*100/length(u)))   #进度条
      setTkProgressBar(pb, i*100/length(u),sprintf("进度 (%s)  耗时:(%ss)",info,round(unclass(as.POSIXct(Sys.time()))-unclass(as.POSIXct(start_time)))),info)  #进度条
    }
    close(pb)  #关闭进度条
  }else{match_output_pre<-NULL}
  match_output_pre<-rbind(output_pre,match_output_pre)
  result_compare<-dplyr::summarise(group_by(match_output_pre,select_model_id,select_model_name,select_model_price,select_regDate,
                                     select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm))%>%
    ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))%>%
    cbind(result_tag[,c("fb_index","pm_index")])
  select_input_id<-match_output_pre%>%dplyr::filter(select_model_id!=select_input$select_model_id)%>%
    group_by(select_model_id)%>%dplyr::summarise(sum_size=mean(fb_n+pm_n))%>%
    top_n(3,sum_size)%>%dplyr::filter(sum_size>2000)%>%dplyr::select(select_model_id)%>%as.data.frame()
  result_compare<-merge(result_compare,rbind(select_input_id,data.frame(select_model_id=car_id)))
  result_compare$fb_index<-cut(abs(result_compare$fb_monitor-result_compare$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_compare$pm_index<-cut(abs(result_compare$pm_monitor-result_compare$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_compare<-result_compare%>%dplyr::mutate(recep_price=pm_price*0.94-0.25)

  
  
  ################第四部分：输出车型的相关信息###############
  result_relation<-main_fun_relation_output(result_compare$select_model_id)
  result_relation_pz<-result_relation$config_pz
  result_relation_xl<-result_relation$config_xl
  result_relation_zk<-result_relation$config_zk
  result_relation_zk_new<-result_relation$config_zk_new
  
  write.xlsx(result_compare,paste0(price_model_loc,"/output/result/",result_tag$select_model_name,'-',format(as.Date(select_input$select_regDate),"%Y%m"),".xlsx"),sheetName = "价格",row.names = F)
  write.xlsx(result_relation_pz,paste0(price_model_loc,"/output/result/",result_tag$select_model_name,'-',format(as.Date(select_input$select_regDate),"%Y%m"),".xlsx"),sheetName = "配置",row.names = F,append = T)
  write.xlsx(result_relation_xl,paste0(price_model_loc,"/output/result/",result_tag$select_model_name,'-',format(as.Date(select_input$select_regDate),"%Y%m"),".xlsx"),sheetName = "销量",row.names = F,append = T)
  write.xlsx(result_relation_zk,paste0(price_model_loc,"/output/result/",result_tag$select_model_name,'-',format(as.Date(select_input$select_regDate),"%Y%m"),".xlsx"),sheetName = "折扣",row.names = F,append = T)
  write.xlsx(result_relation_zk_new,paste0(price_model_loc,"/output/result/",result_tag$select_model_name,'-',format(as.Date(select_input$select_regDate),"%Y%m"),".xlsx"),sheetName = "最新款型折扣",row.names = F,append = T)
}