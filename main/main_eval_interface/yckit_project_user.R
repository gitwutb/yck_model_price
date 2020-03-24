####*************************step0:获取IT系统项目数据
fun_yckit_query_project<-function(belong_project){
  yck_it_query_project<-fun_mysqlload_query(sqllogin_salarable_yck,paste("SELECT a.id query_project,a.project_name,b.vin,b.id yck_query_id,b.config_id,b.kilometre,DATE_FORMAT(FROM_UNIXTIME(b.license_reg_date),'%Y-%m-%d') license_reg_date,c.autohome_id FROM yck_quote_project a
                                                                 INNER JOIN yck_quote_project_cars b ON a.id=b.quote_project_id
                                                                 INNER JOIN yck_car_basic_config c ON b.config_id=c.id WHERE a.id=",belong_project,";"))
  fun_mysqlload_query(sqllogin_salarable_yck,paste0("UPDATE yck_quote_project SET data_quote_status=2 WHERE id=",belong_project))
  for (i in 1:dim(yck_it_query_project)[2]) {
    yck_it_query_project[,i][which(is.na(yck_it_query_project[,i]))]<-"\\N"
  }
  fun_mysqlload_query(local_defin,paste0("DELETE FROM yck_it_query_project WHERE query_project=",belong_project))
  fun_mysqlload_add(price_model_loc,local_defin,yck_it_query_project,"yck_it_query_project",belong_project)
}

####*************************step1:IT匹配数据
fun_yckdc_query_config_id<-function(belong_project){
  ###查看config_plat_id_match的id匹配是否有问题，矫正id匹配再重新运行代码
  new_project_1<-fun_mysqlload_query(local_defin,paste0("SELECT a.autohome_id,c.type_name model_name_a,c.recommend_price price_auto
                                                        FROM (SELECT DISTINCT autohome_id FROM yck_it_query_project WHERE query_project=",belong_project,") a
                                                        LEFT JOIN yck_it_query_config_id b ON a.autohome_id=b.id_autohome
                                                        INNER JOIN ods_erp.ods_car_basic_config c ON a.autohome_id=c.autohome_id
                                                        WHERE id_che300 IS NULL;"))
  if(length(new_project_1$autohome_id)>0){id_autohome_p<-paste0(new_project_1$autohome_id,collapse = ',')}else{id_autohome_p<-0}
  new_project_2<-fun_mysqlload_query(local_defin,paste0("SELECT a.id_autohome autohome_id,a.id_che300,b.model_price price300,b.model_name FROM config_plat_id_match a
                                                        INNER JOIN config_che300_major_info b ON a.id_che300=b.model_id 
                                                        WHERE is_only_autohome=1 AND id_autohome in(",id_autohome_p,")"))
  #第一部分
  new_project_1$price_auto<-as.numeric(new_project_1$price_auto)
  new_project<-dplyr::left_join(new_project_1,new_project_2,by=c("autohome_id"="autohome_id")) %>% dplyr::mutate(is_fee=as.numeric(price_auto)-as.numeric(price300))
  temp_id<-new_project %>% dplyr::select(autohome_id,id_che300,price_auto,price300,is_fee) %>% unique() %>%
    dplyr::group_by(autohome_id) %>% dplyr::mutate(is_count=n()) %>% as.data.frame()
  yck_it_query_config_id<-new_project %>% dplyr::filter(autohome_id %in% temp_id$autohome_id[which(temp_id$is_fee==0 & temp_id$is_count==1)]) %>%
    dplyr::select(id_autohome=autohome_id,id_che300=id_che300)
  temp_id<-temp_id %>% dplyr::filter(is_fee!=0||is_count!=1||is.na(is_fee))
  #第二类为未匹配上的车（需要人为干预）
  not_it_query_config_id<-new_project %>% dplyr::filter(autohome_id %in% setdiff(temp_id$autohome_id,c(33369,1002856,1000525,100646,101472)))
  if(nrow(not_it_query_config_id)>=1){fun_mailsend("YCK系统车源估值专属-ID匹配",paste0(belong_project,'号项目-匹配失败数量为：',nrow(not_it_query_config_id)))}
  fun_mysqlload_add(price_model_loc,local_defin,yck_it_query_config_id,'yck_it_query_config_id',0)
  return(not_it_query_config_id$autohome_id)
}

####*************************现车估值接口：获取IT系统项目数据
fun_yckit_query_model<-function(belong_project){
  fun_yckit_query_project(belong_project)
  #re_fun_yckdc_query_config_id<-fun_yckdc_query_config_id(belong_project)
  #判断是否有车型匹配失败，需要邮件通知人工维护
  charSql_query_yckit<-paste0("SELECT a.autohome_id,c.type_name model_name_a,c.recommend_price price_auto
                                                        FROM (SELECT DISTINCT autohome_id FROM yck_it_query_project WHERE query_project=",belong_project,") a
                                                        LEFT JOIN yck_it_query_config_id b ON a.autohome_id=b.id_autohome
                                                        INNER JOIN ods_erp.ods_car_basic_config c ON a.autohome_id=c.autohome_id
                                                        WHERE id_che300 IS NULL;")
  addFun_yckit_query_configid(price_model_loc,local_defin,charSql_query_yckit)
  select_input_pre<-fun_mysqlload_query(local_defin,paste0("SELECT b.id_che300,a.kilometre,a.license_reg_date,a.query_project,a.yck_query_id
                                                           FROM yck_it_query_project a
                                                           LEFT JOIN yck_it_query_config_id b ON a.autohome_id=b.id_autohome
                                                           WHERE a.query_project=",belong_project,";"))
  #判定有多少查询ID没有匹配，并输出到备注信息
  #返回字典return_post(0为项目无数据;1为处理后项目无数据;2为项目报价成功;3项目报价失败)
  select_input_pre<-select_input_pre %>% dplyr::filter(!is.na(id_che300)) %>% dplyr::mutate(select_partition_month=as.character(Sys.Date()+30)) %>%
    dplyr::select(select_model_id=id_che300,select_regDate=license_reg_date,select_mile=kilometre,select_partition_month,query_project,yck_query_id)
  select_input<-select_input_pre %>% dplyr::filter(select_regDate>='1990-01-01')
  select_input$select_mile[which(is.na(select_input$select_mile))]<-
    40000*as.numeric(difftime(as_datetime(select_input$select_partition_month[which(is.na(select_input$select_mile))]),as_datetime(select_input$select_regDate[which(is.na(select_input$select_mile))]),units="days")/365)
  select_input$select_mile<-round(as.numeric(select_input$select_mile)/10000,2)
  yck_it_query_mresult<-data.frame()
  for (i in 1:nrow(select_input)) {
    linshi<-tryCatch({fun_pred(select_input[i,])},
                     error=function(e){0},
                     finally={0})
    if(class(linshi)=='data.frame'){
      yck_it_query_mresult<-rbind(yck_it_query_mresult,linshi)
    }
  }
  if(nrow(yck_it_query_mresult)>0){
    yck_it_query_mresult<-dplyr::summarise(group_by(yck_it_query_mresult,yck_query_id,select_model_id,select_model_name,select_model_price,select_regDate,
                                                    select_partition_month,select_mile),fb_price=mean(fb),pm_price=round(mean(pm),2),
                                           fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
      ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3),add_time=as.character(Sys.time()))
    yck_it_query_mresult$fb_index<-cut(abs(yck_it_query_mresult$fb_monitor-yck_it_query_mresult$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
    yck_it_query_mresult$pm_index<-cut(abs(yck_it_query_mresult$pm_monitor-yck_it_query_mresult$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
    fun_mysqlload_add(price_model_loc,local_defin,yck_it_query_mresult,'yck_it_query_mresult',belong_project)
    #输出到IT估值表
    c_quotes_number<-fun_mysqlload_query(local_defin,paste0("SELECT a.yck_query_id,COUNT(*) quotes_number FROM yck_it_query_mresult a
                                                            INNER JOIN yck_it_query_project b ON a.yck_query_id=b.yck_query_id
                                                            WHERE b.query_project=",belong_project," GROUP BY a.yck_query_id;"))
    yck_it_query_mresult<-dplyr::left_join(yck_it_query_mresult,c_quotes_number,by='yck_query_id')
    yck_quote_project_log<-data.frame(id='\\N',quotes_number=yck_it_query_mresult$quotes_number,project_car_id=yck_it_query_mresult$yck_query_id,old_price=0,new_price=10000*yck_it_query_mresult$pm_price,
                                      quotes_type=2,quote_type_desc=c('数据批售价'),quoter_name=c('数据中心'),
                                      quote_time=unclass(as.POSIXct(Sys.time())),remark='\\N')
    fun_mysqlload_add(price_model_loc,sqllogin_salarable_yck,yck_quote_project_log,'yck_quote_project_log',belong_project)
    fun_mysqlload_query(sqllogin_salarable_yck,paste0("UPDATE yck_quote_project SET data_quote_status=1 WHERE id=",belong_project))
    return_post=0
    if(return_post!=0){fun_mailsend("YCK系统车源估值专属-现车估值",paste0(belong_project,'号项估值失败'))}
  }
}

####*************************现车估值补充：人工处理完匹配之后调用接口
fun_yckit_query_model_replenish<-function(){
  select_input_pre<-fun_mysqlload_query(local_defin,"SELECT c.id_che300,a.kilometre,a.license_reg_date,a.query_project,a.yck_query_id from yck_it_query_project a 
                                        LEFT JOIN yck_it_query_mresult b ON a.yck_query_id=b.yck_query_id 
                                        LEFT JOIN yck_it_query_config_id c ON a.autohome_id=c.id_autohome
                                        WHERE b.yck_query_id IS NULL AND c.id_che300 IS NOT NULL AND a.license_reg_date IS NOT NULL;")
  
  #判定有多少查询ID没有匹配，并输出到备注信息
  #返回字典return_post(0为项目无数据;1为处理后项目无数据;2为项目报价成功;3项目报价失败)
  select_input_pre<-select_input_pre %>% dplyr::filter(!is.na(id_che300)) %>% dplyr::mutate(select_partition_month=as.character(Sys.Date()+30)) %>%
    dplyr::select(select_model_id=id_che300,select_regDate=license_reg_date,select_mile=kilometre,select_partition_month,query_project,yck_query_id)
  select_input<-select_input_pre %>% dplyr::filter(select_regDate>='1990-01-01')
  select_input$select_mile[which(is.na(select_input$select_mile))]<-
    40000*as.numeric(difftime(as_datetime(select_input$select_partition_month[which(is.na(select_input$select_mile))]),as_datetime(select_input$select_regDate[which(is.na(select_input$select_mile))]),units="days")/365)
  select_input$select_mile<-round(as.numeric(select_input$select_mile)/10000,2)
  yck_it_query_mresult<-data.frame()
  for (i in 1:nrow(select_input)) {
    linshi<-tryCatch({fun_pred(select_input[i,])},
                     error=function(e){0},
                     finally={0})
    if(class(linshi)=='data.frame'){
      yck_it_query_mresult<-rbind(yck_it_query_mresult,linshi)
    }
  }
  if(nrow(yck_it_query_mresult)>0){
    yck_it_query_mresult<-dplyr::summarise(group_by(yck_it_query_mresult,yck_query_id,select_model_id,select_model_name,select_model_price,select_regDate,
                                                    select_partition_month,select_mile),fb_price=mean(fb),pm_price=round(mean(pm),2),
                                           fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
      ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3),add_time=as.character(Sys.time()))
    yck_it_query_mresult$fb_index<-cut(abs(yck_it_query_mresult$fb_monitor-yck_it_query_mresult$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
    yck_it_query_mresult$pm_index<-cut(abs(yck_it_query_mresult$pm_monitor-yck_it_query_mresult$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
    fun_mysqlload_add(price_model_loc,local_defin,yck_it_query_mresult,'yck_it_query_mresult',0)
    #输出到IT估值表
    yck_quote_project_log<-data.frame(id='\\N',quotes_number=1,project_car_id=yck_it_query_mresult$yck_query_id,old_price=0,new_price=10000*yck_it_query_mresult$pm_price,
                                      quotes_type=2,quote_type_desc=c('数据批售价'),quoter_name=c('数据中心'),
                                      quote_time=unclass(as.POSIXct(Sys.time())),remark='\\N')
    fun_mysqlload_add(price_model_loc,sqllogin_salarable_yck,yck_quote_project_log,'yck_quote_project_log',0)
    return_post=0
    if(return_post!=0){fun_mailsend("YCK系统车源估值专属-现车估值",paste0(0,'号项估值失败'))}
  }
}