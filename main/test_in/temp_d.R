#test
select_input<-select_input[which(select_input$select_model_id==1129347),]
select_input<-select_input[79:100,]

loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id 
                                                     FROM yck_project_model_query")),-1)%>% unique()
dbDisconnect(loc_channel)

select_input<-23059
select_partition_month<-as.Date('2015-01-01')+seq(0,3999,31)
select_input<-data.frame(select_model_id=rep(select_input,each=130*130),
                         select_regDate=rep(select_partition_month,each=130),select_mile=1,
                         select_partition_month=rep(select_partition_month,130)) %>% dplyr::filter(select_partition_month-select_regDate>180)
select_input$select_mile<-2*as.numeric(round(difftime(as_datetime(select_input$select_partition_month),as_datetime(select_input$select_regDate),units="days")/365,2))


select_input_org<-select_input
result_tag<-apply(array(1:nrow(select_input_org)),1,fun_pred_round)
result_tag<-list.rbind(list.rbind(result_tag))

result_tag1<-summarise(group_by(result_tag,select_series,select_model_year,select_model_name,select_model_price,select_auto,select_car_level,
                                select_model_id,select_regDate,select_mile,select_partition_month),fb_price=mean(fb),pm_price=mean(pm),
                       fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
  ungroup()%>%as.data.frame()%>%mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3)) %>% dplyr::mutate(type='test')



result_tag1<-result_tag %>% dplyr::select(select_series,select_model_year,select_model_name,select_model_price,select_auto,select_car_level,
                                          select_model_id,select_regDate,select_mile,select_partition_month,province,
                                          fb,pm,r_cor,r_fb,r_pm,fb_n,pm_n,fb_index,pm_index) %>% dplyr::mutate(type='test')
write.csv(result_tag1,paste0(price_model_loc,"/output/result/result_tag1",".csv"),
          row.names = F,fileEncoding = "UTF-8",quote = F)
loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/result_tag1",".csv"),"'",
                               " INTO TABLE ttemp_model_test_cc_copy CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
dbDisconnect(loc_channel)








#test
select_input<-select_input[which(select_input$select_model_id==1129347),]
select_input<-select_input[79:100,]

loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id 
                                                     FROM yck_project_model_query")),-1)%>% unique()
dbDisconnect(loc_channel)
select_partition_month<-as.Date('2017-06-01')+seq(30,3000,30)
select_input<-data.frame(select_model_id=rep(select_input$select_model_id,each=100),
                         select_regDate='2017-01-01',select_mile=1,
                         select_partition_month=select_partition_month)
select_input$select_mile<-2*as.numeric(round(difftime(as_datetime(select_input$select_partition_month),as_datetime(select_input$select_regDate),units="days")/365,2))

select_input_org<-select_input
result_tag<-apply(array(1:nrow(select_input_org)),1,fun_pred_round)
result_tag<-list.rbind(list.rbind(result_tag))

result_tag1<-summarise(group_by(result_tag,select_series,select_model_year,select_model_name,select_model_price,select_auto,select_car_level,
                                select_model_id,select_regDate,select_mile,select_partition_month),fb_price=mean(fb),pm_price=mean(pm),
                       fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
  ungroup()%>%as.data.frame()%>%mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3)) %>% dplyr::mutate(type='test')



result_tag1<-result_tag %>% dplyr::select(select_series,select_model_year,select_model_name,select_model_price,select_auto,select_car_level,
                                          select_model_id,select_regDate,select_mile,select_partition_month,province,
                                          fb,pm,r_cor,r_fb,r_pm,fb_n,pm_n,fb_index,pm_index) %>% dplyr::mutate(type='test')
write.csv(result_tag1,paste0(price_model_loc,"/output/result/result_tag1",".csv"),
          row.names = F,fileEncoding = "UTF-8",quote = F)
loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/result_tag1",".csv"),"'",
                               " INTO TABLE ttemp_model_test_cc CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
dbDisconnect(loc_channel)