#数鼎估值接口数据输出(每周一三五早上8点触发)
evalCompare_fun_sdInterface<-function(input_path,input_ipOut){
  #yck库取数
  sql1_series<-paste0("SELECT a.plate_number,a.vin,a.accurate_detection_level,a.warehouse_city,a.warehouse_province,a.license_reg_date,a.kilometre,a.model_name,a.series_name,a.color,a.inspection_valid_date,
                      a.belong_project_name,a.driving_license_status,a.register_license_status,a.insurance_status,b.first_in_time,DATEDIFF(NOW(),b.first_in_time) d_day,a.usage_method,c.body_structure,c.fuel,c.weight 
                      FROM erp_dw.dwd_cars a 
                      INNER JOIN erp_dw.dwd_allocate b ON a.car_id=b.car_id
                      INNER JOIN ods_erp.ods_car_basic_config c ON a.config_id=c.id
                      WHERE a.is_sold=0 AND a.operate_status=4 AND a.accurate_detection_level IN('A','B1','B2','C1','C2','C3') AND a.kilometre<=300000")
  #a.usage使用性质(0:非营运;1:营运)   driving_license_status行驶证状态（0未选择1有2无3外借）
  #insurance_status保单状态（可选值：0：未申请，1：已申请，2：总部，3：分公司，4：已寄出）
  #register_license_status登记证状态（可选值：0：未申请，1：已申请，2：总部，3：分公司，4：上游已寄未收到，5：抵押方已寄未收到，6：已寄回上游）
  #数据整合
  result_out_fin<-fun_mysqlload_query(input_ipOut,sql1_series) %>% dplyr::filter(usage_method==0&license_reg_date<(Sys.Date()-180))
  result_out_fin$usage_method<-ifelse(result_out_fin$usage_method==0,'非营运','营运')
  result_out_fin$driving_license_status<-ifelse(result_out_fin$driving_license_status==1,'有',ifelse(result_out_fin$driving_license_status==2,'无',ifelse(result_out_fin$driving_license_status==3,'外借','')))
  result_out_fin$insurance_status<-ifelse(result_out_fin$insurance_status==1,'已申请',ifelse(result_out_fin$insurance_status==0,'未申请',ifelse(result_out_fin$insurance_status==2,'总部','')))
  result_out_fin$register_license_status<-ifelse(result_out_fin$register_license_status==1,'已申请','未申请')
  for (i in 1:dim(result_out_fin)[2]) {
    result_out_fin[,i][which(is.na(result_out_fin[,i]))]<-"\\N"
  }
  names(result_out_fin)<-c('车牌','VIN','评级','城市','省份','上牌时间','里程','车型','车系','颜色','年检时间','项目名','行驶证',
                           '登记证状态','保单状态','首次入库时间','库存周期','使用性质','车身形式','燃油类型','整备质量')
  write.csv(result_out_fin,paste0(input_path,"/",'数鼎估值车源数据',".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
}

#数鼎估值对比跟踪
evalCompare_fun_yAs<-function(input_path,input_ipOut){
  select_input<-fun_mysqlload_query(input_ipOut,"SELECT e.id_che300 select_model_id,b.license_reg_date select_regDate,b.kilometre/10000 select_mile,m.eval_date select_partition_month,m.pm_price s_pm_price,m.b_pm_price,m.min_price,m.max_price,c.deal_price,m.car_id from 
                                    (SELECT sd.car_id,sd.eval_date,sd.pm_price,sd.b_pm_price,sd.min_price,sd.max_price  from (select sdtoyck_cars_eval.*,case when car_id = @last_car_id then @rank:=@rank+1 else @rank:=1 end as rank,@last_car_id:=car_id from `yck-data-center`.sdtoyck_cars_eval,(select @last_car_id:= '' , @rank:=0) T  
                                    order by car_id,eval_date desc) sd where sd.rank=1) m  
                                    INNER JOIN `erp_dw`.dwd_cars b on m.car_id=b.car_id 
                                    INNER JOIN `erp_dw`.dwd_car_order_cars c on b.car_id=c.car_id 
                                    INNER JOIN `ods_erp`.ods_car_basic_config d on b.config_id=d.id
                                    INNER JOIN `yckdc_da2_ucar`.config_plat_id_match e on d.autohome_id=e.id_autohome 
                                    LEFT JOIN yckdc_da2_ucar.evalmonitor_yckdc_sdcompare f ON b.car_id=f.car_id 
                                    where e.id_autohome!=0 AND f.car_id IS NULL")
  yAs_eval_df<-NULL
  for (i in 1:nrow(select_input)) {
    tm_yAs_eval_df<-fun_pred(select_input[i,])
    yAs_eval_df<-rbind(yAs_eval_df,tm_yAs_eval_df)
  }
  yAs_eval_rdf<-yAs_eval_df %>% dplyr::group_by(car_id,select_model_id,select_series,select_model_name,select_model_price,select_regDate,select_mile,select_partition_month) %>% 
    dplyr::summarise(yck_fb=mean(fb),yck_pm=mean(pm),sd_pm=round(mean(s_pm_price)/10000,2),sd_fb=round(mean(b_pm_price)/10000,2),
                     deal_price=mean(deal_price)/10000)%>% ungroup()%>% as.data.frame() %>% dplyr::mutate(eval_date=as.character(Sys.Date()))
  fun_mysqlload_add(input_path,input_ipOut,yAs_eval_rdf,'evalmonitor_yckdc_sdcompare',0)
}


##优车库估值分析：YCK车估值（增量）(每5分钟更新一次)
evalMonitor_yckit_selling<-function(input_path,input_ipOut,input_sqlstr){
  select_car_id<-fun_mysqlload_query(input_ipOut,input_sqlstr)
  select_input<-fun_mysqlload_query(input_ipOut,paste0("SELECT a.car_id,d.id_che300,a.license_reg_date,a.kilometre/10000 select_mile,
                                    IF(a.is_sold=0,DATE_FORMAT(NOW(),'%Y-%m-%d'),b.sell_time) select_partition_month,a.province,a.is_sold FROM erp_dw.dwd_cars a 
                                    LEFT JOIN erp_dw.dwd_car_order_cars b ON a.car_id=b.car_id                                     
                                    INNER JOIN ods_erp.ods_car_basic_config c ON a.config_id=c.id
                                    INNER JOIN yckdc_da2_ucar.yck_it_query_config_id d ON c.autohome_id=d.id_autohome 
                                    WHERE a.car_id in(",paste0(select_car_id$car_id,collapse = ','),")")) %>% 
    dplyr::filter(license_reg_date>='1990-01-01'&select_mile>=0&(as.Date(select_partition_month)-as.Date(license_reg_date)>100)) %>% 
    dplyr::select(select_model_id=id_che300,select_regDate=license_reg_date,select_mile,select_partition_month,car_id,is_sold,bd_province=province)
  if(nrow(select_input)>0){
    return_df<-NULL
    for (i in 1:nrow(select_input)) {
      tm_return_df<-fun_pred(select_input[i,])
      return_df<-rbind(return_df,tm_return_df)
    }
    return_df_all<-return_df %>% dplyr::group_by(car_id,select_model_id,select_series,select_model_name,select_model_price,select_regDate,select_mile,select_partition_month,is_sold) %>% 
      dplyr::summarise(yck_fb=round(mean(fb),2),yck_pm=round(mean(pm),2))%>%ungroup()%>%as.data.frame()
    return_df_bdp<-return_df %>% dplyr::filter(bd_province==province) %>% dplyr::select(car_id,bd_province,yck_pm_p=pm)
    return_df_all<-left_join(return_df_all,return_df_bdp,by='car_id') %>% dplyr::mutate(eval_date=as.character(Sys.Date()))
    return_df_all$yck_pm_p[is.na(return_df_all$yck_pm_p)]<-0
    fun_mysqlload_add_upd(input_path,input_ipOut,return_df_all,'evalmonitor_yckit_selling',0)
    #存到IT-link数据库
    return_df_all_it<-return_df_all %>% dplyr::select(car_id,select_model_price,select_regDate,select_mile,select_partition_month,yck_fb,yck_pm,is_sold,eval_date)
    fun_mysqlload_add_upd(input_path,fun_mysql_config_up('yck'),return_df_all_it,'link.evalmonitor_yckit_detection',0)
    fun_mysqlload_add_upd(input_path,fun_mysql_config_up('yckt'),return_df_all_it,'link.evalmonitor_yckit_detection',0)
  }
}