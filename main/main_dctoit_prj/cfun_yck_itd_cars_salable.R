#车务-违章查询数据处理
fun_query1_violation_license<-function(query1_violation_license){
  count_json_degree<-function(input_cjson){
    json_tran_df<-as.data.frame(rlist::list.rbind(RJSONIO::fromJSON(input_cjson)))
    if(names(json_tran_df)%in%c('Degree')){
      json_tran_df<-json_tran_df %>% dplyr::select(Degree)}else{
        json_tran_df<-data.frame(Degree=0)
      }
    return(data.frame(sum_degree=sum(as.numeric(json_tran_df$Degree)),max_degree=max(as.numeric(json_tran_df$Degree))))
  }
  query1_violation_license$violation_info2[-grep("\\[\\{",query1_violation_license$violation_info2)]<-NA
  sum_deg<-query1_violation_license %>% dplyr::filter(!is.na(violation_info2)) %>% dplyr::select(car_id,violation_info2)
  sum_deg$violation_info2<-gsub(".*\\[\\{",'[{',sum_deg$violation_info2)
  sum_deg$violation_info2<-gsub("\\}\\].*",'}]',sum_deg$violation_info2)
  sum_deg_cha<-sum_deg %>% dplyr::select(violation_info2)
  if(length(sum_deg_cha$violation_info2)!=0){
    sum_deg_result<-apply(sum_deg_cha,1, count_json_degree) %>% rlist::list.rbind() %>% dplyr::mutate(car_id=sum_deg$car_id)
    query1_violation_license<-left_join(query1_violation_license,sum_deg_result,by='car_id') %>% dplyr::select(-violation_info2)
  }else{query1_violation_license<-data.frame(query1_violation_license,sum_degree=NA,max_degree=NA) %>% dplyr::select(-violation_info2)}
  query1_violation_license$sum_degree[which(is.na(query1_violation_license$sum_degree))]<-0
  query1_violation_license$max_degree[which(is.na(query1_violation_license$max_degree))]<-0
  return(query1_violation_license=query1_violation_license)
}

#####--可售/不可售原因分析--####
dealFun_cars_salable_reason<-function(all_yck_cars){
  for (i in 1:dim(all_yck_cars)[2]) {
    all_yck_cars[,i][which(is.na(all_yck_cars[,i]))]<-""
  }
  all_yck_cars$sum_degree[which(all_yck_cars$sum_degree=='')]<-0
  all_yck_cars$max_degree[which(all_yck_cars$max_degree=='')]<-0
  all_yck_cars$is_application[which(all_yck_cars$is_application=='')]<-0
  all_yck_cars$inspection_valid_date<-ifelse(all_yck_cars$inspection_valid_date%in%c('','0000-00-00'),'2000-01-01',all_yck_cars$inspection_valid_date)
  all_yck_cars$inspection_valid_date<-as.numeric(difftime(lubridate::as_datetime(all_yck_cars$inspection_valid_date),lubridate::as_datetime(Sys.Date()),units="days"))
  all_yck_cars$inspection_valid_date<-ifelse(is.na(all_yck_cars$inspection_valid_date),0,all_yck_cars$inspection_valid_date)
  ###第四部分：1不可售is_salable计算部分(0可售；1不可售；2预售；3已售；4未开卖)
  all_yck_cars<-all_yck_cars %>% dplyr::mutate(is_salable=ifelse(is_over=='2'|(as.numeric(sum_degree)>=33|as.numeric(max_degree)==12)|is_stop==1|(auction_time_id!='0' & auction_time_id!='')|
                                                                   pm_transfer==1|(project_type==1&mortgage_status %in% c(0,1,4))|is_application==1|(project_type!=1&is_confirm!='1'),1,
                                                                 ifelse((project_type!=1&mortgage_status %in% c(0,1,4))|violation_status%in%c(2,4,'')|
                                                                          driving_license_status!=1|is_confirm!='1'|as.numeric(inspection_valid_date)<=10,2,0)))
  all_yck_cars$is_salable[which(all_yck_cars$is_sold==1)]<-3
  all_yck_cars$is_salable[which(all_yck_cars$is_sold==0&!all_yck_cars$operate_status%in%c(3,4))]<-4
  
  ##第四部分：2不可售is_salable描述分析部分
  ##不可售/预售原因剖析
  all_yck_cars$is_over<-ifelse(all_yck_cars$is_over=='2',"超证;","")
  all_yck_cars$sum_degree<-ifelse(as.numeric(all_yck_cars$sum_degree)>=33|as.numeric(all_yck_cars$max_degree)==12,"超分;","")
  all_yck_cars$auction_time_id<-ifelse(all_yck_cars$auction_time_id!='0' & all_yck_cars$auction_time_id!='',"拍卖;","")
  all_yck_cars$is_stop<-ifelse(all_yck_cars$is_stop=='1',"停售;","")
  all_yck_cars$pm_transfer<-ifelse(all_yck_cars$pm_transfer=='1',"转籍中;","")
  all_yck_cars$project_type<-ifelse(all_yck_cars$project_type==1,"增量项目","定量项目")
  all_yck_cars$project_type<-ifelse(all_yck_cars$mortgage_status %in% c(0,1,4)|all_yck_cars$is_confirm!='1',all_yck_cars$project_typ,"")
  all_yck_cars$mortgage_status<-ifelse(all_yck_cars$mortgage_status %in% c(0,1,4),"抵押待处理;","")
  all_yck_cars$is_confirm<-ifelse(all_yck_cars$is_confirm!='1',"未精检;","")
  all_yck_cars$is_application<-ifelse(all_yck_cars$is_application=='1',"申请单进行中;","")
  all_yck_cars$violation_status<-ifelse(all_yck_cars$violation_status%in%c(2),"有违章;",ifelse(all_yck_cars$violation_status%in%c(4,''),"违章待确认;",""))
  all_yck_cars$driving_license_status<-ifelse(all_yck_cars$driving_license_status!=1,"缺行驶证;","")
  all_yck_cars$inspection_valid_date<-ifelse(as.numeric(all_yck_cars$inspection_valid_date)<10,"年审到期;","")
  all_yck_cars<-all_yck_cars %>% dplyr::select(car_id,is_salable,is_over,sum_degree,auction_time_id,is_stop,pm_transfer,project_type,mortgage_status,
                                               is_confirm,is_application,violation_status,driving_license_status,inspection_valid_date,operate_status) %>% 
    dplyr::mutate(other_reason=paste0(is_over,sum_degree,auction_time_id,is_stop,pm_transfer,project_type,mortgage_status,is_confirm,is_application,
                                      violation_status,driving_license_status,inspection_valid_date))
  
  ##第五部分：数据写入IT的link库，并回传DC云
  all_yck_cars_now<- all_yck_cars %>% dplyr::select(car_id,is_salable,other_reason) %>% dplyr::mutate(add_time=as.character(Sys.time()),status_type=1)
  all_yck_cars_now$other_reason<-ifelse(all_yck_cars_now$is_salable==0,'',all_yck_cars_now$other_reason)
  ##第六部分：
  temp_out_r<- all_yck_cars %>% 
    dplyr::select(-other_reason) %>% dplyr::mutate(update_time=as.character(Sys.time()),version_number=1)
  for (i in 1:dim(temp_out_r)[2]) {
    temp_out_r[,i][which(is.na(temp_out_r[,i]))]<-""
    temp_out_r[,i]<-gsub(";","",temp_out_r[,i])
  }
  return(list(all_yck_cars_out=all_yck_cars_now,temp_out_r=temp_out_r))
}

##****************调用2：接口调用（实时）********##
interfun_yckdc_itd<-function(input_carsid,ip_type='yck'){
  input_carsid<-as.character(input_carsid)
  ip_out<-fun_mysql_config_up(ip_type)
  sql_yckdc_itd_interface<-paste0("SELECT a.id car_id,is_sold,is_stop,auction_time_id,pm_transfer,operate_status,c.type project_type,is_confirm,inspection_valid_date,
                                  deal_status,update_type,violation_status,is_over,record_id,FROM_UNIXTIME(query_time) violation_query_time,violation_info2,
                                  driving_license_status,register_license_status,mortgage_status,insurance_status,FROM_UNIXTIME(operator_time) license_operator_time,
                                  operator_id,compulsory_insurance_label,yearly_inspection_label,is_application FROM yck_cars a
                                  LEFT JOIN yck_cars_violation b ON a.id=b.car_id
                                  INNER JOIN yck_project c ON a.belong_project=c.id
                                  LEFT JOIN yck_assets d ON a.id=d.car_id
                                  LEFT JOIN (SELECT car_id,1 is_application FROM yck_application sa
                                  INNER JOIN yck_application_cars sb ON sa.id=sb.apply_id WHERE sb.car_status NOT IN (3,4) AND (sa.status!=5 AND sa.is_delete=0)) e ON a.id=e.car_id
                                  WHERE a.id in(",paste0(input_carsid,collapse = ','),")")
  query1_interface<-tryCatch({fun_mysqlload_query(ip_out,sql_yckdc_itd_interface)},
                             error=function(e){0},
                             finally={0})
  temp_out<-data.frame(s=0)
  if(class(query1_interface)!="numeric"){
    if(nrow(query1_interface)>0){
      query1_interface<-fun_query1_violation_license(query1_interface)
      temp_out<-dealFun_cars_salable_reason(query1_interface)$all_yck_cars_out %>% dplyr::select(car_id,is_salable,other_reason)
    }
  }
  temp_out<-RJSONIO::toJSON(unname(plyr::alply(temp_out, 1, identity)))
  return(temp_out=temp_out)
}