#local_defin<-data.frame(user = "yckdc",host="172.18.215.178",password= "YckDC888",dbname="yck-data-center",stringsAsFactors = F)
local_defin<-data.frame(user = "yckdc",host="47.106.189.86",password= "YckDC888",dbname="yck-data-center",stringsAsFactors = F)
#local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
#####接口一：模型估价
#####接口一：模型估价
interface_out1_model_mprice<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  interface_out1_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT recep_price,pm_price,fb_price FROM yck_project_model_result_match WHERE  query_lab='T' AND user_query_id=",parameter_user_query_id)),-1)
  dbDisconnect(loc_channel)
  ##是否营运车
  if(parameter_classification_operational=='营运'){
    interface_out1_return<-round(interface_out1_return*0.96,2)
  }
  ##量的多少
  interface_out1_return<-interface_out1_return%>%
    dplyr::mutate(display1=paste0(round(recep_price*0.95,2),"-",round((recep_price+pm_price)/2,2)))%>%
    dplyr::mutate(display2=paste0(round((recep_price+pm_price)/2,2),"-",round((pm_price+fb_price)/2,2)))%>%
    dplyr::mutate(display3=paste0(round((pm_price+fb_price)/2,2),"-",round(round(fb_price*1.05,2),2)))
  interface_out1_return<-toJSON(unname(alply(interface_out1_return, 1, identity)))
  #cat(interface_out1_model_mprice)
  return(interface_out1_return)
  rm(list = ls(all=T))
  gc()
}

#####接口二：亮点配置
interface_out2_detail_th<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  select_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id FROM yck_project_model_query WHERE user_query_id=",parameter_user_query_id)),-1)
  interface_out2_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT hl_configs,hl_configc FROM config_vdatabase_yck_major_info WHERE model_id=",select_query$select_model_id)),-1)
  dbDisconnect(loc_channel)
  interface_out2_return<-toJSON(unname(alply(interface_out2_return, 1, identity)))
  return(interface_out2_return)
  rm(list = ls(all=T))
  gc()
}

#####接口三：模型价格预测
interface_out3_model_fprice<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  interface_out3_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_result_future WHERE user_query_id=",parameter_user_query_id)),-1)
  dbDisconnect(loc_channel)
  ##是否营运车
  if(parameter_classification_operational=='营运'){
    interface_out3_return[,-1]<-round(interface_out3_return[,-1]*0.96,2)
  }
  ##量的多少
  interface_out3_return<-toJSON(unname(alply(interface_out3_return, 1, identity)))
  return(interface_out3_return)
  rm(list = ls(all=T))
  gc()
}

#####接口四：模型区域价格
interface_out4_model_diprice<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  interface_out4_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT province,fb_price FROM yck_project_model_result_tag WHERE user_query_id=",parameter_user_query_id)),-1)
  dbDisconnect(loc_channel)
  ##是否营运车
  if(parameter_classification_operational=='营运'){
    interface_out4_return$fb_price<-round(interface_out4_return$fb_price*0.96,2)
  }
  ##量的多少
  interface_out4_return<-toJSON(unname(alply(interface_out4_return, 1, identity)))
  return(interface_out4_return)
  rm(list = ls(all=T))
  gc()
}

#####接口五：相关二手车输出，输出价格分布(过去一年)同等车龄
interface_out5_sh_mprice<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  select_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id,DATEDIFF(select_partition_month,select_regDate)/365 user_years FROM yck_project_model_query WHERE user_query_id=",parameter_user_query_id)),-1)
  interface_out5_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT partition_month,quotes,car_platform,user_years FROM analysis_wide_table
                                                                WHERE id_che300=",select_query$select_model_id,"
                                                                AND quotes/model_price<0.98")),-1)
  dbDisconnect(loc_channel)
  interface_out5_return<-interface_out5_return%>%filter(abs(user_years-select_query$user_years)<0.2,partition_month>=format.Date(Sys.Date()-365,"%Y%m"))%>%dplyr::select(-user_years)
  interface_out5_return$car_platform[which(interface_out5_return$car_platform %in% c("czb","csp"))]<-"处置价"
  interface_out5_return$car_platform[which(interface_out5_return$car_platform %in% c("che168","rrc","youxin","guazi","yiche","che58","souche"))]<-"个人价"
  ##月均价格
  interface_out5_return<-interface_out5_return%>%group_by(partition_month,car_platform)%>%dplyr::summarise(quotes=round(mean(quotes),2))%>%
    ungroup()%>%as.data.frame()
  interface_out5_return<-toJSON(unname(alply(interface_out5_return, 1, identity)))
  return(interface_out5_return)
  rm(list = ls(all=T))
  gc()
}

#####接口六：二手车价格展示(省份筛选)
interface_out6_display<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number,parameter_province){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  select_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id,select_regDate,select_mile FROM yck_project_model_query WHERE user_query_id=",parameter_user_query_id)),-1)
  all_display<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT model_name,province,location,color,regDate,add_time,mile,quotes FROM analysis_wide_table
                                                      WHERE id_che300=",select_query$select_model_id,
                                                      " AND quotes/model_price<0.98 AND car_platform NOT IN('czb','csp') AND DATEDIFF(NOW(),add_time)<120
                                                      ORDER BY add_time DESC")),-1)
  dbDisconnect(loc_channel)
  interface_out6_display_l<-all_display%>%dplyr::mutate(ttop=abs(as.Date(regDate)-as.Date(select_query$select_regDate)))%>%
    filter(ttop<90,province==parameter_province)%>%top_n(-10,ttop)%>%
    dplyr::select(-ttop)%>%dplyr::mutate(ttop=abs(as.numeric(mile)-as.numeric(select_query$select_mile)))%>%top_n(-10,ttop)%>%dplyr::select(-ttop)
  if(nrow(interface_out6_display_l)<10 &nrow(interface_out6_display_l)>0){
    interface_out6_display_l2<-all_display%>%dplyr::mutate(ttop=abs(as.Date(regDate)-as.Date(select_query$select_regDate)))%>%filter(ttop<90)%>%
      top_n(-(10-nrow(interface_out6_display_l)),ttop)%>%dplyr::select(-ttop)
    interface_out6_display_l2<-interface_out6_display_l2%>%dplyr::mutate(ttop=abs(as.numeric(mile)-as.numeric(select_query$select_mile)))%>%
      top_n(-(10-nrow(interface_out6_display_l)),ttop)%>%dplyr::select(-ttop)
    interface_out6_return<-rbind(interface_out6_display_l,interface_out6_display_l2)
  }else{
    interface_out6_return=interface_out6_display_l
  }
  interface_out6_return<-toJSON(unname(alply(interface_out6_return, 1, identity)))
  return(interface_out6_return)
  rm(list = ls(all=T))
  gc()
}

#####接口七：详细配置对比
interface_out7_detail_mth<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  select_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id FROM yck_project_model_result_match WHERE user_query_id=",parameter_user_query_id)),-1)
  interface_out70_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT b.model_id,ba_level,ba_engine,ba_gearbox,ba_lwh,ba_structure,eng_emission,brk_front_tire_specs,
                                                                 tec_panoramic_camera,tec_auto_start_stop,oeq_power_sunroof,oeq_panoramic_sunroof,oeq_aluminum_alloy_wheel,
                                                                 oeq_electric_trunk,sf_keyless_go,ieq_multi_function_steering_wheel,st_seats_material,
                                                                 st_driver_seat_electric_adjust,st_front_passenger_seat_electric_adjust,mm_central_console_color_screen,
                                                                 mm_bluetooth_carphone,lt_led_head_light,ac_type FROM config_che300_detail_info a
                                                                 INNER JOIN config_vdatabase_yck_major_info b ON a.model_id=b.model_id WHERE b.model_id in (",paste0(select_query$select_model_id,collapse = ","),")")),-1)
  interface_out71_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id model_id,select_model_name model_name,query_lab,
                                                                 CONCAT(ROUND(DATEDIFF(select_partition_month,select_regDate)/365,2),'年(',select_mile,'万)') age_mile,CONCAT(select_model_price,'万') select_model_price,CONCAT(recep_price,' || ',pm_price,' || ',fb_price) price
                                                                 FROM yck_project_model_result_match WHERE user_query_id=",parameter_user_query_id)),-1)
  dbDisconnect(loc_channel)
  interface_out7_return<-merge(interface_out71_return,interface_out70_return,by="model_id")
  if(nrow(interface_out7_return)>1){interface_out7_return<-interface_out7_return[order(interface_out7_return$query_lab,decreasing = T),]}
  interface_out7_return<-interface_out7_return%>%dplyr::select(-model_id,-query_lab)
  interface_out7_return<-toJSON(unname(alply(interface_out7_return, 1, identity)))
  return(interface_out7_return)
  rm(list = ls(all=T))
  gc()
}

#####接口八：对标车型价格对比
interface_out8_match_price<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  interface_out8_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_name,select_model_price,ROUND(DATEDIFF(select_partition_month,select_regDate)/365,2) user_years,select_mile,recep_price,pm_price,fb_price
                                                                FROM yck_project_model_result_match WHERE user_query_id=",parameter_user_query_id)),-1)
  dbDisconnect(loc_channel)
  if(nrow(interface_out8_return)>1){interface_out8_return<-interface_out8_return[order(interface_out8_return$select_model_name),]}
  interface_out8_return<-toJSON(unname(alply(interface_out8_return, 1, identity)))
  return(interface_out8_return)
  rm(list = ls(all=T))
  gc()
}

#####接口九：销量对比
interface_out9_sales_new<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  select_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id FROM yck_project_model_result_match WHERE user_query_id=",parameter_user_query_id)),-1)
  interface_out9_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT c300.model_id,c300.series_name,stat_date,salesNum FROM config_plat_id_match a
                                                                INNER JOIN config_souhu_major_info b ON a.id_souhu=b.model_id
                                                                INNER JOIN spider_salesnum_souhu c ON b.series_id=c.series_id
                                                                INNER JOIN config_vdatabase_yck_major_info c300 ON a.id_che300=c300.model_id
                                                                WHERE id_che300 in (",paste0(select_query$select_model_id,collapse = ","),
                                                                ") UNION SELECT model_id,a.series_name,stat_date,salesNum FROM config_vdatabase_yck_major_info a
                                                                LEFT JOIN config_series_id_match b ON a.series_id=b.series_id_300
                                                                LEFT JOIN spider_salesnum_souhu c ON c.series_id=b.series_id_sohu
                                                                WHERE model_id in (",paste0(select_query$select_model_id,collapse = ","),")")),-1)%>%
    dplyr::select(-model_id)
  dbDisconnect(loc_channel)
  if(nrow(interface_out9_return)>1){interface_out9_return<-interface_out9_return[order(interface_out9_return$series_name),]}
  interface_out9_return<-toJSON(unname(alply(interface_out9_return, 1, identity)))
  return(interface_out9_return)
  rm(list = ls(all=T))
  gc()
}

#####接口十：新款折扣对比
interface_out10_prate<-function(parameter_user_query_id,parameter_classification_operational,parameter_model_number){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  select_query<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_model_id FROM yck_project_model_result_match WHERE user_query_id=",parameter_user_query_id)),-1)
  interface_out10_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT m.brand_name,m.series_name,m.model_year,DATE_FORMAT(mn.update_time,'%Y%m') update_time,ROUND(AVG(bare_discount_rate),2) quotes FROM config_autohome_major_info_tmp m
                                                                 INNER JOIN (SELECT series_name,MAX(model_year) model_year FROM config_autohome_major_info_tmp WHERE series_name in(SELECT series_name FROM config_plat_id_match a
                                                                 INNER JOIN config_autohome_major_info_tmp b ON a.id_autohome=b.model_id WHERE id_che300 in (",paste0(select_query$select_model_id,collapse = ","),"))
                                                                 GROUP BY series_name) n ON m.series_name=n.series_name AND m.model_year=n.model_year
                                                                 INNER JOIN discount_rate_history mn ON mn.model_id=m.model_id GROUP BY series_name,model_year,DATE_FORMAT(update_time,'%Y%m')")),-1)%>%
    dplyr::select(-brand_name)
  dbDisconnect(loc_channel)
  if(nrow(interface_out10_return)>1){interface_out10_return<-interface_out10_return[order(interface_out10_return$series_name),]}
  interface_out10_return<-toJSON(unname(alply(interface_out10_return, 1, identity)))
  return(interface_out10_return)
  rm(list = ls(all=T))
  gc()
}