############本函数为查询输出相关信息##############
main_fun_relation_output<-function(id_che300){
  #id_che300<-c(1150836,33014)
  ##########第一部分：获取最新款型折扣率信息
  new_quotes<-function(id){
    sql_config<-paste0("SELECT m.brand_name,m.series_name,m.model_year,mn.update_time,ROUND(AVG(bare_discount_rate),2) quotes FROM config_autohome_major_info_tmp m
                       INNER JOIN (SELECT series_name,MAX(model_year) model_year FROM config_autohome_major_info_tmp WHERE series_name=(SELECT series_name FROM config_plat_id_match a
                       INNER JOIN config_autohome_major_info_tmp b ON a.id_autohome=b.model_id WHERE id_che300 =",id,") GROUP BY series_name) n ON m.series_name=n.series_name AND m.model_year=n.model_year
                       INNER JOIN discount_rate_history mn ON mn.model_id=m.model_id GROUP BY series_name,update_time",sep='')
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    config_zk_new<-dbFetch(dbSendQuery(loc_channel,sql_config),-1)
    dbDisconnect(loc_channel)
    if(nrow(config_zk_new)==0){
      config_zk_new<-data.frame(id_che300=id,brand_name='',series_name='',model_year='',update_time='',quotes='')
    }else{
      config_zk_new<-data.frame(id_che300=id,config_zk_new)
    }
    return(config_zk_new)
  }
  ##拼凑
  config_zk_new<-NULL
  for (id in id_che300) {
    config_zk_new<-rbind(config_zk_new,new_quotes(id))
  }
  config_zk_new<-dcast(config_zk_new,id_che300+brand_name+series_name+model_year~update_time)
  if(length(grep("Var.5",names(config_zk_new)))==1){
    config_zk_new<-config_zk_new%>%dplyr::select(-Var.5)
  }else{print("")}
  
  ##########第一部分(2)：获取本款车的折扣信息
  sql_config<-paste0("SELECT id_che300,id_autohome,c.brand_name,c.series_name,c.model_name,b.update_time,ROUND(AVG(bare_discount_rate),2) quotes
                     FROM (SELECT id_che300,id_autohome FROM config_plat_id_match WHERE id_che300 in (",paste0(id_che300,sep='',collapse=','),")) a
                     INNER JOIN  discount_rate_history b ON a.id_autohome=b.model_id
                     INNER JOIN  config_autohome_major_info_tmp c ON a.id_autohome=c.model_id
                     GROUP BY id_che300,id_autohome,series_name,model_name,update_time",sep='')
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  config_zk<-dbFetch(dbSendQuery(loc_channel,sql_config),-1)
  dbDisconnect(loc_channel)
  if(nrow(config_zk)==0){
    config_zk=data.frame(out="无")
  }else{config_zk=dcast(config_zk,id_che300+id_autohome+brand_name+series_name+model_name~update_time)}
  
  
  
  ##########第二部分：获取亮点配置
  sql_config<-paste0("SELECT a.model_id,b.model_name,ba_level,ba_engine,ba_gearbox,ba_lwh,ba_structure,eng_emission,brk_front_tire_specs,
                     tec_panoramic_camera,tec_auto_start_stop,oeq_power_sunroof,oeq_panoramic_sunroof,oeq_aluminum_alloy_wheel,
                     oeq_electric_trunk,sf_keyless_go,ieq_multi_function_steering_wheel,st_seats_material,
                     st_driver_seat_electric_adjust,st_front_passenger_seat_electric_adjust,mm_central_console_color_screen,
                     mm_bluetooth_carphone,lt_led_head_light,ac_type FROM config_che300_detail_info a
                     INNER JOIN config_che300_major_info b ON a.model_id=b.model_id WHERE b.model_id in (",paste0(id_che300,sep='',collapse=','),")",sep='')
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  config_pz<-dbFetch(dbSendQuery(loc_channel,sql_config),-1)
  dbDisconnect(loc_channel)
  names(config_pz)<-c("id_che300","车名","级别","发动机","变速箱","长*宽*高(mm)","车身结构","环保标准","前轮胎规格",
                      "全景摄像头","发动机启停技术","电动天窗","全景天窗","铝合金轮圈","电动后备厢",
                      "无钥匙启动系统","多功能方向盘","座椅材质","主驾驶座电动调节","副驾驶座电动调节",
                      "中控台彩色大屏","蓝牙/车载电话","LED日间行车灯","空调控制方式")
  
  
  
  #######第三部分：获取车系销量数据（由于各个平台车系各异，且不尽规范，此处有待优化）
  sql_xl1<-paste0("SELECT c300.model_id,c300.series_name,stat_date,salesNum FROM config_plat_id_match a
                  INNER JOIN config_souhu_major_info b ON a.id_souhu=b.model_id
                  INNER JOIN spider_salesnum_souhu c ON b.series_id=c.series_id
                  INNER JOIN config_che300_major_info c300 ON a.id_che300=c300.model_id
                  WHERE id_che300 in (",paste0(id_che300,sep='',collapse=','),")",sep='')
  sql_xl2<-paste0("SELECT model_id,a.series_name,stat_date,salesNum FROM config_che300_major_info a 
                  LEFT JOIN config_series_id_match b ON a.series_id=b.series_id_300
                  LEFT JOIN spider_salesnum_souhu c ON c.series_id=b.series_id_sohu WHERE model_id in (",paste0(id_che300,sep='',collapse=','),")",sep='')
  sql_xl<-paste0(sql_xl1," UNION ",sql_xl2)
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  config_xl<-dbFetch(dbSendQuery(loc_channel,sql_xl),-1)
  dbDisconnect(loc_channel)
  config_xl<-config_xl%>%dplyr::filter(stat_date>2013)%>%unique()%>%dcast(model_id+series_name~stat_date,mean,margins="salesNum")
  return(list(config_pz=config_pz,config_xl=config_xl,config_zk=config_zk,config_zk_new=config_zk_new))
}