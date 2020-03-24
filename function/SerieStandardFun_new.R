#**********车型对标函数集*************#
#主函数：对标车##V2.0版本对输入数据集进行修正，采取每个系列车仅选取一个代表车款##
main_fun_series_standard<-function(car_id){
  #读取车型配置数据
  input_modelname<-fun_mysqlload_query(local_defin,paste0("SELECT a.model_id,yck_brandid,yck_seriesid,model_name,model_price,model_year,car_level,auto,
  liter,'' liter_type,discharge_standard,ba_lwh,ba_engine,ba_gearbox,ba_structure,ee_max_mileage,sf_keyless_go,brk_front_tire_specs,
  ch_4WD_type,oeq_electric_trunk,oeq_aluminum_alloy_wheel,oeq_power_sunroof,oeq_panoramic_sunroof,ieq_reverse_radar,bd_wheelbase,
  ieq_multi_function_steering_wheel,st_driver_seat_electric_adjust,mm_bluetooth_carphone,tec_auto_start_stop,tec_panoramic_camera,
  mm_central_console_color_screen,lt_hid,lt_auto_head_light,lt_led_head_light FROM config_vdatabase_yck_major_info a 
                            INNER JOIN config_vdatabase_yck_detail_info b ON a.model_id=b.model_id
                              WHERE (a.car_level,a.is_green)=(SELECT car_level,is_green FROM config_vdatabase_yck_major_info WHERE model_id=",car_id,");"))
  col_name<-c("model_id","yck_brandid","yck_seriesid","model_name","model_price","model_year","car_level","auto","liter","liter_type","discharge_standard",
              "ba_lwh","ba_engine","ba_gearbox","ba_structure","ee_max_mileage",
              "sf_keyless_go","brk_front_tire_specs","ch_4WD_type","oeq_electric_trunk",
              "oeq_aluminum_alloy_wheel","oeq_power_sunroof","oeq_panoramic_sunroof","ieq_reverse_radar","bd_wheelbase",
              "ieq_multi_function_steering_wheel","st_driver_seat_electric_adjust","mm_bluetooth_carphone",
              "tec_auto_start_stop","tec_panoramic_camera","mm_central_console_color_screen","lt_hid","lt_auto_head_light",
              "lt_led_head_light")
  #报错输出本身车型
  output_error<-input_modelname[input_modelname$model_id==car_id,c('model_id','yck_brandid','yck_seriesid','model_name','model_price')]
  if(nrow(output_error)==0){
    output_error<-fun_mysqlload_query(local_defin,paste0("SELECT model_id,yck_brandid,yck_seriesid,model_name,model_price FROM 
                                                            config_vdatabase_yck_major_info  WHERE model_id=",car_id,";"))
    write.csv(output_error,paste0(price_model_loc,"/output/relation/",car_id,".csv"),row.names = F)
  }else{
    ###剔除的变量"st_seats_material"
    ##对标：第一部分：数据清洗##
    input_modelname<-input_modelname[,col_name]
    input_modelname$liter_type<-input_modelname$ba_engine
    input_modelname$liter_type<-gsub("\\ .*|[0-9]\\.[0-9]","",input_modelname$liter_type)
    input_modelname$liter_type<-str_extract(input_modelname$liter_type,"T|L|电动|增程")
    input_modelname$discharge_standard<-gsub("京|欧","国",input_modelname$discharge_standard)
    input_modelname$discharge_standard<-gsub("null","",input_modelname$discharge_standard)
    input_modelname<-separate(input_modelname, col = ba_lwh, into = c("ba_length", "ba_weight","ba_height"), sep = "\\*|\\×")
    input_modelname<-data.frame(input_modelname[,1:14],
                                ba_engine1=str_extract(input_modelname$ba_engine,"([0-9]{3}|[0-9]{2})马力"),
                                ba_engine2=str_extract(input_modelname$ba_engine,"L[2-8]"),
                                input_modelname[,16:ncol(input_modelname)])
    input_modelname$ba_engine1<-gsub("马力","",input_modelname$ba_engine1)
    input_modelname$ba_gearbox<-str_extract(input_modelname$ba_gearbox,"([0-9]{2}|[0-9])挡")
    input_modelname<-data.frame(input_modelname[,1:17],
                                ba_struct1=str_extract(input_modelname$ba_structure,"[1-9]门"),
                                ba_struct2=str_extract(input_modelname$ba_structure,"[1-9]座"),
                                ba_struct3=str_extract(input_modelname$ba_structure,"三厢|两厢|掀背|(硬|软)顶跑车|(硬|软)顶敞篷|掀背"),
                                input_modelname[,19:ncol(input_modelname)])
    input_modelname$brk_front_tire_specs<-gsub("Ｒ","R",input_modelname$brk_front_tire_specs)
    input_modelname$brk_front_tire_specs<-gsub("Ｐ","",input_modelname$brk_front_tire_specs)
    input_modelname<-data.frame(input_modelname[,1:22],
                                brk_front_tire_specs1=str_extract(input_modelname$brk_front_tire_specs,"[0-9]{3}(\\/|)"),
                                brk_front_tire_specs2=str_extract(input_modelname$brk_front_tire_specs,"(\\/)[0-9]{2}"),
                                brk_front_tire_specs3=str_extract(input_modelname$brk_front_tire_specs,"R[0-9]{2}"),
                                input_modelname[,24:ncol(input_modelname)])
    input_modelname$brk_front_tire_specs1<-gsub("\\/","",input_modelname$brk_front_tire_specs1)
    input_modelname$brk_front_tire_specs2<-gsub("\\/","",input_modelname$brk_front_tire_specs2)
    input_modelname$bd_wheelbase<-gsub('未知|标配|选配|-','-',input_modelname$bd_wheelbase)
    input_modelname<-sapply(input_modelname,as.character)
    for (i in 1:dim(input_modelname)[2]) {
      input_modelname[,i][which(is.na(input_modelname[,i]))]<-"-"
    }
    input_modelname<-data.frame(input_modelname)%>%dplyr::filter(ba_length!='-',ba_length!="1",ba_length!='',ba_weight!='未知',ba_height!='未知',ba_height!='1')
    input_modelname$ba_length<-as.integer(as.character(input_modelname$ba_length))
    input_modelname$ba_weight<-as.integer(as.character(input_modelname$ba_weight))
    input_modelname$ba_height<-as.integer(as.character(input_modelname$ba_height))
    #变量转换
    input_modelname$model_price<-as.numeric(as.character(input_modelname$model_price))
    col_order<-c("model_year","liter","discharge_standard","ba_length","ba_weight","ba_height","ba_engine1",
                 "ba_engine2","ba_gearbox","ba_struct1","ba_struct2","ee_max_mileage",
                 "brk_front_tire_specs1","brk_front_tire_specs2","brk_front_tire_specs3","bd_wheelbase")
    for (i in 1:length(col_order)) {
      input_modelname[,col_order[i]]<-factor(input_modelname[,col_order[i]],ordered = T)
    }
    tryCatch(write.csv(carMatchFun(input_modelname,car_id),paste0(price_model_loc,"/output/relation/",car_id,".csv"),row.names = F),
             error=function(e){cat(write.csv(output_error,paste0(price_model_loc,"/output/relation/",car_id,".csv"),row.names = F),conditionMessage(e),"\n\n")},
             finally={print("对标OK!")})
  }
}
#嵌入函数：第二部分：数据选取、对标车型模型构建##
carMatchFun<-function(input_modelname,car_id){
  set.seed(10)
  car_select<-input_modelname %>% dplyr::filter(model_id==car_id) %>% 
    dplyr::select(model_id,yck_brandid,yck_seriesid,model_name,model_price,model_year,car_level,auto,liter,liter_type)
  ##条件筛选
  input_filter<-input_modelname%>%dplyr::filter(auto==as.character(car_select$auto),model_price>car_select$model_price-2*(car_select$model_price%/%10)-1,
                                                model_price<car_select$model_price+2*(car_select$model_price%/%10)+1,
                                                as.integer(as.character(model_year))>as.integer(as.character(car_select$model_year))-3,
                                                as.integer(as.character(model_year))<as.integer(as.character(car_select$model_year))+3,
                                                car_level==as.character(car_select$car_level))
  input_filter$model_id<-as.integer(as.character(input_filter$model_id))
  ##去掉同系列其它车
  linshi<-input_filter%>%dplyr::filter(model_id==car_id)
  input_filter<-rbind(input_filter[input_filter$yck_seriesid!=car_select$yck_seriesid,],linshi)
  
  ##去掉部分字段
  input_filter_tr<-input_filter%>%dplyr::select(-model_id,-yck_brandid,-yck_seriesid,-model_name,-car_level,-auto)
  ##对标：第二部分：算法计算##
  #距离变换
  input_filter_distance<-daisy(input_filter_tr,metric = "gower",type = list(logratio = 3))
  tsne_obj <- tryCatch( Rtsne(input_filter_distance, is_distance = TRUE,perplexity=15),
                        error=function(e){Rtsne(input_filter_distance, is_distance = TRUE,perplexity=8)},
                        finally={print("距离计算OK!")})
  #tsne_obj <- Rtsne(input_filter_distance, is_distance = TRUE,perplexity=10)
  
  dis_series <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%
    dplyr::mutate(model_id=input_filter$model_id,yck_brandid = input_filter$yck_brandid,yck_seriesid = input_filter$yck_seriesid)
  dis_x<-dis_series$X[dis_series$model_id==car_id]
  dis_y<-dis_series$Y[dis_series$model_id==car_id]
  linshi<-dis_series%>%dplyr::mutate(distance=sqrt((X-dis_x)^2+(Y-dis_y)^2))%>%
    group_by(yck_brandid,yck_seriesid)%>%dplyr::mutate(min_distance=min(distance))%>%
    dplyr::filter(distance==min_distance)%>%ungroup(yck_brandid,yck_seriesid)%>%
    dplyr::select(model_id)
  input_filter<-inner_join(input_filter,linshi,by="model_id")
  
  ##去掉部分字段
  input_filter_tr<-input_filter%>%dplyr::select(-model_id,-yck_brandid,-yck_seriesid,-model_name,-car_level,-auto)
  ##对标：第二部分：算法计算##
  #距离变换
  input_filter_distance<-daisy(input_filter_tr,metric = "gower",type = list(logratio = 3))
  ##tsne_obj <- Rtsne(input_filter_distance, is_distance = TRUE,perplexity=perp)
  
  #算法区：运行其中某一算法#
  ##方法1:kmeans
  # mean_withinss<-NULL
  # for (i in 1:20) {
  #   result<-kmeans(input_filter_distance,centers = i)
  #   mean_withinss[i]<-mean(result$withinss)/result$tot.withinss
  #   print(i)
  # }
  # plot(1:20,mean_withinss)
  if(nrow(input_filter)<20){
    number_clu=3
  }else if(nrow(input_filter)<40){
    number_clu=4
  }else if(nrow(input_filter)<70){
    number_clu=5
  }else if(nrow(input_filter)<100){
    number_clu=12
  }else{
    number_clu=15
  }
  result<-kmeans(input_filter_distance,centers = number_clu)
  # tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%
  #   dplyr::mutate(cluster = factor(result$cluster),yck_brandid = input_filter$yck_brandid,yck_seriesid = input_filter$yck_seriesid,name = input_filter$model_name)
  # ggplot(aes(x = X, y = Y), data = tsne_data) +geom_point(aes(color = cluster))
  #结果查看
  sc<-data.frame(cc=result$cluster,input_filter)
  
  ##方法2:pam(K-mediods)
  # sil_width <- NULL
  # for(i in 2:6){
  #   pam_fit <- pam(input_filter_distance,diss = TRUE,k = i)
  #   sil_width<- rbind(sil_width,pam_fit$silinfo$avg.width)
  #   print(i)
  # }
  # plot(2:6, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width")
  # lines(2:6, sil_width)
  
  ##(nrow(input_filter)%/%11)+which(sil_width==max(sil_width,na.rm = T))-1
  pam_fit <- pam(input_filter_distance,diss = TRUE,k = number_clu+1)
  sc2<-data.frame(cc=pam_fit$clustering,input_filter)
  
  ##对标：第三部分：结果分析##
  output_result<-sc%>%dplyr::filter(cc==sc$cc[which(sc$model_id==car_id)])%>%.[1:6]
  output_result2<-sc2%>%dplyr::filter(cc==sc2$cc[which(sc2$model_id==car_id)])%>%.[1:6]
  output<-rbind(output_result,output_result2)%>%dplyr::select(-cc)%>%unique()
  return(output)
}