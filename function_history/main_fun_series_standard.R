  #######################对标车######V2.0版本对输入数据集进行修正，采取每个系列车仅选取一个代表车款########
main_fun_series_standard<-function(car_id){
  ########读取车型配置数据#####
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  test<-dbFetch(dbSendQuery(loc_channel,"SELECT * FROM config_che300_major_info a 
                            INNER JOIN config_che300_detail_info b ON a.model_id=b.model_id
                            INNER JOIN config_series_class c ON a.brand_name=c.brand;"),-1)
  dbDisconnect(loc_channel)
  col_name<-c("model_id","brand_name","series_name","model_name","model_price","model_year","car_level","auto","liter","liter_type","discharge_standard",
              "ba_lwh","ba_engine","ba_gearbox","ba_structure","ee_max_mileage",
              "sf_keyless_go","brk_front_tire_specs","ch_4WD_type","oeq_electric_trunk",
              "oeq_aluminum_alloy_wheel","oeq_power_sunroof","oeq_panoramic_sunroof","ieq_reverse_radar",
              "ieq_multi_function_steering_wheel","st_driver_seat_electric_adjust","mm_bluetooth_carphone",
              "tec_auto_start_stop","tec_panoramic_camera","mm_central_console_color_screen","lt_hid","lt_auto_head_light",
              "lt_led_head_light")
  ###剔除的变量"st_seats_material"
  ###########################第一部分：数据清洗############################
  test<-test[,col_name]
  test$liter_type<-test$ba_engine
  test$liter_type<-gsub("\\ .*|[0-9]\\.[0-9]","",test$liter_type)
  test$liter_type<-str_extract(test$liter_type,"T|L|电动|增程")
  test$discharge_standard<-gsub("京|欧","国",test$discharge_standard)
  test$discharge_standard<-gsub("null","",test$discharge_standard)
  test<-separate(test, col = ba_lwh, into = c("ba_length", "ba_weight","ba_height"), sep = "\\*|\\×")
  test<-data.frame(test[,1:14],
                   ba_engine1=str_extract(test$ba_engine,"([0-9]{3}|[0-9]{2})马力"),
                   ba_engine2=str_extract(test$ba_engine,"L[2-8]"),
                   test[,16:ncol(test)])
  test$ba_engine1<-gsub("马力","",test$ba_engine1)
  test$ba_gearbox<-str_extract(test$ba_gearbox,"([0-9]{2}|[0-9])挡")
  test<-data.frame(test[,1:17],
                   ba_struct1=str_extract(test$ba_structure,"[1-9]门"),
                   ba_struct2=str_extract(test$ba_structure,"[1-9]座"),
                   ba_struct3=str_extract(test$ba_structure,"三厢|两厢|掀背|(硬|软)顶跑车|(硬|软)顶敞篷|掀背"),
                   test[,19:ncol(test)])
  test$brk_front_tire_specs<-gsub("Ｒ","R",test$brk_front_tire_specs)
  test$brk_front_tire_specs<-gsub("Ｐ","",test$brk_front_tire_specs)
  test<-data.frame(test[,1:22],
                   brk_front_tire_specs1=str_extract(test$brk_front_tire_specs,"[0-9]{3}(\\/|)"),
                   brk_front_tire_specs2=str_extract(test$brk_front_tire_specs,"(\\/)[0-9]{2}"),
                   brk_front_tire_specs3=str_extract(test$brk_front_tire_specs,"R[0-9]{2}"),
                   test[,24:ncol(test)])
  test$brk_front_tire_specs1<-gsub("\\/","",test$brk_front_tire_specs1)
  test$brk_front_tire_specs2<-gsub("\\/","",test$brk_front_tire_specs2)
  test<-sapply(test,as.character)
  for (i in 1:dim(test)[2]) {
    test[,i][which(is.na(test[,i]))]<-"-"
  }
  test<-data.frame(test)%>%dplyr::filter(ba_length!='-',ba_length!="1",ba_length!='',ba_weight!='未知',ba_height!='未知',ba_height!='1')
  test$ba_length<-as.integer(as.character(test$ba_length))
  test$ba_weight<-as.integer(as.character(test$ba_weight))
  test$ba_height<-as.integer(as.character(test$ba_height))
  ######变量转换######
  test$model_price<-as.numeric(as.character(test$model_price))
  col_order<-c("model_year","liter","discharge_standard","ba_length","ba_weight","ba_height","ba_engine1",
               "ba_engine2","ba_gearbox","ba_struct1","ba_struct2","ee_max_mileage",
               "brk_front_tire_specs1","brk_front_tire_specs2","brk_front_tire_specs3")
  for (i in 1:length(col_order)) {
    test[,col_order[i]]<-factor(test[,col_order[i]],ordered = T)
  }
  
  ############################第二部分：数据选取#############################
  car_match<-function(car_id){
    set.seed(10)
    car_select<-test[test$model_id==car_id,1:10]
    ##条件筛选
    aa<-test%>%dplyr::filter(auto==as.character(car_select$auto),model_price>car_select$model_price-2*(car_select$model_price%/%10)-1,
                      model_price<car_select$model_price+2*(car_select$model_price%/%10)+1,
                      as.integer(as.character(model_year))>as.integer(as.character(car_select$model_year))-3,
                      as.integer(as.character(model_year))<as.integer(as.character(car_select$model_year))+3,
                      car_level==as.character(car_select$car_level))
    aa$model_id<-as.integer(as.character(aa$model_id))
    ##去掉同系列其它车
    linshi<-aa%>%dplyr::filter(model_id==car_id)
    aa<-rbind(aa[aa$series_name!=car_select$series_name,],linshi)
    
    ##去掉部分字段
    aa_tr<-aa%>%dplyr::select(-model_id,-brand_name,-series_name,-model_name,-car_level,-auto)
    ###########################第二部分：算法计算####################################
    ######距离变换#####
    aa_distance<-daisy(aa_tr,metric = "gower",type = list(logratio = 3))
    tsne_obj <- tryCatch( Rtsne(aa_distance, is_distance = TRUE,perplexity=15),
             error=function(e){Rtsne(aa_distance, is_distance = TRUE,perplexity=8)},
             finally={print("结果输出")})
    #tsne_obj <- Rtsne(aa_distance, is_distance = TRUE,perplexity=10)
    
    ###########################################
    dis_series <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%
      dplyr::mutate(model_id=aa$model_id,brand_name = aa$brand_name,series_name = aa$series_name)
    dis_x<-dis_series$X[dis_series$model_id==car_id]
    dis_y<-dis_series$Y[dis_series$model_id==car_id]
    linshi<-dis_series%>%dplyr::mutate(distance=sqrt((X-dis_x)^2+(Y-dis_y)^2))%>%
      group_by(brand_name,series_name)%>%dplyr::mutate(min_distance=min(distance))%>%
      dplyr::filter(distance==min_distance)%>%ungroup(brand_name,series_name)%>%
      dplyr::select(model_id)
    aa<-inner_join(aa,linshi,by="model_id")
    
    ##去掉部分字段
    aa_tr<-aa%>%dplyr::select(-model_id,-brand_name,-series_name,-model_name,-car_level,-auto)
    ###########################第二部分：算法计算####################################
    ######距离变换#####
    aa_distance<-daisy(aa_tr,metric = "gower",type = list(logratio = 3))
    ##tsne_obj <- Rtsne(aa_distance, is_distance = TRUE,perplexity=perp)
    
    ###################算法区：运行其中某一算法#################
    ########方法1:kmeans
    # mean_withinss<-NULL
    # for (i in 1:20) {
    #   result<-kmeans(aa_distance,centers = i)
    #   mean_withinss[i]<-mean(result$withinss)/result$tot.withinss
    #   print(i)
    # }
    # plot(1:20,mean_withinss)
    if(nrow(aa)<20){
      number_clu=3
    }else if(nrow(aa)<40){
      number_clu=4
    }else if(nrow(aa)<70){
      number_clu=5
    }else if(nrow(aa)<100){
      number_clu=12
    }else{
      number_clu=15
    }
    result<-kmeans(aa_distance,centers = number_clu)
    # tsne_data <- tsne_obj$Y %>%data.frame() %>%setNames(c("X", "Y")) %>%
    #   dplyr::mutate(cluster = factor(result$cluster),brand_name = aa$brand_name,series_name = aa$series_name,name = aa$model_name)
    # ggplot(aes(x = X, y = Y), data = tsne_data) +geom_point(aes(color = cluster))
    #结果查看
    sc<-data.frame(cc=result$cluster,aa)
    
    ########方法2:pam(K-mediods)
    # sil_width <- NULL
    # for(i in 2:6){
    #   pam_fit <- pam(aa_distance,diss = TRUE,k = i)
    #   sil_width<- rbind(sil_width,pam_fit$silinfo$avg.width)
    #   print(i)
    # }
    # plot(2:6, sil_width,xlab = "Number of clusters",ylab = "Silhouette Width")
    # lines(2:6, sil_width)
    
    #####(nrow(aa)%/%11)+which(sil_width==max(sil_width,na.rm = T))-1
    pam_fit <- pam(aa_distance,diss = TRUE,k = number_clu+1)
    sc2<-data.frame(cc=pam_fit$clustering,aa)
    
    #########################第三部分：结果分析#############
    output_result<-sc%>%dplyr::filter(cc==sc$cc[which(sc$model_id==car_id)])%>%.[1:6]
    output_result2<-sc2%>%dplyr::filter(cc==sc2$cc[which(sc2$model_id==car_id)])%>%.[1:6]
    output<-rbind(output_result,output_result2)%>%dplyr::select(-cc)%>%unique()
    #####2018-9-4添加，统一使用清洗后的车型名
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    output<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT car_id model_id,car_name brand_name,car_series1 series_name,car_model_name model_name,car_price model_price
                                                   FROM analysis_che300_cofig_info WHERE car_id in (",paste0(output$model_id,collapse = ','),")",sep='')),-1)
    dbDisconnect(loc_channel)
    return(output)
  }
  output_error<-test[test$model_id==car_id,c('model_id','brand_name','series_name','model_name','model_price')]
  tryCatch(write.csv(car_match(car_id),paste0(price_model_loc,"\\output\\relation\\",car_id,".csv"),row.names = F),
           error=function(e){cat(write.csv(output_error,paste0(price_model_loc,"\\output\\relation\\",car_id,".csv"),row.names = F),conditionMessage(e),"\n\n")},
           finally={print("结果输出f")})
}
