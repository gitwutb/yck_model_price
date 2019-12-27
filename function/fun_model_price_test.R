#模型使用函数库(核心库)
##********第一部分：模型数据处理、训练等**********##
#层次二---参数输入：训练模型中车龄及公里数选择
fun_parameter_ym<-function(partition_month,regDate){
  select_user_year<-as.numeric(as.character(round((as.Date(partition_month)-as.Date(regDate))/365,2)))
  case="11"
  user_years_plower=0.1
  user_years_pupper=15
  mile_plower=0.01
  mile_pupper=30
  return(list(user_years_plower=user_years_plower,user_years_pupper=user_years_pupper,mile_plower=mile_plower,mile_pupper=mile_pupper,case=case))
}

##本函数为建模数据选取及处理过程####最新修改日期：2018年4月27日（25）
fun_input_train<-function(input_analysis,select_input_transfor){
  select_brand<-select_input_transfor$select_brand
  select_series<-select_input_transfor$select_series
  select_model_year<-select_input_transfor$select_model_year
  select_model_price<- select_input_transfor$select_model_price
  select_auto<-select_input_transfor$select_auto
  select_car_level<-select_input_transfor$select_car_level
  select_regDate<-as.Date(select_input_transfor$select_regDate)
  select_mile<-select_input_transfor$select_mile
  select_partition_month<-as.Date(select_input_transfor$select_partition_month)
  ##变换参数
  parameter_ym_value<-fun_parameter_ym(select_partition_month,select_regDate)
  user_years_plower=parameter_ym_value$user_years_plower
  user_years_pupper=parameter_ym_value$user_years_pupper
  mile_plower=parameter_ym_value$mile_plower
  mile_pupper=parameter_ym_value$mile_pupper
  case=parameter_ym_value$case
  
  ###训练数据选取--
  input_analysis$quotes<-input_analysis$quotes_p
  input_train<-input_analysis%>%dplyr::filter(user_years>user_years_plower&user_years<user_years_pupper&mile>mile_plower&mile<mile_pupper)%>%
    dplyr::select(-quotes_p,-regDate,-partition_month,-brand,-series,-parti_year,-reg_month,-yck_seriesid) 
  ##20180720##
  if(nrow(input_train)<500){
    input_train<-input_analysis%>%
      dplyr::select(-quotes_p,-regDate,-partition_month,-brand,-series,-parti_year,-reg_month,-yck_seriesid)
  }
  #2018年4月27日##
  parameter_residuals<-0.09
  input_train_linshi<-NULL
  for (i in unique(input_train$car_platform)) {
    input_train_one<-input_train%>%dplyr::filter(car_platform==i)
    aa1<-lm(input_train_one$quotes~input_train_one$mile+input_train_one$user_years)
    input_train_one<-input_train_one%>%dplyr::mutate(residuals=aa1$residuals,fit=aa1$fitted.values)%>%dplyr::filter(abs(residuals)<parameter_residuals)
    #----20180711增加（当训练样本过多则选取精度最高的样本）
    parameter_res=parameter_residuals
    while (nrow(input_train_one)>2600) {
      parameter_res=parameter_res-0.002
      input_train_one<-input_train_one%>%dplyr::filter(abs(residuals)<parameter_res)
    }
    #----20180711增加##print(paste0(i,parameter_res))
    input_train_linshi<-rbind(input_train_linshi,input_train_one)
  }
  
  #拼接##
  input_train<-input_train_linshi%>%dplyr::select(-residuals,-fit)
  #因子变量因子水平（使训练及测试数据样式归一化）
  input_train<-fun_factor_standar(input_train)
  return(input_train=input_train)
}
##本函数为建模数据选取及处理过程####2018/8/27更改select_partition_month的parti_year为reg_year
fun_input_test<-function(select_input_transfor){
  parameter_monthd<-2
  input_transfor<-select_input_transfor[rep(1,parameter_monthd*2+1),]
  input_transfor$select_partition_month<-
    as.character(as.Date(input_transfor$select_partition_month)+seq(-30*parameter_monthd,30*parameter_monthd,30))
  input_transfor$select_mile<-select_input_transfor$select_mile/as.numeric(round(difftime(as_datetime(select_input_transfor$select_partition_month),as_datetime(select_input_transfor$select_regDate),units="days")/365,2))*
    as.numeric(round(difftime(as_datetime(input_transfor$select_partition_month),as_datetime(input_transfor$select_regDate),units="days")/365,2))
  select_brand<-input_transfor$select_brand
  select_series<-input_transfor$select_series
  select_model_year<-input_transfor$select_model_year
  select_model_price<- input_transfor$select_model_price
  select_auto<-input_transfor$select_auto
  select_car_level<-input_transfor$select_car_level
  select_regDate<-as.Date(input_transfor$select_regDate)
  select_mile<-input_transfor$select_mile
  select_partition_month<-as.Date(input_transfor$select_partition_month)
  
  ##测试数据输入-test_input---------0711修改剔除auto和car_level
  level_province<-c("安徽","陕西","上海","湖南","广东","湖北","江苏","广西","浙江","辽宁","北京",
                    "山东","河南","四川","河北","福建","黑龙江","山西","重庆","新疆","贵州","吉林",
                    "云南","海南","天津","江西","甘肃","宁夏","内蒙古","青海","西藏")
  level_platform<-c("che168","rrc","youxin","guazi","yiche","che58","souche","czb","csp")
  test_input<-data.frame(car_platform=rep(level_platform,each=length(select_model_year)),
                         model_year=select_model_year,
                         auto=select_auto,
                         #car_level=select_car_level,
                         model_price=select_model_price,
                         ##1-4万公里rep(1:5,each=6)
                         mile=select_mile,
                         ##30多个省份rep(unique(input_transfor$province),each=6)
                         province=rep(level_province,each=length(level_platform)*length(select_model_year)),
                         user_years=as.numeric(as.character(round((select_partition_month-select_regDate)/365,2))),
                         reg_year=str_sub(select_regDate,1,4),
                         #reg_month=str_sub(select_regDate,6,7),
                         #parti_year=str_sub(select_partition_month,1,4),
                         parti_month=str_sub(select_partition_month,6,7))
  #因子变量因子水平（使训练及测试数据样式归一化）
  test_input<-fun_factor_standar(test_input)
  #test_input$model_price<-factor(test_input$model_price,levels = levels(ana1$model_price))
  return(test_input=test_input)
}
##fun_model_input模型使用数据选取##
fun_input<-function(select_input_transfor){
  #20190726样本缺失处理
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  seriestandard_list<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT a.*,b.count_s FROM config_match_seriestandard a 
          LEFT JOIN analysis_wide_table_cous b ON a.yck_seriesid=b.yck_seriesid 
          WHERE a.bd_yck_seriesid='",as.character(select_input_transfor$yck_seriesid),"' ORDER BY count_p DESC")),-1)
  series_list<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT DISTINCT c.car_country bd_car_country,a.yck_seriesid,c.car_country,0 count_p,b.count_s FROM config_vdatabase_yck_major_info a 
                                                INNER JOIN analysis_wide_table_cous b ON a.yck_seriesid=b.yck_seriesid
                                                INNER JOIN config_vdatabase_yck_brand c ON a.yck_brandid=c.yck_brandid
                                                  WHERE c.car_country='",as.character(unique(seriestandard_list$bd_car_country)),
                                                      "' AND a.car_level='",as.character(select_input_transfor$select_car_level),
                                                      "' AND is_green=",select_input_transfor$is_green," ORDER BY count_s DESC")),-1)
  dbDisconnect(loc_channel)
  seriestandard_list$count_s[is.na(seriestandard_list$count_s)]<-0
  if(nrow(series_list)>0){ series_list<-data.frame(bd_yck_seriesid=as.character(select_input_transfor$yck_seriesid),series_list) %>% 
    dplyr::filter(!(yck_seriesid %in% seriestandard_list$yck_seriesid))}
  seriestandard_list<-rbind(seriestandard_list,series_list)
  a1<-seriestandard_list %>% dplyr::filter(bd_yck_seriesid==yck_seriesid) %>% dplyr::mutate(sum_code=sum(count_s))
  if(mean(a1$sum_code)<4000){
    seriestandard_listt<-seriestandard_list %>% dplyr::filter(bd_car_country==car_country) %>% dplyr::mutate(sum_code=(cumsum(count_s)-4000)/abs(cumsum(count_s)-4000)) %>% 
      dplyr::mutate(sum_code1=c(-1,sum_code[-length(sum_code)]))
    n_number<- which(seriestandard_listt$sum_code!=seriestandard_listt$sum_code1)
    if(length(n_number)>0){a1<-seriestandard_listt[1:n_number,] %>% dplyr::mutate(sum_code=sum(count_s))}else{
      a1<-seriestandard_list %>% dplyr::filter(bd_car_country==car_country) %>% dplyr::mutate(sum_code=sum(count_s))
      if(mean(a1$sum_code)<1000){
        a1<-seriestandard_list %>% dplyr::mutate(sum_code=sum(count_s))
      }
    }
  }
  series_list_fin<-paste0(as.character(unique(c(select_input_transfor$yck_seriesid,a1$yck_seriesid))),collapse = "','")
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  input_orig<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT car_platform,model_year,brand,series,yck_seriesid,auto,regDate,quotes,
                    model_price,mile,province,user_years,a.partition_month FROM analysis_wide_table a WHERE  yck_seriesid IN('",series_list_fin,
                                                     "') AND is_green=",select_input_transfor$is_green)),-1)
  dbDisconnect(loc_channel)
  if(nrow(input_orig)<300){
    para_mprice1<-0.5;para_mprice2<-1.5
    if(select_input_transfor$is_green==2){para_mprice1<-0.01;para_mprice2<-10}
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    series_list_fin<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_major_info 
                    WHERE car_level ='",as.character(select_input_transfor$select_car_level),
                                                            "' AND model_price>",as.numeric(select_input_transfor$select_model_price)*para_mprice1,
                                                            " AND model_price<",as.numeric(select_input_transfor$select_model_price)*para_mprice2,
                                                            " AND is_green=",select_input_transfor$is_green)),-1)
    series_list_fin<-paste0(as.character(series_list_fin$yck_seriesid),collapse = ",")
    input_orig<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT car_platform,model_year,brand,series,yck_seriesid,auto,regDate,quotes,
                    model_price,mile,province,user_years,a.partition_month FROM analysis_wide_table a WHERE  yck_seriesid IN('",series_list_fin,
                                                       "') AND is_green=",select_input_transfor$is_green)),-1) %>% rbind(input_orig) %>% unique()
    dbDisconnect(loc_channel)
  }
  input_analysis<-input_orig
  #数据处理##
  input_analysis<-data.frame(input_analysis,quotes_p=round(input_analysis$quotes/input_analysis$model_price,2))
  input_analysis<-input_analysis%>%dplyr::filter(quotes_p>0.05&quotes_p<1.01)
  input_analysis<-input_analysis%>%dplyr::filter(user_years<1|quotes_p<1)
  input_analysis<-input_analysis%>%dplyr::filter(regDate>'2000-01-01'&regDate!='NA')
  input_analysis<-data.frame(input_analysis,reg_year=str_sub(input_analysis$regDate,1,4),
                             reg_month=str_sub(input_analysis$regDate,6,7),
                             parti_year=str_sub(input_analysis$partition_month,1,4),
                             parti_month=str_sub(input_analysis$partition_month,5,6))
  return(input_analysis)
}
##fun_factor_transform用于将一些变量转化为因子变量##20180117--14:00修改model_price为数值型
fun_factor_standar<-function(input_value){
  ##factor的level水平（因子选取的宽度对于模型输出有影响）
  level_month<-c("01","02","03","04","05","06","07","08","09","10","11","12")
  level_year<-c(1997:2030)
  level_platform<-c("che168","rrc","youxin","guazi","yiche","che58","souche","czb","csp")
  level_province<-c("安徽","陕西","上海","湖南","广东","湖北","江苏","广西","浙江","辽宁","北京",
                    "山东","河南","四川","河北","福建","黑龙江","山西","重庆","新疆","贵州","吉林",
                    "云南","海南","天津","江西","甘肃","宁夏","内蒙古","青海","西藏" )
  level_auto<-c("自动","手动","电动")
  level_car_level<-c("中大型SUV","中大型车","中型车","中型SUV","紧凑型车","跑车","MPV","紧凑型SUV","小型SUV","小型车",
                     "皮卡","全尺寸SUV","微型车","豪华车","微面","客车","其它","卡车")
  input_value$car_platform<-factor(input_value$car_platform,levels=level_platform)
  input_value$province<-factor(input_value$province,levels = level_province)
  input_value$reg_year<-factor(input_value$reg_year,levels = level_year)
  #input_value$reg_month<-factor(input_value$reg_month,levels = level_month)
  #input_value$parti_year<-factor(input_value$parti_year,levels = level_year)
  input_value$parti_month<-factor(input_value$parti_month,levels = level_month)
  #20180711修改
  input_value$auto<-factor(input_value$auto,levels = level_auto)
  #input_value$car_level<-factor(input_value$car_level,levels = level_car_level)
  input_value$model_year<-factor(input_value$model_year,levels = level_year)
  input_value$mile<-as.numeric(as.character(input_value$mile))
  return(input_value)
}
##本函数为训练模型：输入ana1，输出模型结构
fun_model_train<-function(input_train,price_model_loc,model_code){
  ##模型训练
  model.svm <- e1071::svm(quotes~., input_train,gamma=0.02) 
  #preds <- predict(model.svm, input_train)
  input_train<-input_train %>% dplyr::select(car_platform,quotes)
  save(model.svm,file=paste0(price_model_loc,"\\model_net\\",model_code,".RData"))
  save(input_train,file=paste0(price_model_loc,"\\model_net\\",model_code,"input_train.RData"))
  return(model.svm)
}
##本函数为测试模型：输入--测试数据-模型结构，输出结果##输出由综合输出一个值变为输出两个值（拍卖/发布）
fun_model_test<-function(select_input_transfor,input_test,model.svm){
  ##模型预测
  pre_preds <- predict(model.svm, input_test) 
  test_output<-data.frame(input_test,pre_preds)
  test_output$pre_preds<-round(test_output$model_price*test_output$pre_preds,3) #将折扣率转换为残值
  #配置平台权重表
  config_platform<-data.frame(car_platform=c("guazi","rrc","yiche","che168","youxin","che58","souche","czb","csp"),
                              car_platform_class=c("fb","fb","fb","fb","fb","fb","fb","pm","pm"),
                              platform_weight=c(0.25,0.2,0.15,0.1,0.1,0.15,0.05,0.5,0.5))
  test_output$car_platform<-as.character(test_output$car_platform)
  config_platform$car_platform<-as.character(config_platform$car_platform)
  parameter_month<-nrow(input_test)/nrow(config_platform)/31
  output_final<-inner_join(config_platform,test_output,by="car_platform")%>%
    dplyr::mutate(price=platform_weight*pre_preds)%>%group_by(province,car_platform_class)%>%dplyr::summarise(preds=sum(price)/parameter_month)%>%
    dcast(province~car_platform_class)
  data.frame(select_input_transfor,output_final)
}
##---****提取同级别车的对比参数***---##
fun_pred_compare_line<-function(select_input){
  sql_config<-paste0("SELECT platform_class car_platform_class,parmeter_y,parmeter_m,parmeter_v FROM config_quotes_class
                     WHERE (car_level,auto)= (SELECT car_level,auto FROM config_vdatabase_yck_major_info WHERE model_id =",select_input$select_model_id,")",sep='')
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  return_config_reg<-dbFetch(dbSendQuery(loc_channel,sql_config),-1)
  if(nrow(return_config_reg)>0){
    return_config_reg<-return_config_reg
  }else{
    return_config_reg<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT platform_class car_platform_class,parmeter_y,parmeter_m,parmeter_v FROM config_quotes_class
                                                       WHERE car_level= '全级别'
                                                       AND auto= (SELECT auto FROM config_vdatabase_yck_major_info WHERE model_id =",select_input$select_model_id,")",sep='')),-1)
  }
  dbDisconnect(loc_channel)
  select_year<-round((as.Date(select_input$select_partition_month)-as.Date(select_input$select_regDate))/365,2)%>%as.character()%>%as.numeric()
  select_mile<-as.numeric(select_input$select_mile)
  return_config_reg<-return_config_reg%>%dplyr::mutate(ss=parmeter_v+select_year*parmeter_y+select_mile*parmeter_m)%>%dcast(.~car_platform_class)%>%.[,-1]
  names(return_config_reg)[names(return_config_reg) == "fb"] = c("fb_index")
  names(return_config_reg)[names(return_config_reg) == "pm"] = c("pm_index")
  return(return_config_reg)
}
##---****提取估值辅助参数输出***---##
fun_pred_out<-function(input_train,model.svm,select_input){
  input_train_temp<-input_train %>% dplyr::select(car_platform,quotes) %>% mutate(fitted=model.svm$fitted,residuals=model.svm$residuals)%>%
    dplyr::mutate(car_platform_type=ifelse(car_platform %in% c("czb","csp"),"pm_n","fb_n"))
  sample_size<-input_train_temp%>%group_by(car_platform_type)%>%dplyr::summarise(sample_size=n())%>%as.data.frame()
  if('pm_n'%in%sample_size$car_platform_type==F){sample_size<-rbind(sample_size,data.frame(car_platform_type='pm_n',sample_size=0))}
  #参数输出一：拟合解释率
  r_cor<-data.frame(r_cor=paste0(round(100*cor(input_train_temp$quotes,input_train_temp$fitted),1),'%'))
  #参数输出二：平均绝对误差（处理后）
  if(sample_size$sample_size[sample_size$car_platform_type=='pm_n']>50){
    n=sample_size$sample_size[sample_size$car_platform_type=='pm_n']
    r_pm<-input_train_temp %>% filter(car_platform_type=='pm_n') %>% 
      top_n(n-floor(n*0.05),residuals) %>% top_n(floor(n*0.1)-n,residuals) %>% summarise(r_pm=round(mean(abs(residuals)),4))
  }else{
    r_pm<-input_train_temp %>% filter(car_platform_type=='pm_n') %>% summarise(r_pm=round(mean(abs(residuals)),4))
  }
  if(sample_size$sample_size[sample_size$car_platform_type=='fb_n']>50){
    n=sample_size$sample_size[sample_size$car_platform_type=='fb_n']
    r_fb<-input_train_temp %>% filter(car_platform_type=='fb_n') %>% 
      top_n(n-floor(n*0.05),residuals) %>% top_n(floor(n*0.1)-n,residuals) %>% summarise(r_fb=round(mean(abs(residuals)),4))
  }else{
    r_fb<-input_train_temp %>% filter(car_platform_type=='fb_n') %>% summarise(r_fb=round(mean(abs(residuals)),4))
  }
  #参数输出三：
  sample_size<-sample_size%>%dcast(.~car_platform_type)%>%dplyr::select(-.)
  if(length(grep("pm_n",names(sample_size)))==1){sample_size<-sample_size}else{
    sample_size<-data.frame(sample_size,pm_n=as.integer(0))}
  #参数输出四：标准性数据（同级别）
  config_reg<-fun_pred_compare_line(select_input)
  return_pred_out<-data.frame(r_cor,r_fb,r_pm,sample_size,config_reg)
  return(return_pred_out)
}
##此函数为最终预测调用函数
fun_pred<-function(select_input){
  select_input_transfor<-fun_mysqlload_query(local_defin,paste0(c("SELECT yck_brandid,yck_seriesid,brand_name select_brand,series_name select_series,
                 model_year select_model_year,model_name select_model_name,model_price select_model_price,
                 auto select_auto,is_green,car_level select_car_level FROM config_vdatabase_yck_major_info WHERE model_id ="),select_input$select_model_id))
  select_input_transfor<-data.frame(select_input_transfor,select_input)
  case<-fun_parameter_ym(select_input_transfor$select_partition_month,select_input_transfor$select_regDate)$case
  model_code<-paste0(select_input_transfor$yck_seriesid,"T","IME","CASE",case,sep="")%>%toupper()
  #模型高效处理方法
  input_test<-fun_input_test(select_input_transfor)
  list_model<-list.files(paste0(price_model_loc,"\\model_net"), full.names = T,pattern = "input_train.RData")
  list_model<-gsub(".*model_net\\/|input_train.RData","",list_model)
  if(length(grep(model_code,list_model))==0){
    input_analysis<-fun_input(select_input_transfor)
    input_train<-fun_input_train(input_analysis,select_input_transfor)
    model.svm<-fun_model_train(input_train,price_model_loc,model_code)
  }else{
    load(paste0(paste0(price_model_loc,"\\model_net"),"\\",model_code,".RData"))
    load(paste0(paste0(price_model_loc,"\\model_net"),"\\",model_code,"input_train.RData"))
  }
  output_pre<-fun_model_test(select_input_transfor,input_test,model.svm)
  return_pred_out<-fun_pred_out(input_train,model.svm,select_input)
  output_pre<-data.frame(output_pre,return_pred_out)
  return(output_pre)
}

##此函数为最终预测调用函数(离线训练-循环)
fun_pred_round<-function(i){
  select_input<-select_input_org[i,]
  output_pre<-tryCatch({outline_series_fun_pred(select_input)},
                       error=function(e){select_input$select_model_id},
                       finally={NULL})
  return(output_pre)
}
##********第二部分：模型相关输出**********##
fun_pred_user_match<-function(select_input){
  #对标车型选取
  car_id<-select_input$select_model_id
  list_matchfile<-tryCatch({car_match<- read.csv(paste0(price_model_loc,"\\output\\relation\\",car_id,".csv"),header = T,sep = ",") %>%
    dplyr::filter(model_id!=car_id)},error=function(e){return(1)})
  if(class(list_matchfile)!='data.frame'){
    main_fun_series_standard(car_id)
    car_match<- read.csv(paste0(price_model_loc,"\\output\\relation\\",car_id,".csv"),header = T,sep = ",")%>%
      dplyr::filter(model_id!=car_id)
  }
  series_max<-fun_mysqlload_query(local_defin,paste0("SELECT series_name,sum(count_s) cou FROM analysis_wide_table_cous a
                                                   INNER JOIN config_vdatabase_yck_series b ON a.yck_seriesid=b.yck_seriesid
                                                   WHERE series_name in (","'",paste0(car_match$series_name,collapse = "','",sep=''),"'",") GROUP BY series_name",sep=''))
  series_max<-series_max%>%dplyr::top_n(4,cou)%>%dplyr::filter(cou>1000)
  car_match<-car_match%>%dplyr::filter(series_name %in% series_max$series)
  #对标车型预测
  match_output_pre<-NULL
  if(nrow(car_match)>0){
    select_input_db<-data.frame(user_query_id=select_input$user_query_id,select_model_id=car_match$model_id,select_input[,-c(1:2)])
    for (i in 1:nrow(select_input_db)) {
      linshi1<-fun_pred(select_input_db[i,])%>%dplyr::mutate(query_lab="F")
      match_output_pre<-rbind(match_output_pre,linshi1)
    }
  }
  return(match_output_pre=match_output_pre)
}
fun_pred_user_future<-function(select_input){
  #*标的车未来五个月预测值********##
  select_input_future<-select_input[rep(1,6),]
  select_input_future$select_partition_month<-as.character(as.Date(select_input_future$select_partition_month)+seq(0,31*5,31))
  select_input_future$select_mile<-select_input$select_mile/as.numeric(round(difftime(as_datetime(select_input$select_partition_month),as_datetime(select_input$select_regDate),units="days")/365,2))*
    as.numeric(round(difftime(as_datetime(select_input_future$select_partition_month),as_datetime(select_input_future$select_regDate),units="days")/365,2))
  match_output_future<-NULL
  for (i in 1:nrow(select_input_future)) {
    linshi1<-fun_pred(select_input_future[i,])
    match_output_future<-rbind(match_output_future,linshi1)
  }
  result_future<-dplyr::summarise(group_by(match_output_future,user_query_id,select_partition_month),fb_price=round(mean(fb),2))%>%
    ungroup()%>%as.data.frame()%>%dcast(user_query_id~select_partition_month)
  names(result_future)<-c("user_query_id","now_price","future1","future2","future3","future4","future5")
  return(result_future=result_future)
}
##综合输出（产品）##
model_main<-function(select_input,p_type=''){
  ##model_main:第一部分：标的车估值##
  output_pre<-fun_pred(select_input)%>%dplyr::mutate(query_lab="T")
  ##标的车型-区域价格
  result_bd<-dplyr::summarise(group_by(output_pre,user_query_id,province,select_model_price),fb_price=mean(fb),pm_price=mean(pm))%>% ungroup()%>%
    as.data.frame()%>%dplyr::mutate(recep_price=round(pm_price*0.94-0.25,2),pm_monitor=round(pm_price/select_model_price,3)) %>% 
    dplyr::select(user_query_id,province,fb_price,pm_price,recep_price,pm_monitor)
  fun_mysqlload_add(price_model_loc,local_defin,result_bd,paste0('yck_project_model',p_type,'_result_tag'),unique(select_input$user_query_id))
  fun_mysqlload_query(local_defin,paste0("UPDATE ",'yck_project_model',p_type,'_query'," SET query_statue=2 WHERE user_query_id=",unique(select_input$user_query_id),";"))
  
  #******标的车未来五个月预测值********##
  result_future<-fun_pred_user_future(select_input)
  fun_mysqlload_add(price_model_loc,local_defin,result_future,paste0('yck_project_model',p_type,'_result_future'),unique(select_input$user_query_id))
  #******对标车预测值********##
  match_output_pre<-fun_pred_user_match(select_input)
  result_compare<-dplyr::summarise(group_by(rbind(match_output_pre,output_pre),user_query_id,query_lab,select_model_id,select_model_name,select_model_price,
                                            select_regDate,select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm),
                                   fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index),
                                   r_cor=unique(r_cor),r_fb=mean(r_fb),r_pm=mean(r_pm))%>%ungroup()%>%as.data.frame()%>%
    dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3),sum_size=fb_n+pm_n)%>%
    dplyr::group_by(query_lab)%>%top_n(3,sum_size)%>%dplyr::select(-sum_size)%>%as.data.frame()
  result_compare$fb_index<-cut(abs(result_compare$fb_monitor-result_compare$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_compare$pm_index<-cut(abs(result_compare$pm_monitor-result_compare$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_compare<-result_compare%>%dplyr::mutate(recep_price=pm_price*0.94-0.25) %>%
    dplyr::select(select_model_id,user_query_id,query_lab,select_model_name,select_model_price,select_regDate,select_partition_month,
                  select_mile,fb_price,pm_price,fb_monitor,pm_monitor,fb_index,pm_index,recep_price,r_cor,r_fb,r_pm)
  fun_mysqlload_add(price_model_loc,local_defin,result_compare,paste0('yck_project_model',p_type,'_result_match'),unique(select_input$user_query_id))
  return(0)
}

##********第三部分：模型离线训练函数**********##
outline_series_fun_pred<-function(select_input){
  select_input_transfor<-fun_mysqlload_query(local_defin,paste0(c("SELECT yck_brandid,yck_seriesid,brand_name select_brand,series_name select_series,
                 model_year select_model_year,model_name select_model_name,model_price select_model_price,
                 auto select_auto,is_green,car_level select_car_level FROM config_vdatabase_yck_major_info WHERE model_id ="),select_input$select_model_id))
  select_input_transfor<-data.frame(select_input_transfor,select_input)
  case<-fun_parameter_ym(select_input_transfor$select_partition_month,select_input_transfor$select_regDate)$case
  model_code<-paste0(select_input_transfor$yck_seriesid,"T","IME","CASE",case,sep="")%>%toupper()
  input_analysis<-fun_input(select_input_transfor)
  input_train<-fun_input_train(input_analysis,select_input_transfor)
  model.svm<-fun_model_train(input_train,price_model_loc,model_code)
  return('ok')
}