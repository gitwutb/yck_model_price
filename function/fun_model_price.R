#模型使用函数库(核心库)
##############********第一部分：模型数据处理、训练等**********################
###############fun_select_transfor模型使用数据选取########0711修改
fun_select_transfor<-function(select_input){
  part_select<-c("SELECT yck_brandid,yck_seriesid,brand_name select_brand,series_name select_series,model_year select_model_year,model_name select_model_name,model_price select_model_price,auto select_auto,car_level select_car_level FROM config_vdatabase_yck_major_info WHERE ")
  part_condition<-c("model_id =")
  part_value<-select_input$select_model_id
  part_all<-paste0(part_select,part_condition,part_value)
  ######数据库查询
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  input_orig<-dbFetch(dbSendQuery(loc_channel,part_all),-1)
  dbDisconnect(loc_channel)
  select_input_tansfor<-data.frame(input_orig,select_input)
  return(select_input_tansfor)
}
##############层次二---参数输入：训练模型中车龄及公里数选择
fun_parameter_ym<-function(partition_month,regDate){
  select_user_year<-as.numeric(as.character(round((as.Date(partition_month)-as.Date(regDate))/365,2)))
  if (select_user_year<=0.5) {
    case="1"
    user_years_plower=0
    user_years_pupper=1
    mile_plower=0.01
    mile_pupper=4
  }else if (select_user_year<1.5){
    case="2"
    user_years_plower=0
    user_years_pupper=3
    mile_plower=0.01
    mile_pupper=12
  }else if (select_user_year<3){
    case="3"
    user_years_plower=0.6
    user_years_pupper=4
    mile_plower=0.01
    mile_pupper=15
  }else if (select_user_year<5){
    case="4"
    user_years_plower=2
    user_years_pupper=7
    mile_plower=1
    mile_pupper=20
  }else {
    case="5"
    user_years_plower=3
    user_years_pupper=15
    mile_plower=1
    mile_pupper=30
  }
  return(list(user_years_plower=user_years_plower,user_years_pupper=user_years_pupper,mile_plower=mile_plower,mile_pupper=mile_pupper,case=case))
}
##################################本函数为建模数据选取及处理过程############################################
#最新修改日期：2018年4月27日（25）
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
  }else{
    print(paste0(select_series,"初始样本量为",nrow(input_train)))
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
    while (nrow(input_train_one)>2500) {
      parameter_res=parameter_res-0.002
      input_train_one<-input_train_one%>%dplyr::filter(abs(residuals)<parameter_res)
    }
    #----20180711增加######print(paste0(i,parameter_res))
    input_train_linshi<-rbind(input_train_linshi,input_train_one)
  }
  
  #拼接##
  input_train<-input_train_linshi%>%dplyr::select(-residuals,-fit)
  #因子变量因子水平（使训练及测试数据样式归一化）
  input_train<-fun_factor_standar(input_train)
  return(input_train=input_train)
}
##################################本函数为建模数据选取及处理过程############################################
#2018/8/27更改select_partition_month的parti_year为reg_year
fun_input_test<-function(select_input_transfor){
  select_brand<-select_input_transfor$select_brand
  select_series<-select_input_transfor$select_series
  select_model_year<-select_input_transfor$select_model_year
  select_model_price<- select_input_transfor$select_model_price
  select_auto<-select_input_transfor$select_auto
  select_car_level<-select_input_transfor$select_car_level
  select_regDate<-as.Date(select_input_transfor$select_regDate)
  select_mile<-select_input_transfor$select_mile
  select_partition_month<-as.Date(select_input_transfor$select_partition_month)
  
  ##############测试数据输入-test_input---------0711修改剔除auto和car_level
  level_province<-c("安徽","陕西","上海","湖南","广东","湖北","江苏","广西","浙江","辽宁","北京",
                    "山东","河南","四川","河北","福建","黑龙江","山西","重庆","新疆","贵州","吉林",
                    "云南","海南","天津","江西","甘肃","宁夏","内蒙古","青海","西藏")
  level_platform<-c("che168","rrc","youxin","guazi","yiche","che58","souche","czb","csp")
  test_input<-data.frame(car_platform=level_platform,
                         model_year=select_model_year,
                         auto=select_auto,
                         #car_level=select_car_level,
                         model_price=select_model_price,
                         ##1-4万公里rep(1:5,each=6)
                         mile=select_mile,
                         ##30多个省份rep(unique(select_input_transfor$province),each=6)
                         province=rep(level_province,each=length(level_platform)),
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
###############fun_model_input模型使用数据选取########
fun_input<-function(select_input_transfor){
  part_select<-c("SELECT car_platform,model_year,brand,series,yck_seriesid,auto,regDate,quotes,model_price,mile,province,user_years,a.partition_month FROM analysis_wide_table a WHERE ")
  part_condition<-c("yck_seriesid =")
  part_value<-paste0("'",as.character(select_input_transfor$yck_seriesid),"'")
  part_all<-paste0(part_select,part_condition,part_value)
  ######数据库查询
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  input_orig<-dbFetch(dbSendQuery(loc_channel,part_all),-1)
  #2018/12/20添加(对于样本少数据使用同品牌同级别车代替；若仍不够使用品牌代替)
  if(nrow(input_orig)<500){
    series_list<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_major_info
                                                        WHERE yck_brandid='",as.character(select_input_transfor$yck_brandid),
                                                        "' AND car_level='",as.character(select_input_transfor$select_car_level),"'")),-1)
    input_orig<-dbFetch(dbSendQuery(loc_channel,paste0(part_select,"yck_seriesid IN('",paste0(as.character(series_list$yck_seriesid),collapse = "','"),"')")),-1)
  }
  #2018/12/20-end
  dbDisconnect(loc_channel)
  #input_analysis<-input_orig%>%dplyr::filter(car_platform!='czb',car_platform!='souche')
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
##############fun_factor_transform用于将一些变量转化为因子变量
##20180117--14:00修改model_price为数值型
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
  ####20180117修改（价格只有一个） 
  # if(length(unique(input_value$model_price))==1){
  #   input_value$model_price<-factor(input_value$model_price,levels = c(unique(input_value$model_price),unique(input_value$model_price)-0.01))
  # }else{
  #   input_value$model_price<-factor(input_value$model_price,levels = unique(input_value$model_price))
  # }
  #20171214修改#
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
##############本函数为训练模型：输入ana1，输出模型结构
fun_model_train<-function(input_train,price_model_loc,model_code){
  ##模型训练
  model.svm <- e1071::svm(quotes~., input_train,gamma=0.02) 
  #preds <- predict(model.svm, input_train)
  input_train<-input_train %>% dplyr::select(car_platform,quotes)
  save(model.svm,file=paste0(price_model_loc,"\\model_net\\",model_code,".RData"))
  save(input_train,file=paste0(price_model_loc,"\\model_net\\",model_code,"input_train.RData"))
  return(model.svm)
}
##############本函数为测试模型：输入--测试数据-模型结构，输出结果##输出由综合输出一个值变为输出两个值（拍卖/发布）
fun_model_test<-function(select_input_transfor,input_test,model.svm){
  ##模型预测
  pre_preds <- predict(model.svm, input_test) 
  test_output<-data.frame(input_test,pre_preds)
  test_output$pre_preds<-round(test_output$model_price*test_output$pre_preds,3) #将折扣率转换为残值
  #
  # train_inout<-data.frame(quotes=model.svm$residuals+model.svm$fitted,fitted=model.svm$fitted,residuals=model.svm$residuals)
  # n<-nrow(train_inout)
  # result_temp<-data.frame(residuals=train_inout$residuals) %>% top_n(n-floor(n*0.05),residuals) %>% top_n(floor(n*0.1)-n,residuals)
  # result_temp<-data.frame(r_cor=paste0(round(100*cor(train_inout$quotes,train_inout$fitted),1),'%'),
  #                         r_residuals=round(mean(abs(result_temp$residuals)),4),r_median=round(median(abs(result_temp$residuals)),4))
  #配置平台权重表
  config_platform<-data.frame(car_platform=c("guazi","rrc","yiche","che168","youxin","che58","souche","czb","csp"),
                              car_platform_class=c("fb","fb","fb","fb","fb","fb","fb","pm","pm"),
                              platform_weight=c(0.25,0.2,0.15,0.1,0.1,0.15,0.05,0.5,0.5))
  test_output$car_platform<-as.character(test_output$car_platform)
  config_platform$car_platform<-as.character(config_platform$car_platform)
  output_final<-inner_join(config_platform,test_output,by="car_platform")%>%
    dplyr::mutate(price=platform_weight*pre_preds)%>%group_by(province,car_platform_class)%>%dplyr::summarise(preds=sum(price))%>%
    dcast(province~car_platform_class)
  data.frame(select_input_transfor,output_final)
}
##---****提取同级别车的对比参数***---##
fun_pred_compare_line<-function(select_input){
  sql_config<-paste0("SELECT platform_class car_platform_class,parmeter_y,parmeter_m,parmeter_v FROM config_quotes_class
                     WHERE car_level= (SELECT car_level FROM config_vdatabase_yck_major_info WHERE model_id =",select_input$select_model_id,")
                     AND auto= (SELECT auto FROM config_vdatabase_yck_major_info WHERE model_id =",select_input$select_model_id,")",sep='')
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  return_config_reg<-dbFetch(dbSendQuery(loc_channel,sql_config),-1)
  if(nrow(return_config_reg)>0){
    return_config_reg<-return_config_reg
  }else{
    return_config_reg<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT platform_class car_platform_class,parmeter_y,parmeter_m,parmeter_v FROM config_quotes_class
                                                       WHERE car_level= '全级别'
                                                       AND auto= (SELECT auto FROM config_vdatabase_yck_major_info WHERE model_id =",car_id,")",sep='')),-1)
  }
  dbDisconnect(loc_channel)
  select_year<-round((as.Date(select_input$select_partition_month)-as.Date(select_input$select_regDate))/365,2)%>%as.character()%>%as.numeric()
  select_mile<-select_input$select_mile
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
############此函数为最终预测调用函数
fun_pred<-function(select_input){
  select_input_transfor<-fun_select_transfor(select_input)
  case<-fun_parameter_ym(select_input_transfor$select_partition_month,select_input_transfor$select_regDate)$case
  model_code<-paste0(select_input_transfor$yck_seriesid,"T",paste0(format(as.Date(Sys.Date()),"%Y"),week(Sys.Date())),"CASE",case,sep="")%>%toupper()
  #模型高效处理方法
  input_test<-fun_input_test(select_input_transfor)
  list_model<-list.files(paste0(price_model_loc,"\\model_net"), full.names = T,pattern = "RData")
  list_model<-gsub(".*model_net\\/|.RData","",list_model)
  if(length(grep(model_code,list_model))==0){
    input_analysis<-fun_input(select_input_transfor)
    if(nrow(input_analysis)<10){
      print(paste0(select_input_transfor$select_model_name,"车辆信息太少"))
      output_pre<-NULL
    }else{
      input_train<-fun_input_train(input_analysis,select_input_transfor)
      model.svm<-fun_model_train(input_train,price_model_loc,model_code)
      output_pre<-fun_model_test(select_input_transfor,input_test,model.svm)
      return_pred_out<-fun_pred_out(input_train,model.svm,select_input)
      output_pre<-data.frame(output_pre,return_pred_out)
    }
  } else
  {
    load(paste0(paste0(price_model_loc,"\\model_net"),"\\",model_code,".RData"))
    load(paste0(paste0(price_model_loc,"\\model_net"),"\\",model_code,"input_train.RData"))
    output_pre<-fun_model_test(select_input_transfor,input_test,model.svm)
    return_pred_out<-fun_pred_out(input_train,model.svm,select_input)
    output_pre<-data.frame(output_pre,return_pred_out)
  }
  return(list(output_pre=output_pre))
}
###########此函数为最终预测调用函数(与业务估值对接-循环)
fun_pred_round<-function(i){
  select_input<-select_input_org[i,]
  output_pre<-fun_pred(select_input)
  return(output_pre)
}

##############********第二部分：模型相关输出**********################
###本函数为查询输出相关信息###
main_fun_relation_output<-function(id_che300){
  #id_che300<-c(1150836,33014)
  #第一部分：获取最新款型折扣率信息
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
                     INNER JOIN config_vdatabase_yck_major_info b ON a.model_id=b.model_id WHERE b.model_id in (",paste0(id_che300,sep='',collapse=','),")",sep='')
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
                  INNER JOIN config_vdatabase_yck_major_info c300 ON a.id_che300=c300.model_id
                  WHERE id_che300 in (",paste0(id_che300,sep='',collapse=','),")",sep='')
  sql_xl2<-paste0("SELECT model_id,a.series_name,stat_date,salesNum FROM config_vdatabase_yck_major_info a 
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
#######################对标车######V2.0版本对输入数据集进行修正，采取每个系列车仅选取一个代表车款########
main_fun_series_standard<-function(car_id){
  #读取车型配置数据
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  test<-dbFetch(dbSendQuery(loc_channel,"SELECT a.model_id,brand_name,series_name,model_name,model_price,model_year,car_level,auto,
  liter,'' liter_type,discharge_standard,ba_lwh,ba_engine,ba_gearbox,ba_structure,ee_max_mileage,sf_keyless_go,brk_front_tire_specs,
  ch_4WD_type,oeq_electric_trunk,oeq_aluminum_alloy_wheel,oeq_power_sunroof,oeq_panoramic_sunroof,ieq_reverse_radar,
  ieq_multi_function_steering_wheel,st_driver_seat_electric_adjust,mm_bluetooth_carphone,tec_auto_start_stop,tec_panoramic_camera,
  mm_central_console_color_screen,lt_hid,lt_auto_head_light,lt_led_head_light FROM config_vdatabase_yck_major_info a 
                            INNER JOIN config_che300_detail_info b ON a.model_id=b.model_id;"),-1)
  dbDisconnect(loc_channel)
  col_name<-c("model_id","brand_name","series_name","model_name","model_price","model_year","car_level","auto","liter","liter_type","discharge_standard",
              "ba_lwh","ba_engine","ba_gearbox","ba_structure","ee_max_mileage",
              "sf_keyless_go","brk_front_tire_specs","ch_4WD_type","oeq_electric_trunk",
              "oeq_aluminum_alloy_wheel","oeq_power_sunroof","oeq_panoramic_sunroof","ieq_reverse_radar",
              "ieq_multi_function_steering_wheel","st_driver_seat_electric_adjust","mm_bluetooth_carphone",
              "tec_auto_start_stop","tec_panoramic_camera","mm_central_console_color_screen","lt_hid","lt_auto_head_light",
              "lt_led_head_light")
  ###剔除的变量"st_seats_material"
  ###########################对标：第一部分：数据清洗############################
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
  #变量转换
  test$model_price<-as.numeric(as.character(test$model_price))
  col_order<-c("model_year","liter","discharge_standard","ba_length","ba_weight","ba_height","ba_engine1",
               "ba_engine2","ba_gearbox","ba_struct1","ba_struct2","ee_max_mileage",
               "brk_front_tire_specs1","brk_front_tire_specs2","brk_front_tire_specs3")
  for (i in 1:length(col_order)) {
    test[,col_order[i]]<-factor(test[,col_order[i]],ordered = T)
  }
  
  ############################对标：第二部分：数据选取#############################
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
    ###########################对标：第二部分：算法计算####################################
    #距离变换
    aa_distance<-daisy(aa_tr,metric = "gower",type = list(logratio = 3))
    tsne_obj <- tryCatch( Rtsne(aa_distance, is_distance = TRUE,perplexity=15),
                          error=function(e){Rtsne(aa_distance, is_distance = TRUE,perplexity=8)},
                          finally={print("结果输出")})
    #tsne_obj <- Rtsne(aa_distance, is_distance = TRUE,perplexity=10)
    
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
    ###########################对标：第二部分：算法计算####################################
    #距离变换
    aa_distance<-daisy(aa_tr,metric = "gower",type = list(logratio = 3))
    ##tsne_obj <- Rtsne(aa_distance, is_distance = TRUE,perplexity=perp)
    
    #算法区：运行其中某一算法#
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
    
    #########################对标：第三部分：结果分析#############
    output_result<-sc%>%dplyr::filter(cc==sc$cc[which(sc$model_id==car_id)])%>%.[1:6]
    output_result2<-sc2%>%dplyr::filter(cc==sc2$cc[which(sc2$model_id==car_id)])%>%.[1:6]
    output<-rbind(output_result,output_result2)%>%dplyr::select(-cc)%>%unique()
    return(output)
  }
  output_error<-test[test$model_id==car_id,c('model_id','brand_name','series_name','model_name','model_price')]
  tryCatch(write.csv(car_match(car_id),paste0(price_model_loc,"\\output\\relation\\",car_id,".csv"),row.names = F),
           error=function(e){cat(write.csv(output_error,paste0(price_model_loc,"\\output\\relation\\",car_id,".csv"),row.names = F),conditionMessage(e),"\n\n")},
           finally={print("结果输出f")})
}
#综合输出###
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
  series_max<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT series_name,sum(count_s) cou FROM analysis_wide_table_cous a
                        INNER JOIN config_vdatabase_yck_series b ON a.yck_seriesid=b.yck_seriesid
                                                     WHERE series_name in (","'",paste0(car_match$series_name,collapse = "','",sep=''),"'",") GROUP BY series_name",sep='')),-1)
  series_max<-series_max%>%top_n(4,cou)%>%dplyr::filter(cou>1000)
  dbDisconnect(loc_channel)
  car_match<-car_match%>%dplyr::filter(series_name %in% series_max$series)
  
  
  ################第二部分：对应价格预测###############
  ###1.价格预测（当样本量可信度较低时调用对标车）##
  linshi<-fun_pred(select_input)
  output_pre<-linshi[[1]]
  result_tag<-dplyr::summarise(group_by(output_pre,select_model_id,select_model_name,select_model_price,select_regDate,
                                        select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm))%>%
    ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))
  
  ###2.市场范围数据规范性校验##
  sql_config<-paste0("SELECT platform_class car_platform_class,parmeter_y,parmeter_m,parmeter_v FROM config_quotes_class
                     WHERE car_level= (SELECT car_level FROM config_vdatabase_yck_major_info WHERE model_id =",car_id,")
                     AND auto= (SELECT auto FROM config_vdatabase_yck_major_info WHERE model_id =",car_id,")",sep='')
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  config_reg<-dbFetch(dbSendQuery(loc_channel,sql_config),-1)
  if(nrow(config_reg)>0){
    config_reg<-config_reg
  }else{
    config_reg<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT platform_class car_platform_class,parmeter_y,parmeter_m,parmeter_v FROM config_quotes_class
                                                       WHERE car_level= '全级别'
                                                       AND auto= (SELECT auto FROM config_vdatabase_yck_major_info WHERE model_id =",car_id,")",sep='')),-1)
  }
  dbDisconnect(loc_channel)
  select_year<-round((as.Date(result_tag$select_partition_month)-as.Date(result_tag$select_regDate))/365,2)%>%as.character()%>%as.numeric()
  select_mile<-result_tag$select_mile
  config_reg<-config_reg%>%dplyr::mutate(ss=parmeter_v+select_year*parmeter_y+select_mile*parmeter_m)%>%dcast(.~car_platform_class)%>%.[,-1]
  names(config_reg)[names(config_reg) == "fb"] = c("fb_index")
  names(config_reg)[names(config_reg) == "pm"] = c("pm_index")
  result_tag<-data.frame(result_tag,config_reg)
  
  
  ################第三部分：对标车型价格预测###############
  ###3.当样本量过少启动(对标车型误差)###
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
#######综合输出---策略调整（对于冷门车）
main_fun_price_model<-function(select_input){
  ##########数据输入
  ###########加载自定义函数###########paste0(price_model_loc,"\\function")
  
  ###模型链条完善###
  #select_input<-select_input[1:1,]
  linshi<-fun_pred(select_input)
  return(linshi)
}
##综合输出（产品）##
model_main<-function(select_input){
  car_id<-select_input$select_model_id
  ################model_main:第一部分：对标车型###############
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
  ####去掉量少的车id
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  series_max<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT series_name,sum(count_s) cou FROM analysis_wide_table_cous a
                                INNER JOIN config_vdatabase_yck_series b ON a.yck_seriesid=b.yck_seriesid
                                                     WHERE series_name in (","'",paste0(car_match$series_name,collapse = "','",sep=''),"'",") GROUP BY series_name",sep='')),-1)
  series_max<-series_max%>%top_n(4,cou)%>%dplyr::filter(cou>1000)
  dbDisconnect(loc_channel)
  car_match<-car_match%>%dplyr::filter(series_name %in% series_max$series)
  
  
  ################model_main:第二部分：对应价格预测###############
  ##1.价格预测（当样本量可信度较低时调用对标车）#
  linshi<-fun_pred(select_input)
  output_pre<-linshi[[1]]%>%dplyr::mutate(query_lab="T")
  result_tag<-dplyr::summarise(group_by(output_pre,user_query_id,query_lab,select_model_id,select_model_name,select_model_price,select_regDate,
                                        select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm),
                               fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
    ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))
  
  ################model_main:第三部分：对标车型价格预测###############
  #3.当样本量过少启动(对标车型误差)#
  #acount_sample<-sum(output_sample$sample_size[which(output_sample$car_platform %in% c("czb","csp"))])
  ##输出处理（对标车型）
  if(nrow(car_match)>0){
    select_input_db<-data.frame(user_query_id=select_input$user_query_id,select_model_id=car_match$model_id,select_input[,-c(1:2)])
    match_output_pre<-NULL
    for (i in 1:nrow(select_input_db)) {
      linshi<-fun_pred(select_input_db[i,])
      linshi1<-linshi[[1]]%>%dplyr::mutate(query_lab="F")
      match_output_pre<-rbind(match_output_pre,linshi1)
    }
    result_compare<-dplyr::summarise(group_by(match_output_pre,user_query_id,query_lab,select_model_id,select_model_name,select_model_price,
                                              select_regDate,select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm),
                                     fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%ungroup()%>%as.data.frame()%>%
      dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3),sum_size=fb_n+pm_n)%>%
      top_n(3,sum_size)%>%dplyr::filter(sum_size>2000)%>%dplyr::select(-sum_size)%>%as.data.frame()
  }else{result_compare<-NULL}
  result_compare<-rbind(result_compare,result_tag)
  result_compare$fb_index<-cut(abs(result_compare$fb_monitor-result_compare$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_compare$pm_index<-cut(abs(result_compare$pm_monitor-result_compare$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_compare<-result_compare%>%dplyr::mutate(recep_price=pm_price*0.94-0.25)
  ##输出处理（标的车型）
  result_bd<-dplyr::summarise(group_by(output_pre,user_query_id,select_model_name,select_model_price,select_regDate,
                                       select_partition_month,select_mile,province),fb_price=mean(fb),pm_price=mean(pm),
                              fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
    ungroup()%>%as.data.frame()%>%dplyr::mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))
  result_bd$fb_index<-cut(abs(result_bd$fb_monitor-result_bd$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_bd$pm_index<-cut(abs(result_bd$pm_monitor-result_bd$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
  result_bd<-result_bd%>%dplyr::mutate(recep_price=pm_price*0.94-0.25)
  #按照格式输出
  result_bd<-result_bd %>% dplyr::select(user_query_id,select_model_name,select_model_price,select_regDate,select_partition_month,
                                         select_mile,province,fb_price,pm_price,fb_monitor,pm_monitor,fb_index,pm_index,recep_price)
  result_compare<-result_compare %>% dplyr::select(select_model_id,user_query_id,query_lab,select_model_name,select_model_price,select_regDate,
                                                   select_partition_month,select_mile,fb_price,pm_price,fb_monitor,pm_monitor,fb_index,pm_index,recep_price)
  
  #********输出未来五个月预测值********##
  select_input_future<-NULL
  for (i in 1:6) {
    linshi_input<-select_input
    linshi_input$select_partition_month<-as.character(as.Date(select_input$select_partition_month)+31*(i-1))
    select_input_future<-rbind(select_input_future,linshi_input)
  }
  select_input_future$select_mile<-select_input$select_mile/as.numeric(round(difftime(as_datetime(select_input$select_partition_month),as_datetime(select_input$select_regDate),units="days")/365,2))*
    as.numeric(round(difftime(as_datetime(select_input_future$select_partition_month),as_datetime(select_input_future$select_regDate),units="days")/365,2))
  
  match_output_future<-NULL
  for (i in 1:nrow(select_input_future)) {
    linshi<-fun_pred(select_input_future[i,])
    linshi1<-linshi[[1]]
    match_output_future<-rbind(match_output_future,linshi1)
  }
  result_future<-dplyr::summarise(group_by(match_output_future,user_query_id,select_partition_month),fb_price=round(mean(fb),2))%>%
    ungroup()%>%as.data.frame()%>%dcast(user_query_id~select_partition_month)
  names(result_future)<-c("user_query_id","now_price","future1","future2","future3","future4","future5")
  #********输出未来五个月预测值********#
  #存入数据库
  write.csv(result_bd,paste0(price_model_loc,"/output/result/yck_project_model_result_tag",".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  write.csv(result_compare,paste0(price_model_loc,"/output/result/yck_project_model_result_match",".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  write.csv(result_future,paste0(price_model_loc,"/output/result/yck_project_model_result_future",".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_result_match",".csv"),"'",
                                 " INTO TABLE yck_project_model_result_match CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_result_tag",".csv"),"'",
                                 " INTO TABLE yck_project_model_result_tag CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  dbSendQuery(loc_channel,paste0("LOAD DATA LOCAL INFILE ","'",paste0(price_model_loc,"/output/result/yck_project_model_result_future",".csv"),"'",
                                 " INTO TABLE yck_project_model_result_future CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  dbSendQuery(loc_channel,paste0("UPDATE yck_project_model_query SET query_statue=2 WHERE user_query_id=",unique(result_compare$user_query_id),";"))
  dbDisconnect(loc_channel)
  return(1)
}


##############********第三部分：模型离线训练函数**********################
#fun_model_input模型使用数据选取#
outline_all_fun_input<-function(){
  part_select<-c("SELECT car_platform,model_year,brand,series,yck_seriesid,auto,regDate,quotes,model_price,mile,province,user_years,a.partition_month FROM analysis_wide_table a")
  #测试离线代码
  #part_select<-c("SELECT car_platform,model_year,brand,series,yck_seriesid,auto,regDate,quotes,model_price,mile,province,user_years,a.partition_month FROM analysis_wide_table a where series='凯越'")
  ######数据库查询
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  input_orig<-dbFetch(dbSendQuery(loc_channel,part_select),-1)
  dbDisconnect(loc_channel)
  
  #数据处理#
  input_orig<-data.frame(input_orig,quotes_p=round(input_orig$quotes/input_orig$model_price,2))
  input_orig<-input_orig%>%dplyr::filter(quotes_p>0.05&quotes_p<0.95)
  input_orig<-input_orig%>%dplyr::filter(user_years<1|quotes_p<1)
  input_orig<-input_orig%>%dplyr::filter(regDate>'2000-01-01' & regDate!='NA')
  input_orig<-data.frame(input_orig,reg_year=str_sub(input_orig$regDate,1,4),
                         reg_month=str_sub(input_orig$regDate,6,7),
                         parti_year=str_sub(input_orig$partition_month,1,4),
                         parti_month=str_sub(input_orig$partition_month,5,6))
  return(input_orig)
}
##fun_model_input模型使用数据选取##
outline_series_fun_input<-function(select_input_transfor){
  part_value<-as.character(select_input_transfor$yck_seriesid)
  input_analysis<-input_orig%>%dplyr::filter(yck_seriesid==part_value)
  if(nrow(input_analysis)<500){
    loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
    dbSendQuery(loc_channel,'SET NAMES gbk')
    series_list<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT DISTINCT yck_seriesid FROM config_vdatabase_yck_major_info
                                                        WHERE yck_brandid='",as.character(select_input_transfor$yck_brandid),
                                                        "' AND car_level='",as.character(select_input_transfor$select_car_level),"'")),-1)
    dbDisconnect(loc_channel)
    input_analysis<-input_orig%>%dplyr::filter(yck_seriesid %in% series_list$yck_seriesid)
  }
  return(input_analysis)
}
############此函数为最终预测调用函数(离线训练)
outline_series_fun_pred<-function(i){
  select_input<-select_input_org[i,]
  select_input_transfor<-fun_select_transfor(select_input)
  case<-fun_parameter_ym(select_input_transfor$select_partition_month,select_input_transfor$select_regDate)$case
  model_code<-paste0(select_input_transfor$yck_seriesid,"T",paste0(format(as.Date(Sys.Date()+1),"%Y"),week(Sys.Date()+1)),"CASE",case,sep="")%>%toupper()
  #模型高效处理方法
  input_test<-fun_input_test(select_input_transfor)
  list_model<-list.files(paste0(price_model_loc,"\\model_net"), full.names = T,pattern = "RData")
  list_model<-gsub(".*model_net\\/|.RData","",list_model)
  if(length(grep(model_code,list_model))==0){
    input_analysis<-outline_series_fun_input(select_input_transfor)
    if(nrow(input_analysis)<10){
      print(paste0(select_input_transfor$select_model_name,"车辆信息太少"))
      return(list(output_pre=NULL))
    }else{
      #20180720修改#
      input_train<-fun_input_train(input_analysis,select_input_transfor)
      model.svm<-fun_model_train(input_train,price_model_loc,model_code)
      output_pre<-data.frame(a=1)
      return(list(output_pre=output_pre))
    }
  } else
  {
    output_pre<-data.frame(a=2)
    return(list(output_pre=output_pre))
  }
}