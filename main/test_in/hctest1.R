rm(list = ls(all=T))
gc()
library(reshape2)
library(dplyr)
library(RMySQL)
library(stringr)
library(e1071) 
library(tcltk)
library(lubridate)
library(parallel)
library(rlist)
price_model_loc<-gsub("\\/main|\\/bat|\\/test_in","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"/function/fun_model_price_test.R"),echo=FALSE,encoding="utf-8")
source(paste0(price_model_loc,"/function/yck_base_function.R"),echo=FALSE,encoding="utf-8")
local_defin<-fun_mysql_config_up()

loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
config_level_quote<-dbFetch(dbSendQuery(loc_channel,"SELECT * FROM config_quotes_class where car_level='全级别'"),-1) %>% dplyr::select(-car_level)
select_input_org<-dbFetch(dbSendQuery(loc_channel,"SELECT car_platform,id_che300,model_year,yck_seriesid,is_import,is_green,
 brand,series,model_name,color,auto,discharge_standard,car_level,location,regDate,quotes,model_price,mile,update_time,province,
                                      car_country,user_years FROM analysis_wide_table"),-1)
dbDisconnect(loc_channel)
# select_input_org<-select_input_org %>% dplyr::filter(user_years>0&user_years<20&mile<60&round(quotes/model_price,2)>0.05&round(quotes/model_price,2)<1.01)
# nrow_2<-nrow(select_input_org)
# select_input_org1<-select_input_org[1:1000,]
# select_input_org$car_platform<-ifelse(select_input_org$car_platform%in%c('csp','czb'),'pm','fb')
# select_input_org<-inner_join(select_input_org,config_level_quote,by=c('car_platform'='platform_class','auto'='auto'))
# select_input_org<-select_input_org %>% dplyr::mutate(bd_quotes_p=parmeter_v+as.numeric(user_years)*parmeter_y+as.numeric(mile)*parmeter_m,quotes_p=round(quotes/model_price,2))
# temp_a<-select_input_org %>% dplyr::filter(quotes_p-bd_quotes_p>0.2)

loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
dbSendQuery(loc_channel,'SET NAMES gbk')
input_orig<-dbFetch(dbSendQuery(loc_channel,"SELECT car_platform,brand,series,color,location,car_level,regDate,quotes,model_price,
                              mile,province,car_country,user_years,a.partition_month,auto
                                  FROM analysis_wide_table a;"),-1)
dbDisconnect(loc_channel)

input_orig<-input_orig %>% dplyr::filter(user_years>0&user_years<20&mile<60&round(quotes/model_price,2)>0.05&round(quotes/model_price,2)<1.01)
input_orig<-data.frame(input_orig,quotes_p=round(input_orig$quotes/input_orig$model_price,2))
input_orig<-input_orig%>%filter(quotes_p>0.05&quotes_p<0.99)
input_orig<-input_orig%>%dplyr::select(-color,-location)

########_____版本1：取不同平台的前20个车系________###循环计算不同平台不同车系、不同制动方式##
weight_calculate<-function(input_orig1,auto_p,car_level_p){
  a<-input_orig1%>%group_by(car_platform,series)%>%summarise(Freq=n())%>%
    top_n(20,Freq)%>%as.data.frame()%>%mutate(id=paste0(car_platform,series))%>%dplyr::select(id,Freq)
  names(a)<-c("id","Freq")
  a$id<-as.character(a$id)
  input_orig1<-merge(input_orig1,a,by="id")
  
  turn_id<-input_orig1[,c("car_platform","series")]%>%unique()
  cun<-NULL
  for (i in 1:nrow(turn_id)) {
    want_out<-input_orig1[which(input_orig1$car_platform==turn_id[i,1]&input_orig1$series==turn_id[i,2]),]
    total_tab<-nrow(want_out)
    if(total_tab!=0){
      want_out<-lm(want_out$quotes_p~want_out$user_years+want_out$mile)
      cun_ls<-data.frame(car_platform=turn_id[i,1],series=turn_id[i,2],total_tab=total_tab,m_residuals=mean(abs(want_out$residuals)),
                         value=want_out$coefficients[1],parmeter_y=want_out$coefficients[2],
                         parmeter_m=want_out$coefficients[3],row.names = F)
      cun<-rbind(cun,cun_ls)
    }
    else{
      print(paste0(turn_id[i,1],turn_id[i,2]))
    }
  }
  #################基于线性模型拟合误差值计算平台及车系的权值#################
  re_fina<-cun%>%group_by(car_platform)%>%mutate(c_s_w=(1/m_residuals)/sum(1/m_residuals))%>%
    as.data.frame()
  
  c_weight<-cun%>%group_by(car_platform)%>%summarise(m_w=1/mean(m_residuals))%>%ungroup()%>%
    as.data.frame()%>%mutate(zb=ifelse(car_platform=="czb","pm",ifelse(car_platform=="csp","pm","fb")))%>%
    group_by(zb)%>%mutate(c_w=m_w/sum(m_w))%>%as.data.frame()%>%dplyr::select(-m_w)
  
  re_fina<-merge(re_fina,c_weight,by="car_platform")%>%group_by(zb)%>%
    summarise(parmeter_y=sum(c_s_w*c_w*parmeter_y),
              parmeter_m=sum(c_s_w*c_w*parmeter_m),
              value=sum(c_s_w*c_w*value))%>%as.data.frame()%>%mutate(auto_p,car_level_p)
  return(re_fina)
}
auto_p<-c("手动","自动")
car_level_p<-c("中型车","紧凑型车","中大型车","MPV","小型SUV",
               "紧凑型SUV","小型车","中型SUV","微型车","中大型SUV","豪华车")

#"客车" "跑车" "微面" "皮卡""全尺寸SUV" "卡车""其它""大型SUV"  

weight_re<-NULL
m<-0
for (i in car_level_p) {
  for (j in auto_p) {
    input_orig1<-input_orig%>%filter(user_years<10)%>%filter(user_years>0)%>%
      filter(partition_month>201707)%>%filter(auto==j)%>%filter(car_level==i)%>%
      mutate(id=paste0(car_platform,series))
    if(nrow(input_orig1)>1000){
      linshi<-weight_calculate(input_orig1,j,i)
      weight_re<-rbind(weight_re,linshi)
    }else{
      print(i)
    }
    m=m+1
    print(m)
  }
}
write.csv(weight_re,"weight_re.csv")


########_____版本2：取不同平台的前20个车系________###循环计算不同平台不同车系、不同制动方式##
########循环计算不同平台不同车系、不同制动方式####
weight_calculate<-function(input_orig1,auto_p,car_level_p){
  a<-as.data.frame(table(input_orig1$series))%>%top_n(20,Freq)
  names(a)<-c("series","Freq")
  a$series<-as.character(a$series)
  input_orig1<-merge(input_orig1,a,by="series")
  cun<-NULL
  for (i in unique(input_orig1$car_platform)) {
    for (j in unique(input_orig1$series)) {
      want_out<-input_orig1[which(input_orig1$car_platform==i&input_orig1$series==j),]
      total_tab<-nrow(want_out)
      if(total_tab!=0){
        want_out<-lm(want_out$quotes_p~want_out$user_years+want_out$mile)
        cun_ls<-data.frame(car_platform=i,series=j,total_tab=total_tab,m_residuals=mean(abs(want_out$residuals)),
                           value=want_out$coefficients[1],parmeter_y=want_out$coefficients[2],
                           parmeter_m=want_out$coefficients[3],row.names = F)
        cun<-rbind(cun,cun_ls)
      }
      else{
        print(paste0(i,j))
      }
    }
  }
  #################基于线性模型拟合误差值计算平台及车系的权值#################
  re_fina<-cun%>%group_by(car_platform)%>%mutate(c_s_w=(1/m_residuals)/sum(1/m_residuals))%>%
    as.data.frame()
  
  c_weight<-cun%>%group_by(car_platform)%>%summarise(m_w=1/mean(m_residuals))%>%ungroup()%>%
    as.data.frame()%>%mutate(zb=ifelse(car_platform=="czb","pm",ifelse(car_platform=="csp","pm","fb")))%>%
    group_by(zb)%>%mutate(c_w=m_w/sum(m_w))%>%as.data.frame()%>%select(-m_w)
  
  re_fina<-merge(re_fina,c_weight,by="car_platform")%>%group_by(zb)%>%
    summarise(parmeter_y=sum(c_s_w*c_w*parmeter_y),
              parmeter_m=sum(c_s_w*c_w*parmeter_m),
              value=sum(c_s_w*c_w*value))%>%as.data.frame()%>%mutate(auto_p,car_level_p)
  return(re_fina)
}
auto_p<-c("手动","自动")
car_level_p<-c("中型车","紧凑型车","中大型车","MPV","小型SUV",
               "紧凑型SUV","小型车","中型SUV","微型车","中大型SUV","豪华车")
weight_re<-NULL
m<-0
for (i in car_level_p) {
  for (j in auto_p) {
    input_orig1<-input_orig%>%filter(user_years<10)%>%filter(user_years>0)%>%
      filter(partition_month>201707)%>%filter(auto==j)%>%filter(car_level==i)
    if(nrow(input_orig1)>1000){
      linshi<-weight_calculate(input_orig1,j,i)
      weight_re<-rbind(weight_re,linshi)
    }else{
      print(i)
    }
    m=m+1
    print(m)
  }
}