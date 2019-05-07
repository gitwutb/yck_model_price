rm(list = ls(all=T))
gc()
price_model_loc<-gsub("\\/main","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")
select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst.csv"),header = T)
input_tra<-select_input %>% mutate(user_id=1,select_classification_operational='非营运',select_classification_car='现车',add_time=Sys.time())
for (i in 1:nrow(input_tra)) {
  model_interface_train(input_tra[i,])
}





output_pre<-NULL
output_sample<-NULL
for (i in 1:nrow(select_input)) {
  linshi<-fun_pred(select_input[i,])
  linshi1<-linshi[[1]]
  output_pre<-rbind(output_pre,linshi1)
}
result_tag<-summarise(group_by(output_pre,select_model_id,select_model_name,select_model_price,select_regDate,
                               select_partition_month,select_mile),fb_price=mean(fb),pm_price=mean(pm),
                      fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index),
                      r_cor=unique(r_cor),r_fb=mean(r_fb),r_pm=mean(r_pm))%>%
  ungroup()%>%as.data.frame()%>%mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))
result_tag$fb_index<-cut(abs(result_tag$fb_monitor-result_tag$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
result_tag$pm_index<-cut(abs(result_tag$pm_monitor-result_tag$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))