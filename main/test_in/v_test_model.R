rm(list = ls(all=T))
gc()
library(parallel)
price_model_loc<-gsub("(\\/main|\\/bat).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"/main/model_interface.R"),echo=FALSE,encoding="utf-8")

##第一部分：有多少报价在新版本失败
select_input<-fun_mysqlload_query(local_defin,paste0("SELECT select_model_id,select_regDate,select_mile,select_partition_month,yck_query_id,fb_price old_fb,pm_price old_pm
                                                     FROM yck_it_query_mresult where yck_query_id in(81860,98470)"))%>%
  dplyr::select(select_model_id,select_regDate,select_mile,select_partition_month,yck_query_id,old_fb,old_pm) %>% unique()
childFun_pred_round<-function(i){
  select_input_i<-select_input_org[i,]
  out_pred<-tryCatch(
    {fun_pred(select_input_i)},
    error = function(e) {NULL})
  return(out_pred=out_pred)
}
t1<-Sys.time()
select_input_org<-select_input
x<-1:nrow(select_input_org)
cl<-makeCluster(16)
clusterExport(cl,c("select_input_org","price_model_loc","local_defin","childFun_pred_round"))
clusterEvalQ(cl,c(source(paste0(price_model_loc,"/main/model_interface.R"),echo=FALSE,encoding="utf-8")))
result_tag<-parLapply(cl,x,childFun_pred_round)
stopCluster(cl)
result_tag1<-list.rbind(result_tag)
Sys.time()-t1
result_tag1<-result_tag1 %>% dplyr::group_by(yck_query_id,select_model_name,select_model_price,select_regDate,
                                            select_partition_month,select_mile) %>% 
  dplyr::summarise(fb_price=mean(fb),pm_price=mean(pm),fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
  ungroup()%>%as.data.frame()
id_setdiff<-setdiff(select_input$yck_query_id,result_tag1$yck_query_id)


##第二部分：车型覆盖率（所有车型估值一台车）
select_input<-fun_mysqlload_query(local_defin,paste0("SELECT model_id select_model_id,min_reg_year select_regDate FROM config_vdatabase_yck_major_info ")) %>% unique()
select_input$select_regDate<-paste0(select_input$select_regDate,'-01-01')
select_input$select_mile<-2.1
select_input$select_partition_month<-as.Date(select_input$select_regDate)+400

result_tag1<-result_tag1 %>% dplyr::group_by(select_model_id,select_brand,select_series,select_model_name,select_model_price,
                                             select_auto,is_green,select_regDate,select_partition_month,select_mile) %>% 
  dplyr::summarise(fb_price=mean(fb),pm_price=mean(pm),fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
  ungroup()%>%as.data.frame()

select_input_id<-select_input$select_model_id[!(select_input$select_model_id%in%result_tag1$select_model_id)]
select_input<-fun_mysqlload_query(local_defin,paste0("SELECT model_id select_model_id,series_name,brand_name 
                                                     FROM config_vdatabase_yck_major_info where model_id in",'(',paste0(select_input_id,collapse = ","),')'))

