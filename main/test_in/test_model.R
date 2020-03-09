rm(list = ls(all=T))
gc()
library(parallel)
price_model_loc<-gsub("(\\/main|\\/bat).*","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")

####-----第一部分：估值源数据-----####
#select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst.csv"),header = T)
#IT现车估值
select_input<-fun_mysqlload_query(local_defin,paste0("SELECT select_model_id,select_regDate,select_mile,select_partition_month,yck_query_id  
                                                     FROM yck_it_query_mresult WHERE yck_query_id in (",paste0(c(85716:85724),collapse = ','),')'))%>%
  dplyr::select(select_model_id,select_regDate,select_mile,select_partition_month,yck_query_id)
#估值产品
select_input<-fun_mysqlload_query(local_defin,paste0("SELECT select_model_id,select_regDate,select_mile,select_partition_month,user_query_id 
                                                     FROM yck_project_model_query WHERE user_query_id=",1039))

####-----第二部分：估值应用-----####
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
cl<-makeCluster(4)
clusterExport(cl,c("select_input_org","price_model_loc","local_defin","childFun_pred_round"))
clusterEvalQ(cl,c(source(paste0(price_model_loc,"\\main\\model_interface.R"),echo=FALSE,encoding="utf-8")))
result_tag<-parLapply(cl,x,childFun_pred_round)
stopCluster(cl)
result_tag1<-list.rbind(result_tag)
Sys.time()-t1


####-----第三部分：估值结果计算-----####
result_tag<-result_tag1 %>% dplyr::group_by(yck_query_id,select_model_name,select_model_price,select_regDate,
                                            select_partition_month,select_mile) %>% dplyr::summarise(fb_price=mean(fb),pm_price=mean(pm),
                                                                                                     fb_n=mean(fb_n),pm_n=mean(pm_n),fb_index=mean(fb_index),pm_index=mean(pm_index))%>%
  ungroup()%>%as.data.frame()%>%mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3))
result_tag$fb_index<-cut(abs(result_tag$fb_monitor-result_tag$fb_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))
result_tag$pm_index<-cut(abs(result_tag$pm_monitor-result_tag$pm_index),c(-0.001,0.06,0.1,0.15,1),labels=c('高','基本','不可信','错误'))

###-----第四部分：接口启动-----####
eval_task_pred16<-function(i){
  output_pre<-tryCatch({fun_pred(eval_task_select_input[i,])},
                       error=function(e){write.table(paste0(paste0(select_input_i,collapse = '|'),as.character(e)),paste0(price_model_loc,'/Log/tasklog_modelTrainDaily.txt'),append = T, quote = TRUE, sep = " ", row.names = F,col.names = F)},
                       finally={NULL})
  output_pre<-output_pre %>% dplyr::group_by(select_model_name,select_brand,select_series,select_model_year,select_model_name,
                                             select_model_price,select_auto,select_regDate,select_partition_month,select_mile) %>% 
    dplyr::summarise(fb_price=mean(fb),pm_price=mean(pm),fb_n=mean(fb_n),pm_n=mean(pm_n))%>%
    ungroup()%>%as.data.frame()%>%mutate(fb_monitor=round(fb_price/select_model_price,3),pm_monitor=round(pm_price/select_model_price,3),add_time=as.character(Sys.time()))
  fun_mysqlload_add(price_model_loc,local_defin,output_pre,paste0('eval_task_select_output'),as.numeric(Sys.time()))
}
pr <- plumber::plumb("E:/Work_table/gitwutb/git_project/yck_model_price/function/else_fun/plumber_api.R")
pr$run(host = "0.0.0.0", port = 4267)


###-----第五部分：画图-----####
ggplot2::ggplot(data=input_train_one, mapping=ggplot2::aes(x=user_years, y=quotes_p))+ggplot2::geom_point(ggplot2::aes(color=input_analysis$car_platform),size=3)
ggplot2::ggplot(data=input_train, mapping=ggplot2::aes(x=user_years, y=quotes))+ggplot2::geom_point(ggplot2::aes(color=input_train$car_platform),size=3)
ggplot2::ggplot(data=input_train_one, mapping=ggplot2::aes(x=user_years, y=quotes_p))+
  ggplot2::geom_point(ggplot2::aes(color=input_train_one$province))
ggplot2::ggplot(data=test_output, mapping=ggplot2::aes(x=user_years, y=pre_preds))+ggplot2::geom_point(ggplot2::aes(color=test_output$car_platform),size=3)