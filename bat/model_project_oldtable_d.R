rm(list = ls(all=T))
gc()
library(dplyr)
library(RMySQL)
price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"/function/yck_base_function.R"),echo=FALSE,encoding="utf-8")
local_defin<-fun_mysql_config_up('10')
df_user_query_id<-fun_mysqlload_query(local_defin,'SELECT user_query_id FROM yck_project_model_query WHERE query_statue=3 OR DATEDIFF(NOW(),add_time)>6')
fun_mysqlload_query(local_defin,paste0('INSERT INTO yck_project_model_old_query SELECT * FROM yck_project_model_query WHERE user_query_id in(',paste0(df_user_query_id$user_query_id,collapse = ','),')'))
fun_mysqlload_query(local_defin,paste0('INSERT INTO yck_project_model_old_result_future SELECT * FROM yck_project_model_result_future WHERE user_query_id in(',paste0(df_user_query_id$user_query_id,collapse = ','),')'))
fun_mysqlload_query(local_defin,paste0('INSERT INTO yck_project_model_old_result_match SELECT * FROM yck_project_model_result_match WHERE user_query_id in(',paste0(df_user_query_id$user_query_id,collapse = ','),')'))
fun_mysqlload_query(local_defin,paste0('INSERT INTO yck_project_model_old_result_tag SELECT * FROM yck_project_model_result_tag WHERE user_query_id in(',paste0(df_user_query_id$user_query_id,collapse = ','),')'))
fun_mysqlload_query(local_defin,paste0('DELETE FROM yck_project_model_query WHERE user_query_id in(',paste0(df_user_query_id$user_query_id,collapse = ','),')'))
