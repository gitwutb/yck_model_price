#####历史估值列表
interface_querylist_history<-function(parameter_user_id){
  loc_channel<-dbConnect(MySQL(),user = local_defin$user,host=local_defin$host,password= local_defin$password,dbname=local_defin$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  interface_querylist_history_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_classification_car,che300.model_name select_model_name,select_regDate,select_classification_operational,add_time,query_statue,user_query_id
  FROM config_vdatabase_yck_major_info che300
  INNER JOIN yck_project_model_query yck_q ON che300.model_id=yck_q.select_model_id 
    WHERE yck_q.user_id=",parameter_user_id," ORDER BY add_time DESC")),-1)
  dbDisconnect(loc_channel)
  if(nrow(interface_querylist_history_return)==0){
    interface_querylist_history_return<-interface_querylist_history_return
  }else{
    interface_querylist_history_return<-data.frame(order_number=c(1:nrow(interface_querylist_history_return)),interface_querylist_history_return)%>%
      dplyr::mutate(query_statue_m=query_statue)
    }
  interface_querylist_history_return<-RJSONIO::toJSON(unname(plyr::alply(interface_querylist_history_return, 1, identity)))
  return(interface_querylist_history_return)
}