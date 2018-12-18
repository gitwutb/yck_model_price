#####历史估值列表
interface_querylist_history<-function(parameter_user_id){
  options(warn =-1)
  library(reshape2)
  library(dplyr)
  library(RMySQL)
  library(RJSONIO)
  library(plyr)
  loc_channel<-dbConnect(MySQL(),user = "yckdc",host="172.18.215.178",password= "YckDC888",dbname="yck-data-center")
  dbSendQuery(loc_channel,'SET NAMES gbk')
  interface_querylist_history_return<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT select_classification_car,che300.model_name select_model_name,select_regDate,select_classification_operational,add_time,query_statue,user_query_id
  FROM config_che300_major_info che300
  INNER JOIN yck_project_model_query yck_q ON che300.model_id=yck_q.select_model_id 
    WHERE yck_q.user_id=",parameter_user_id," ORDER BY add_time DESC")),-1)
  dbDisconnect(loc_channel)
  if(nrow(interface_querylist_history_return)==0){
    interface_querylist_history_return<-interface_querylist_history_return
  }else{
    interface_querylist_history_return<-data.frame(order_number=c(1:nrow(interface_querylist_history_return)),interface_querylist_history_return)%>%
      dplyr::mutate(query_statue_m=query_statue)
    }
  interface_querylist_history_return<-toJSON(unname(alply(interface_querylist_history_return, 1, identity)))
  return(interface_querylist_history_return)
}