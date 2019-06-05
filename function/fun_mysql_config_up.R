##--数据库配置函数--调用##
fun_mysql_config_up<-function(){
  local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
  return(local_defin)
}
# ##--数据库配置函数--调用##
# fun_mysql_config_up<-function(){
#   local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
#   local_defin_yun<-data.frame(user = 'yckdctab',host='47.106.189.86',password= 'YckDCtemp2019',dbname='yck-data-center',stringsAsFactors = F)
#   local_defin_yunl<-data.frame(user = "yckdctab",host="172.18.215.178",password= "YckDCtemp2019",dbname="yck-data-center",stringsAsFactors = F)
#   local_defin_yun_it<-data.frame(user = 'data',host='youcku.com',password= 'ibfIh28',dbname='yck',stringsAsFactors = F)
#   return(list(local_defin=local_defin,local_defin_yun=local_defin_yun,local_defin_yunl=local_defin_yunl,local_defin_yun_it=local_defin_yun_it))
# }