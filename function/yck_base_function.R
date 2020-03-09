###*******基础库******#######
##--数据库配置函数--调用##
fun_mysql_config_up<-function(para_definsql="local_defin"){
  if(para_definsql ==  '111'){
    return_sqllogin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
  }else if(para_definsql ==  '10'){
    return_sqllogin<-data.frame(user = 'root',host='192.168.0.10',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
  }else if(para_definsql ==  '47'){
    return_sqllogin<-data.frame(user = 'yckdc_dev',host='47.106.189.86',password= 'yRyiXqJ4WYHSBXP11hKs',dbname='yckdc_da2_ucar',stringsAsFactors = F)
  }else if(para_definsql ==  '47l'){
    return_sqllogin<-data.frame(user = "yckdc_dev",host="172.18.215.178",password= "yRyiXqJ4WYHSBXP11hKs",dbname="yckdc_da2_ucar",stringsAsFactors = F)
  }else if(para_definsql ==  'yck'){
    return_sqllogin<-data.frame(user = 'data',host='www.youcku.com',password= '6wrzfhG',dbname='yck',stringsAsFactors = F)
  }else if(para_definsql ==  'yckt'){
    return_sqllogin<-data.frame(user = "data",host="120.79.98.108",password= "543asdfQ",dbname="yck",stringsAsFactors = F)
  }else{
    return_sqllogin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
  }
  return(return_sqllogin)
}

#库1：邮箱发送(lzbzotpxumvrbgbi/zqnsxdrnfumtcadh)
fun_mailsend<-function(input_subject,input_body){
  mailR::send.mail(from = "270437211@qq.com",
            to = c("270437211@qq.com",'rozsa@youcku.com'),
            subject = input_subject,
            encoding = 'utf-8',
            body = input_body,
            html = TRUE,
            smtp = list(host.name = "smtp.qq.com",port = 465,user.name = "270437211@qq.com",passwd = "zqnsxdrnfumtcadh",ssl = TRUE,tls =TRUE),
            authenticate = TRUE,
            send = TRUE)
}
#库2:数据插入到表
fun_mysqlload_add<-function(input_path,input_ip,input_table,input_tablename,pare_uni=0){
  for (i in 1:dim(input_table)[2]) {
    input_table[,i][which(is.na(input_table[,i]))]<-"\\N"
  }
  write.csv(input_table,paste0(input_path,"/file/",input_tablename,pare_uni,".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  fun_mysqlload_query(input_ip,paste0("LOAD DATA LOCAL INFILE ","'",paste0(input_path,"/file/",input_tablename,pare_uni,".csv"),"'",
                                      " INTO TABLE ",input_tablename," CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  file.remove(paste0(input_path,"/file/",input_tablename,pare_uni,".csv"))
}

#库2.1:数据更新插入到表
fun_mysqlload_add_upd<-function(input_path,input_ip,input_table,input_tablename,pare_uni=0){
  for (i in 1:dim(input_table)[2]) {
    input_table[,i][which(is.na(input_table[,i]))]<-"\\N"
  }
  write.csv(input_table,paste0(input_path,"/file/",input_tablename,pare_uni,".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  fun_mysqlload_query(input_ip,paste0("LOAD DATA LOCAL INFILE ","'",paste0(input_path,"/file/",input_tablename,pare_uni,".csv"),"'",
                                      " REPLACE INTO TABLE ",input_tablename," CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  file.remove(paste0(input_path,"/file/",input_tablename,pare_uni,".csv"))
}

#库3：全量更新
fun_mysqlload_all<-function(input_path,input_ip,input_table,input_tablename,pare_uni=0){
  for (i in 1:dim(input_table)[2]) {
    input_table[,i][which(is.na(input_table[,i]))]<-"\\N"
    input_table[,i]<-gsub("\\,","，",input_table[,i])
    input_table[,i]<-gsub("\\\n","",input_table[,i])
  }
  write.csv(input_table,paste0(input_path,"/file/",input_tablename,pare_uni,".csv"),
            row.names = F,fileEncoding = "UTF-8",quote = F)
  fun_mysqlload_query(input_ip,paste0("TRUNCATE TABLE ",input_tablename))
  fun_mysqlload_query(input_ip,paste0("LOAD DATA LOCAL INFILE ","'",paste0(input_path,"/file/",input_tablename,pare_uni,".csv"),"'",
                                      " INTO TABLE ",input_tablename," CHARACTER SET utf8 FIELDS TERMINATED BY ',' lines terminated by '\r\n' IGNORE 1 LINES;"))
  file.remove(paste0(input_path,"/file/",input_tablename,pare_uni,".csv"))
}

#库4：查询代码
fun_mysqlload_query<-function(input_ip,input_sqlstr){
  loc_channel<-dbConnect(MySQL(),user = input_ip$user,host=input_ip$host,password= input_ip$password,dbname=input_ip$dbname)
  dbSendQuery(loc_channel,'SET NAMES gbk')
  query_result<-dbFetch(dbSendQuery(loc_channel,input_sqlstr),-1)
  dbDisconnect(loc_channel)
  return(query_result)
}