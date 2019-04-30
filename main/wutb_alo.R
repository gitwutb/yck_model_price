##用于对不同算法测试
rm(list = ls(all=T))
gc()
library(reshape2)
library(dplyr)
library(ggplot2)
library(RMySQL)
library(stringr)
library(e1071) 
library(tcltk)
library(lubridate)
library(parallel)
library(rlist)
library(tidyr)
##########数据输入
###########加载自定义函数###########paste0(price_model_loc,"\\function")
price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
local_defin<-data.frame(user = 'root',host='192.168.0.111',password= '000000',dbname='yck-data-center',stringsAsFactors = F)
source(paste0(price_model_loc,"\\function\\fun_model_price_test.R"),echo=FALSE,encoding="utf-8")
############################模型链条完善##############################
#############################测试数据1：########################################
#select_input<-read.csv(paste0(price_model_loc,"\\file\\","select_inputst.csv"),header = T)
loc_channel<-dbConnect(MySQL(),user = "yckdc",host="47.106.189.86",password= "YckDC888",dbname="yck-data-center")
dbSendQuery(loc_channel,'SET NAMES gbk')
select_input<-dbFetch(dbSendQuery(loc_channel,paste0("SELECT * FROM yck_project_model_query WHERE user_query_id in (",paste0(184:184,collapse = ','),')')),-1)%>%
  dplyr::select(select_model_id,select_regDate,select_mile,select_partition_month)
dbDisconnect(loc_channel)

select_input_transfor<-fun_select_transfor(select_input)
case<-fun_parameter_ym(select_input_transfor$select_partition_month,select_input_transfor$select_regDate)$case
#模型高效处理方法
input_test<-fun_input_test(select_input_transfor)
#获取训练数据
input_analysis<-fun_input(select_input_transfor)
nrow(input_analysis)
input_train<-fun_input_train(input_analysis,select_input_transfor)
input_train<-input_train[sample(1:nrow(input_train)),]
nrow(input_train)

######模型测试
#########算法一：svm####
model_algorithm1<-function(parameter_gamma){
  t1<-Sys.time()
  model.svm <- e1071::svm(quotes~., input_train[1:round(nrow(input_train)*0.8,0),],gamma=parameter_gamma)
  train_output<-data.frame(input_train[1:round(nrow(input_train)*0.8,0),],pre_preds=model.svm$fitted)
  pre_preds <- predict(model.svm, input_train[(round(nrow(input_train)*0.8,0)+1):nrow(input_train),-4]) 
  test_output<-data.frame(input_train[(round(nrow(input_train)*0.8,0)+1):nrow(input_train),],pre_preds)
  test_output%>%group_by(car_platform)%>%summarise(mean(abs(pre_preds-quotes)))%>%as.data.frame()
  print(paste0(parameter_gamma,'误差',round(mean(abs(model.svm$residuals)),5),"cc",round(mean(abs(test_output$pre_preds-test_output$quotes)),5)))
  p<-data.frame(parameter_gamma=parameter_gamma,x1=round(mean(abs(model.svm$residuals)),5),x2=round(mean(abs(test_output$pre_preds-test_output$quotes)),5))
  print(Sys.time()-t1)
  return(p)
}
x<-seq(0.008,0.15,by=0.02)
cl<-makeCluster(4)
clusterExport(cl,c("model_algorithm1","input_train"))
clusterEvalQ(cl,c(library(reshape2),library(dplyr),library(RMySQL),library(stringr),
                  library(e1071) ,library(tcltk),library(lubridate)))
error_return<-tryCatch(
  {parLapply(cl,x,model_algorithm1)}, 
  warning = function(w) {"出警告啦"}, 
  error = function(e) { "出错啦"})
error_return<-list.rbind(error_return)
stopCluster(cl)
ggplot(data=error_return, mapping=aes(x=parameter_gamma, y=x1))+geom_line(aes(color='y'),size=1.5)+geom_point(aes(color='y'),size=3)+
  geom_line(aes(y=x2,color='red'),size=1.5)+geom_point(aes(y=x2,color='red'),size=3,alpha=1)


########算法二：nnet中的神经网络####
model_algorithm2<-function(parameter_maxit){
  t1<-Sys.time()
  model.svm <- nnet::nnet(quotes~., input_train[1:round(nrow(input_train)*0.8,0),], size = 5, decay = 0.01, maxit = parameter_maxit, linout = T, trace = F) 
  train_output<-data.frame(input_train[1:round(nrow(input_train)*0.8,0),],pre_preds=model.svm$fitted)
  pre_preds <- predict(model.svm, input_train[(round(nrow(input_train)*0.8,0)+1):nrow(input_train),-4]) 
  test_output<-data.frame(input_train[(round(nrow(input_train)*0.8,0)+1):nrow(input_train),],pre_preds)
  test_output%>%group_by(car_platform)%>%summarise(mean(abs(pre_preds-quotes)))%>%as.data.frame()
  print(paste0(parameter_maxit,'误差',round(mean(abs(model.svm$residuals)),5),"cc",round(mean(abs(test_output$pre_preds-test_output$quotes)),5)))
  p<-data.frame(parameter_maxit=parameter_maxit,x1=round(mean(abs(model.svm$residuals)),5),x2=round(mean(abs(test_output$pre_preds-test_output$quotes)),5))
  print(Sys.time()-t1)
  return(p)
}
error_return<-apply(array(seq(100,1000,by=50), c(1,length(seq(100,1000,by=50)))), 2, model_algorithm2)
error_return<-list.rbind(error_return)
ggplot(data=error_return, mapping=aes(x=parameter_maxit, y=x1))+geom_line(aes(color='y'),size=1.5)+geom_point(aes(color='y'),size=3)+
  geom_line(aes(y=x2,color='red'),size=1.5)+geom_point(aes(y=x2,color='red'),size=3,alpha=1)

########算法三：Ksvm####
model.svm <- ksvm(quotes~., input_train,type='eps-svr',knernel='rbfdot', kpar = "automatic",
                  C = 1, nu = 0.2, epsilon = 0.2) 
pre_preds <- predict(model.svm, input_train[,-4]) 
train_output2<-data.frame(input_train,pre_preds)
pre_preds <- predict(model.svm, input_test) 
test_output2<-data.frame(input_test,pre_preds)

########算法四：AMORE中的神经网络####
net <- newff(n.neurons=c(9,15,12,1), learning.rate.global=1e-2, momentum.global=0.5,
             error.criterium="LMLS", Stao=NA, hidden.layer="sigmoid", 
             output.layer="purelin", method="ADAPTgdwm")
aa<-matrix(unlist(input_train[,-4]),nrow=nrow(input_train))
result <- train(net, aa, input_train[,4], error.criterium="LMS", report=TRUE, show.step=100, n.shows=50 )
pre_preds <- sim(result$net, aa)
train_output3<-data.frame(input_train,pre_preds)
pre_preds <- sim(result$net, matrix(unlist(input_test),nrow=nrow(input_test)))
test_output3<-data.frame(input_test,pre_preds)

