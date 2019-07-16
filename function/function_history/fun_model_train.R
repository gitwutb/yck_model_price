##############本函数为训练模型：输入ana1，输出模型结构
fun_model_train<-function(input_train,price_model_loc,model_code){
  ##模型训练
  model.svm <- e1071::svm(quotes~., input_train) 
  preds <- predict(model.svm, input_train)
  save(model.svm,file=paste0(price_model_loc,"\\model_net\\",model_code,".RData"))
  save(input_train,file=paste0(price_model_loc,"\\model_net\\",model_code,"input_train.RData"))
  return(model.svm)
}