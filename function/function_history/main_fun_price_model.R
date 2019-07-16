#######策略调整（对于冷门车）
main_fun_price_model<-function(select_input){
  ##########数据输入
  ###########加载自定义函数###########paste0(price_model_loc,"\\function")

  ############################模型链条完善##############################
  #select_input<-select_input[1:1,]
  linshi<-fun_pred(select_input)
  return(linshi)
}