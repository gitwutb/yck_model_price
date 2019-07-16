##############本函数为测试模型：输入--测试数据-模型结构，输出结果##输出由综合输出一个值变为输出两个值（拍卖/发布）
fun_model_test<-function(select_input_transfor,input_test,model.svm){
  ##模型预测
  pre_preds <- predict(model.svm, input_test) 
  test_output<-data.frame(input_test,pre_preds)
  #配置平台权重表
  config_platform<-data.frame(car_platform=c("guazi","rrc","yiche","che168","youxin","che58","souche","czb","csp"),
                              car_platform_class=c("fb","fb","fb","fb","fb","fb","fb","pm","pm"),
                              platform_weight=c(0.25,0.2,0.15,0.1,0.1,0.15,0.05,0.5,0.5))
  # config_platform<-data.frame(car_platform=c("che168","che58","guazi","rrc","youxin","csp","yiche"),
  #                             platform_weight=c(0.2,0.15,0.15,0.1,0.15,0.05,0.2))
  test_output$car_platform<-as.character(test_output$car_platform)
  config_platform$car_platform<-as.character(config_platform$car_platform)
  output_final<-inner_join(config_platform,test_output,by="car_platform")%>%
    dplyr::mutate(price=platform_weight*pre_preds)%>%group_by(province,car_platform_class)%>%dplyr::summarise(preds=sum(price))%>%
    dcast(province~car_platform_class)
  data.frame(select_input_transfor,output_final)
}