############此函数为最终预测调用函数
fun_pred_round<-function(i){
  output_pre<-tryCatch(
    {fun_pred(select_input_org[i,])[[1]]}, 
    warning = function(w) {fun_pred(select_input_org[i,])[[1]]}, 
    error = function(e) {cat("错误:",i)})
  return(list(output_pre=output_pre))
}
