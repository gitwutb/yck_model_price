# plumber.R

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @field logger
#' @get /yck_ygz
function(query1_interface){
  re_tt<-fun_pred(select_input[1,])
  list(msg =re_tt)
}
