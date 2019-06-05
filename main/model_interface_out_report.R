price_model_loc<-gsub("\\/main|\\/bat","",tryCatch(dirname(rstudioapi::getActiveDocumentContext()$path),error=function(e){getwd()}))
source(paste0(price_model_loc,"\\function\\fun_mysql_config_up.R"),echo=FALSE,encoding="utf-8")
local_defin<-fun_mysql_config_up()
source(paste0(price_model_loc,"\\function\\f_model_interface_out_report.R"),echo=FALSE,encoding="utf-8")