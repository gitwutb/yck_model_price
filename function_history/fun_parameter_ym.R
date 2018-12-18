##############层次二---参数输入：训练模型中车龄及公里数选择
fun_parameter_ym<-function(partition_month,regDate){
  select_user_year<-as.numeric(as.character(round((as.Date(partition_month)-as.Date(regDate))/365,2)))
  if (select_user_year<=0.5) {
    case="1"
    user_years_plower=0
    user_years_pupper=1
    mile_plower=0
    mile_pupper=4
  }else if (select_user_year<1.5){
    case="2"
    user_years_plower=0
    user_years_pupper=3
    mile_plower=0.01
    mile_pupper=7
  }else if (select_user_year<3){
    case="3"
    user_years_plower=0.6
    user_years_pupper=4
    mile_plower=0
    mile_pupper=13
  }else if (select_user_year<5){
    case="4"
    user_years_plower=2
    user_years_pupper=7
    mile_plower=1
    mile_pupper=20
  }else {
    case="5"
    user_years_plower=3
    user_years_pupper=15
    mile_plower=1
    mile_pupper=30
  }
  return(list(user_years_plower=user_years_plower,user_years_pupper=user_years_pupper,mile_plower=mile_plower,mile_pupper=mile_pupper,case=case))
}