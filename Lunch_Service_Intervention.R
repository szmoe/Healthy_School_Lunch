#Simulation for lunch service intervention

#make variables

library(decisionSupport)

make_variables<-function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}
make_variables(estimate_read_csv("Input_lunch.csv"))

lunch_service_function <- function(x, varnames) {

#Add risk-adjusted benefits if intervened 
  
#Add percent non-compliance that can affect external funding
Non_compliance_risk_funding <- min(Te_noncom,
                                   Pa_noncom_ng,
                                   St_noncom,
                                   No_food_safety)

Non_compliance_risk_lunchsale <- min(Te_noncom,
                                     Pa_noncom_ng,
                                     Pa_noncom_sf,
                                     No_food_safety)

#Increase in benefits due to intervention
Increased_organizational_funding <- Og_funding*Og_increased_funding

Increased_individual_funding <- Do_funding*Do_increased_funding

Increased_enrollment <- Profit_enrollment*Profit_increased_enrollment
                    
Income_lunchsales <- Unit_lunch_value * Number_student_paid

Saving_supplementary <- Cost_supplementary_food * Saving_supplementary_food

#Risk_adjusted benefits if lunch policy is implemented

Lunch_policy_yes_no <- chance_event(If_lunch_policy,
                                    value_if = 1,
                                    value_if_not = 0)

#Extra funding from organizations
Benefit_organizational_funding <- if (Lunch_policy_yes_no == 1) {
  Benefit_organizational_funding = vv (Increased_organizational_funding + 
                                       Policy_increase_Ogfunding, 
                                       var_CV, n_years,
                                       relative_trend = inflation_rate)*
                                       Non_compliance_risk_funding
 
} else {
  vv (Increased_organizational_funding,
      var_CV, n_years,
      relative_trend = inflation_rate)* Non_compliance_risk_funding
}

#Increased funding from organization starts from year 2
Benefit_organizational_funding[1] <- 0

#Extra funding from individual donors
Benefit_individual_funding <- if (Lunch_policy_yes_no == 1) {
  Benefit_individual_funding = vv (Increased_individual_funding +
                                   Policy_increase_Dofunding,
                                   var_CV, n_years,
                                   relative_trend = inflation_rate)*
                                   Non_compliance_risk_funding
} else {
  vv (Increased_individual_funding,
      var_CV, n_years,
      relative_trend = inflation_rate)* Non_compliance_risk_funding
}

#Extra lunch sales
Benefit_lunchsales <- if (Lunch_policy_yes_no == 1) {
  Benefit_lunchsales = vv (Income_lunchsales + Policy_increase_lunchsales,
                           var_CV, n_years,
                           relative_trend = inflation_rate)*
                           Non_compliance_risk_lunchsale
} else {
  vv (Income_lunchsales,
      var_CV, n_years,
      relative_trend = inflation_rate)* Non_compliance_risk_lunchsale
}

#Extra income from increased enrollment
Benefit_enrollment <- vv (Increased_enrollment,
                          var_CV, n_years,
                          relative_trend = inflation_rate)*
                          Non_compliance_risk_funding

#Enrollment increased from year 2
Benefit_enrollment[1] <- 0

#Saving from reduced cost of supplementary food
Saving_supplementary_food <- vv (Saving_supplementary,
                                 var_CV, n_years,
                                 relative_trend = inflation_rate)

Total_benefit <- Benefit_organizational_funding +
                 Benefit_individual_funding +
                 Benefit_lunchsales +
                 Benefit_enrollment +
                 Saving_supplementary_food +
                 Og_funding + Do_funding +
                 Profit_enrollment
  
  
  
}