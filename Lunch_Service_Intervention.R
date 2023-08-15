#Simulation for lunch service intervention

#make variables

library(decisionSupport)

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(decisionSupport::estimate_read_csv(paste("Input_lunch.csv")))

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

Total_benefit_Intervention <- Benefit_organizational_funding +
                              Benefit_individual_funding +
                              Benefit_lunchsales +
                              Benefit_enrollment +
                              Saving_supplementary_food +
                              Og_funding + Do_funding +
                              Profit_enrollment
  
#Add risk-adjusted cost if intervened

#One-time cost due to intervention
Cost_establishment <- Cost_construction + Cost_installation +
                      Cost_equipment + Cost_utensil

#Reduce cost of establishment if there is initial investment (for year 1 only)
Initial_investment_yes_no <- chance_event(If_investment,
                                          value_if = 1,
                                          value_if_not = 0)

Cost_establishment_with_investment <- if (Initial_investment_yes_no == 1) {
  vv (Cost_establishment * Initial_investment,
      var_CV = var_CV,
      n = 1,
      relative_trend = inflation_rate)
} else {
  vv (Cost_establishment,
      var_CV = var_CV,
      n = 1,
      relative_trend = inflation_rate)
}

#Recurring cost due to intervention

#Cost if summer cooking class
Summer_cooking_class_yes_no <- chance_event(If_summer_class,
                                            value_if = 1,
                                            value_if_not = 0)

Cost_summer_cooking_class <- if (Summer_cooking_class_yes_no == 1) {
  Cost_summer_cooking_class = vv (Cost_summer_class,
                                  var_CV, n_years,
                                  relative_trend = inflation_rate)
} else {
  Cost_summer_cooking_class <- 0
}

#Maintenance cost if natural hazard occurs
Natural_hazard_yes_no <- chance_event(If_natural_hazard,
                                      value_if = 1,
                                      value_if_not = 0)

Cost_maintenance_total <- if (Natural_hazard_yes_no == 1) {
  Cost_maintenance_total = vv (Cost_maintenance + 
                              (Cost_natural_hazard * Cost_maintenance),
                              var_CV, n_years,
                              relative_trend = inflation_rate)
} else {
  Cost_maintenance_total = vv (Cost_maintenance,
                               var_CV, n_years,
                               relative_trend = inflation_rate)
}

#Cost of food loss_waste and fuel,saving for electricity if power outage occurs
Power_outage_yes_no <- chance_event(If_power_outage,
                                    value_if = 1,
                                    value_if_not = 0)

Cost_food_waste_loss <- if (Power_outage_yes_no == 1) {
  Cost_food_waste_loss = vv (Food_waste_loss +
                             (Cost_power_outage_food * Food_waste_loss),
                             var_CV, n_years,
                             relative_trend = inflation_rate)
} else {
  Cost_food_waste_loss = vv (Food_waste_loss,
                             var_CV, n_years,
                             relative_trend = inflation_rate)
}

Cost_fuel_total <- if (Power_outage_yes_no == 1) {
  Cost_fuel_total = vv (Cost_fuel +
                        (Cost_power_outage_fuel * Cost_fuel),
                        var_CV, n_years,
                        relative_trend = inflation_rate)
} else {
  Cost_fuel_total = vv (Cost_fuel,
                        var_CV, n_years,
                        relative_trend = inflation_rate)
}

Cost_electricity_total <- if (Power_outage_yes_no == 1) {
  Cost_electricity_total = vv (Cost_electricity -
                               (Cost_electricity * Saving_electricity),
                               var_CV, n_years,
                               relative_trend = inflation_rate)
} else {
  Cost_electricity_total = vv (Cost_electricity,
                               var_CV, n_years,
                               relative_trend = inflation_rate)
}

# Training cost if low knowledge and low skill

#Adjuted training cost if nutrionist has low nutrition knowledge and skill
Nu_low_skill_yes_no <- chance_event(Nu_low_skill,
                                    value_if = 1,
                                    value_if_not = 0)

Cost_training_adjusted <- if (Nu_low_skill_yes_no == 1) {
  Cost_training_adjusted = vv (Cost_training +
                               (Cost_training * Cost_nu_low_skill),
                               var_CV, n_years,
                               relative_trend = inflation_rate)
} else {
  Cost_training_adjusted = vv (Cost_training,
                               var_CV, n_years,
                               relative_trend = inflation_rate)
}

#Total cost of training if workers have low skill
Worker_low_skill_yes_no <- chance_event(Worker_low_skill,
                                        value_if = 1,
                                        value_if_not = 0)

Cost_training_total <- if (Worker_low_skill_yes_no == 1) {
  Cost_training_total = vv (Cost_training_adjusted +
                            (Cost_training_adjusted * Cost_worker_low_skill),
                            var_CV, n_years,
                            relative_trend = inflation_rate)
} else {
  Cost_training_total = vv (Cost_training_adjusted,
                            var_CV, n_years,
                            relative_trend = inflation_rate)
}

#Cost for free lunch for students from low income family
Cost_free_lunch <- vv (Unit_lunch_value * Number_student_unpaid,
                       var_CV, n_years,
                       relative_trend = inflation_rate)

#Add remaining costs
Remaining_cost <- vv (Cost_water + Cost_cooking_gas +
                      Cost_fresh_goods + Cost_dry_goods +
                      Cost_salary + Cost_menu_printing +
                      Cost_supplementary_food,
                      var_CV, n_years,
                      relative_trend = inflation_rate)

#Add total cost with intervention
Total_cost_with_intervention <- Cost_summer_cooking_class +
                                Cost_maintenance_total +
                                Cost_food_waste_loss + 
                                Cost_fuel_total +
                                Cost_electricity_total +
                                Cost_training_total + 
                                Cost_free_lunch + 
                                Remaining_cost +
                                Cost_establishment_with_investment


#Find intervention result
Lunch_service_intervention_result <- Total_benefit_Intervention -
                                     Total_cost_with_intervention

#Add benefit without intervention
Benefit_no_intervention <- vv (Og_funding + Do_funding + Profit_enrollment,
                               var_CV, n_years,
                               relative_trend = inflation_rate)

#Add cost without intervention
Cost_no_intervention <- vv (Cost_supplementary_food,
                            var_CV, n_years,
                            relative_trend = inflation_rate)

#Find result for no intervention
Lunch_service_no_intervention_result <- Benefit_no_intervention -
                                        Cost_no_intervention

#Calculate the Net Present Value (NPV) with discount rate

#NPV for intervention
NPV_interv <- discount(x = Lunch_service_intervention_result,
                       discount_rate = discount_rate,
                       calculate_NPV = TRUE)

#NPV for no intervention
NPV_no_interv <- discount(x = Lunch_service_no_intervention_result,
                          discount_rate = discount_rate,
                          calculate_NPV = TRUE)

return(list(NPV_lunch_service = NPV_interv,
            NPV_no_lunch_service = NPV_no_interv,
            decision = NPV_interv - NPV_no_interv,
            Cashflow_lunch_service = Lunch_service_intervention_result -
                                     Lunch_service_no_intervention_result))
                            
}

#Run the Monte Carlo Simulation

#library(readr)
#input_table <- read.csv("Input_lunch.csv")

Lunch_service_simulation_result<- mcSimulation (
                                estimate = estimate_read_csv("Input_lunch.csv"),
                                model_function = lunch_service_function,
                                numberOfModelRuns = 1000,
                                functionSyntax = "plainNames")

#plot distributions for NPV_lunch_service and NPV_no_lunch_service
plot_distributions(mcSimulation_object = Lunch_service_simulation_result, 
                   vars = c("NPV_lunch_service", "NPV_no_lunch_service"),
                   method = 'hist_simple_overlay', 
                   base_size = 7)

#plot distribution for the decision
plot_distributions(mcSimulation_object = Lunch_service_simulation_result, 
                   vars = "decision",
                   method = 'hist_simple_overlay')

# Cashflow of the Lunch_service_intervention
plot_cashflow(mcSimulation_object = Lunch_service_simulation_result, 
              cashflow_var_name = " Cashflow_lunch_service")

#Find EVPI 
mcSimulation_table <- data.frame(Lunch_service_simulation_result$x, 
                                 Lunch_service_simulation_result$y[1:3])

evpi <- multi_EVPI(mc = mcSimulation_table, first_out_var = "NPV_lunch_service")


plot_evpi(evpi, decision_vars = "decision")

#Find PLS result
pls_result <- plsr.mcSimulation(object = Lunch_service_simulation_result,
                                resultName = names
                                (Lunch_service_simulation_result$y)[1], 
                                ncomp = 1)

input_table <- read.csv("Input_lunch.csv")
plot_pls(pls_result, input_table = input_table, threshold = 0)

