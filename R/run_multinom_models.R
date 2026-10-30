#' run multinomial logistic regression models 
#' to explore correlates of infection and coinfection
#'
#' @title run_multinom_models
#'
#' @param multinom_data_all
#' @param multinom_data_ats 
#' @param multinom_data_rodents 
#'
#' @return list of model results
#' @export
run_multinom_models <- function(multinom_data_all, multinom_data_bats, 
                                multinom_data_rodents){
  
  model_list <- list()
  
  ## all animals
  model_list[[1]] <- nnet::multinom(inf_cat ~ geo_region + host_order + sex + 
                                      age_class + n_virus_groups_tested + 
                                      n_specimen_types, 
                                    data = multinom_data_all)

  ## bats
  # this model can't handle REs so have to add host_family as FE
  model_list[[2]] <- nnet::multinom(inf_cat ~ geo_region + host_family + sex + 
                                      age_class + cave_roosting +
                                      n_virus_groups_tested + n_specimen_types, 
                                    data = multinom_data_bats)

  ## rodents
  model_list[[3]] <- nnet::multinom(inf_cat ~ sex + age_class + 
                                      captivity_status + n_virus_groups_tested + 
                                      n_specimen_types,
                                    data = multinom_data_rodents)

  names(model_list) <- c("all", "bats", "rodents")
  
  return(model_list)
}
