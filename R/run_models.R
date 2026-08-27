#' run GL(M)Ms to explore correlates of coinfection
#'
#' @title run_models
#'
#' @param model_data_all
#' @param model_data_bats 
#' @param model_data_rodents 
#'
#' @return list of model results
#' @export
run_models <- function(model_data_all, model_data_bats, model_data_rodents){
  
  model_list <- list()
  
  ## all animals
  model_list[[1]] <- glm(virus_coinf ~ geo_region + host_order + sex + 
                           age_class + n_virus_targets_tested + 
                           n_specimen_types, 
                         family = binomial(), data = model_data_all)
    
  ## bats
  model_list[[2]] <- glmmTMB(virus_coinf ~ geo_region + sex + age_class + 
                               cave_roosting + n_virus_targets_tested + 
                               n_specimen_types + (1|host_family),
                             family = binomial(), data = model_data_bats)
  
  ## rodents
  model_list[[3]] <- glm(virus_coinf ~ sex + age_class + captivity_status +
                           n_virus_targets_tested + n_specimen_types, 
                         family = binomial(), data = model_data_rodents)

  names(model_list) <- c("all", "bats", "rodents")
  
  return(model_list)
}
