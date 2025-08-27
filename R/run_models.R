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
  model_list[[1]] <- glm(virus_coinf ~ geo_region + taxa_group + sex + 
                           age_class + captivity_status, family = binomial(), 
                         data = model_data_all)
  # summary(model_list[[1]])
  # drop1(model_list[[1]], test = "Chisq")
    

  ## bats
  model_list[[2]] <- glmmTMB(virus_coinf ~ geo_region + sex + age_class + 
                               cave_roosting + (1|host_family),
                             family = binomial(), data = model_data_bats)
  # summary(model_list[[2]])
  # drop1(model_list[[2]], test = "Chisq")
  
  
  ## rodents
  model_list[[3]] <- glm(virus_coinf ~ sex + age_class, 
                         family = binomial(), data = model_data_rodents)
  # summary(model_list[[3]])
  # drop1(model_list[[3]], test = "Chisq")
  
  names(model_list) <- c("all", "bats", "rodents")
  
  return(model_list)
  
}
