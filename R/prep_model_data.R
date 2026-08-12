#' functions to prep data for GLM(M)s to explore correlates of coinfection
#'
prep_model_data_all <- function(coinf_df){
  
  # only want to keep the host orders containing at least one coinfected animal
  order_keep <- sort(unique(coinf_df$host_order))[xtabs(~host_order + virus_coinf, coinf_df)[,2]>0]
  
  model_data_all <- coinf_df %>% 
    dplyr::filter(
      host_order %in% order_keep,
      # only keep animals with known sex
      sex %in% c("male", "female"),
      # excludes 1 fetus and 4 neonates, as well as those with missing age class
      age_class %in% c("adult (reproductive age)",
                       "subadult (immature, independent)", 
                       "juvenile (dependent on dam)")) %>% 
    mutate(age_class = forcats::fct_recode(
      age_class,
      adult = "adult (reproductive age)",
      subadult = "subadult (immature, independent)",
      juvenile = "juvenile (dependent on dam)")) %>% 
    mutate(across(c(host_order, sex, age_class, captivity_status), factor)) %>% 
    mutate(age_class = forcats::fct_relevel(age_class, "adult", "subadult"),
           host_order = forcats::fct_relevel(host_order, "Chiroptera"))
    
  return(model_data_all)
  
}

prep_model_data_bats <- function(model_data_all, roosting_data){
  
  model_data_bats <- model_data_all %>% 
    dplyr::filter(host_order == "Chiroptera") %>% 
    dplyr::left_join(roosting_data, 
                     by = c("host_sci_name" = "scientific_name")) %>% 
    # exclude bats with missing family or missing roosting data
    dplyr::filter(!is.na(host_family),
                  !is.na(cave_roosting))
  
  return(model_data_bats)
  
}

prep_model_data_rodents <- function(model_data_all){
  
  model_data_rodents <- model_data_all %>% 
    dplyr::filter(host_order == "Rodentia") %>%
    # collapse juveniles and subadults because there are few juveniles
    mutate(age_class = forcats::fct_collapse(
      age_class,
      "adult" = "adult",
      "subadult or juvenile" = c("subadult", "juvenile")))
  
  return(model_data_rodents)
  
}