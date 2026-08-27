#' calculate several test-related quantities of interest at the animal level
#'
#' @title calc_test_summaries
#'
#' @param pcr_harmonized
#'
#' @return df of all animals with testing summary quantities
#' @export
#' 
calc_test_summaries <- function(pcr_harmonized){
  
  # number of virus targets tested for
  target_testing = pcr_harmonized %>% 
    distinct(predict_sample_id, virus_target_tested) %>% 
    group_by(predict_sample_id) %>% 
    dplyr::summarise(n_virus_targets_tested = n())
  
  # distinct virus targets tested for
  unique_virus_targets <- pcr_harmonized %>% 
    distinct(predict_sample_id, virus_target_tested) %>% 
    arrange(predict_sample_id, virus_target_tested) %>% 
    group_by(predict_sample_id) %>%
    dplyr::summarise(virus_targets = paste(virus_target_tested, collapse = ", "))
  
  # number of unique PCR tests performed
  unique_testing = pcr_harmonized %>% 
    distinct(predict_sample_id, new_test_id) %>% 
    group_by(predict_sample_id) %>% 
    dplyr::summarise(n_tests_performed = n())
  
  # number of distinct specimen types
  num_specimen_types = pcr_harmonized %>% 
    distinct(predict_sample_id, specimen_type) %>% 
    group_by(predict_sample_id) %>% 
    dplyr::summarise(n_specimen_types = n()) %>% 
    mutate(n_specimen_types_binary = case_when(
      n_specimen_types == 1 ~ "one",
      n_specimen_types > 1 ~ "two or more"))
  
  # distinct specimen types
  unique_specimen_types <- pcr_harmonized %>% 
    distinct(predict_sample_id, specimen_type) %>% 
    arrange(predict_sample_id, specimen_type) %>% 
    group_by(predict_sample_id) %>%
    dplyr::summarise(specimen_types = paste(specimen_type, collapse = ", "))
  
  # number of collected specimens
  num_specimens = pcr_harmonized %>% 
    distinct(predict_sample_id, specimen_id) %>% 
    group_by(predict_sample_id) %>% 
    dplyr::summarise(n_specimens = n())
  
  # join everything together
  test_summaries <- target_testing %>% 
    left_join(unique_virus_targets) %>%
    left_join(unique_testing) %>% 
    left_join(num_specimen_types) %>% 
    left_join(unique_specimen_types) %>% 
    left_join(num_specimens) %>% 
    left_join(pcr_harmonized[, c("predict_sample_id", "host_order")] %>% 
                distinct())
  
  return(test_summaries)
  
}