#' calculate several test-related quantities of interest at the animal level
#'
#' @title calc_test_summaries
#'
#' @param pcr_all
#'
#' @return df of all animals with testing summary quantities
#' @export
#' 
calc_test_summaries <- function(pcr_all){
  
  # number of virus families tested for
  family_testing = pcr_all %>% 
    distinct(predict_sample_id, viral_family_tested) %>% 
    group_by(predict_sample_id) %>% 
    dplyr::summarise(n_viral_families_tested = n())
  
  # number of unique PCR tests performed
  unique_testing = pcr_all %>% 
    distinct(predict_sample_id, new_test_id) %>% 
    group_by(predict_sample_id) %>% 
    dplyr::summarise(n_tests_performed = n())
  
  # number of distinct specimen types
  num_specimen_types = pcr_all %>% 
    distinct(predict_sample_id, specimen_type) %>% 
    group_by(predict_sample_id) %>% 
    dplyr::summarise(n_specimen_types = n())
  
  # distinct specimen types
  unique_specimen_types <- pcr_all %>% 
    distinct(predict_sample_id, specimen_type) %>% 
    arrange(predict_sample_id, specimen_type) %>% 
    group_by(predict_sample_id) %>%
    dplyr::summarise(specimen_types = paste(specimen_type, collapse = ", "))
  
  test_summaries <- family_testing %>% 
    left_join(unique_testing) %>% 
    left_join(num_specimen_types) %>% 
    left_join(unique_specimen_types)
  
  return(test_summaries)
  
}