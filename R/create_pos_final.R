#' create a dataframe of all unique virus-animal combinations
#' and whether they represent single infections or coinfections
#'
#' @title create_pos_final
#'
#' @param pcr_pos pcr testing results (positives only)
#'
#' @return df with unique virus-animal combos and coinfection status
#' @export
#' 
create_pos_final <- function(pcr_pos){
  
  coinfects <- pcr_pos %>% 
    # get unique animal-virus combos
    distinct(predict_sample_id, virus) %>% 
    # which animals show up more than once? these are the coinfected ones
    janitor::get_dupes(predict_sample_id) %>% 
    distinct(predict_sample_id) %>% 
    pull(predict_sample_id)
  
  pos_final <- pcr_pos %>%
    mutate(virus_animal = paste(predict_sample_id, virus, sep = "-")) %>%
    dplyr::filter(!duplicated(virus_animal)) %>%
    mutate(coinfect_status = case_when(
      predict_sample_id %in% coinfects ~ "coinfection",
      !predict_sample_id %in% coinfects ~ "single infection")) %>% 
    # since this is now on the animal level, need to remove the test info
    # since that's at a specimen rather than animal level
    dplyr::select(-c(specimen_type, test_type, test_result:sequence))
  
  return(pos_final)
  
}