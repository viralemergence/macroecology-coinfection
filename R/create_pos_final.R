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
  
  # # check for duplicate viral families identified in each animal 
  # double_detects <- pcr_pos %>% 
  #   mutate(virus_animal = paste(predict_sample_id, viral_family)) %>% 
  #   filter(duplicated(virus_animal)) %>% pull(virus_animal) 
  # 
  # check <- pcr_pos %>% 
  #   mutate(virus_animal = paste(predict_sample_id, viral_family)) %>% 
  #   filter(virus_animal %in% double_detects) %>% 
  #   select(virus_animal, specimen_type, virus, infection) 
  # 
  # View(check) # a bunch where the same virus is detected multiple times per sample
  
  coinfects <- pcr_pos %>% 
    dplyr::filter(!is.na(virus)) %>% 
    # get unique animal-virus combos
    distinct(predict_sample_id, virus) %>% 
    # which animals are duplicated?
    janitor::get_dupes(predict_sample_id) %>% 
    distinct(predict_sample_id) %>% 
    pull(predict_sample_id) #223 unique animals coinfected (out of 3271)
  
  pos_final <- pcr_pos %>%
    mutate(virus_animal = paste(predict_sample_id, virus, sep = "-")) %>%
    dplyr::filter(!duplicated(virus_animal)) %>% #3506 
    mutate(coinfect_status = case_when(
      predict_sample_id %in% coinfects ~ "coinfection",
      !predict_sample_id %in% coinfects ~ "single infection")) 
  
  return(pos_final)
  
}