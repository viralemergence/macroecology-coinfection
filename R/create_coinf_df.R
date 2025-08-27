#' create coinfection dataframe for GLMMs
#' each row is a unique animal
#'
#' @title create_coinf_df
#'
#' @param pcr_pos pcr testing results (positives only)
#'
#' @return df of all positive animals along with coinfection info
#' @export
#' 
create_coinf_df <- function(pcr_pos){
  
  ## summaries of coinfection 
  # pcr_pos %>%  
  #   group_by(predict_sample_id) %>% 
  #   count() %>% 
  #   pull(n) %>% 
  #   summary # 7 includes some of the same 

  # get sample ids for those that have multiple infections
  # ids_coinf <- pcr_pos %>%  
  #   group_by(predict_sample_id) %>% 
  #   count() %>% 
  #   dplyr::filter(n > 1) %>% 
  #   pull(predict_sample_id) 

  # count unique viruses detected per sample_id
  virus_coinf <- pcr_pos %>% 
    #dplyr::filter(predict_sample_id %in% ids_coinf) %>% 
    dplyr::select(predict_sample_id, viral_family, virus) %>% 
    distinct() %>% 
    group_by(predict_sample_id) %>% 
    count %>% 
    rename(n_virus = n)
  
  # count unique virus families detected per sample_id
  virus_family_coinf <- pcr_pos %>% 
    #dplyr::filter(predict_sample_id %in% ids_coinf) %>% 
    dplyr::select(predict_sample_id, viral_family) %>% 
    distinct() %>%
    group_by(predict_sample_id) %>% 
    count %>% 
    rename(n_virus_fam = n) 
  
  # create df of all positive sample_id, noting virus and virus family coinfections
  coinf_df <- pcr_pos %>% 
    left_join(virus_coinf, by = "predict_sample_id") %>% 
    left_join(virus_family_coinf, by = "predict_sample_id") %>% 
    dplyr::select(-c(specimen_id:interpretation, infection)) %>% 
    dplyr::filter(!duplicated(predict_sample_id)) %>% 
    mutate(virus_coinf = case_when(n_virus > 1 ~ 1,
                                   n_virus == 1 ~ 0),
           virus_fam_coinf = case_when(n_virus_fam > 1 ~ 1, 
                                       n_virus_fam == 1 ~ 0)) #3271 
  
  return(coinf_df)
  
}
