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

  # count unique viruses detected per sample_id
  virus_coinf <- pcr_pos %>% 
    #dplyr::filter(predict_sample_id %in% ids_coinf) %>% 
    dplyr::select(predict_sample_id, viral_family, virus) %>% 
    distinct() %>% 
    group_by(predict_sample_id) %>% 
    count %>% 
    rename(n_virus = n)
  
  # create df of all positive sample_id, noting virus coinfections
  coinf_df <- pcr_pos %>% 
    left_join(virus_coinf, by = "predict_sample_id") %>% 
    dplyr::select(-c(specimen_id:interpretation, infection)) %>% 
    dplyr::filter(!duplicated(predict_sample_id)) %>% 
    mutate(virus_coinf = case_when(n_virus > 1 ~ 1,
                                   n_virus == 1 ~ 0))
  
  return(coinf_df)
  
}
