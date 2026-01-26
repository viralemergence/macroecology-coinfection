#' create coinfection dataframe for GLMMs
#' each row is a unique animal
#'
#' @title create_coinf_df
#'
#' @param pcr_pos pcr testing results (positives only)
#' @param family_testing number of virus families tested, for each animal
#'
#' @return df of all positive animals along with coinfection info
#' @export
#' 
create_coinf_df <- function(pcr_pos, family_testing){

  # count unique viruses detected per sample_id (unique animals)
  virus_coinf <- pcr_pos %>% 
    dplyr::select(predict_sample_id, viral_family, virus) %>% 
    distinct() %>% 
    group_by(predict_sample_id) %>% 
    count %>% 
    rename(n_virus = n)
  
  # create df of all positive sample_id, noting virus coinfections
  coinf_df <- pcr_pos %>% 
    left_join(virus_coinf, by = "predict_sample_id") %>% 
    # remove columns at the test level, because each row is an animal
    dplyr::select(-c(predict_test_id, specimen_id:interpretation, infection,
                     sample_location_latitude, sample_location_longitude)) %>% 
    dplyr::filter(!duplicated(predict_sample_id)) %>% 
    mutate(virus_coinf = case_when(n_virus > 1 ~ 1,
                                   n_virus == 1 ~ 0)) %>% 
    # add number of virus families tested for
    left_join(family_testing)
  
  return(coinf_df)
  
}
