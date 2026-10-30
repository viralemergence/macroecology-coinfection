#' for modeling, create a dataframe where each unique animal is one row
#' and we have data on number of viruses detected
#'
#' @title prep_unique_animals
#'
#' @param pcr_all
#' @param test_summaries 
#'
#' @return df of all infected and uninfected animals
#' @export
prep_unique_animals <- function(pcr_all, test_summaries){
  
  # for all unique infected animals, count unique viruses detected
  vir_counts <- pcr_all %>% 
    dplyr::filter(infection == 1) |> 
    dplyr::select(predict_sample_id, viral_family, virus) %>% 
    distinct() %>% 
    group_by(predict_sample_id) %>% 
    count %>% 
    rename(n_virus = n)
  
  unique_animals <- pcr_all |> 
    # keep only unique animals
    distinct(predict_sample_id, .keep_all = T) |> 
    # remove columns containing test level data because each row is now an animal
    dplyr::select(-c(predict_test_id, new_test_id, specimen_id:interpretation, 
                     infection, sample_location_latitude, 
                     sample_location_longitude)) |> 
    # add the virus count data for the infected animals
    left_join(vir_counts) |> 
    # all other animals are uninfected, ie. n_virus = 0
    mutate(n_virus = replace_na(n_virus, 0)) |> 
    # create categorical variable for infection status
    dplyr::mutate(inf_cat = case_when(
      n_virus == 0 ~ "uninfected",
      n_virus == 1 ~ "single infection",
      n_virus > 1 ~ "coinfection")) |> 
    # add testing effort summary info
    left_join(test_summaries)
  
  return(unique_animals)
  
}
