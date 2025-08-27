#' prep matrix data for network analyses
#'
#' @title prep_matrix_data
#'
#' @param pos_final 
#' @param vir_level either "viral_family" or "virus"
#' @param total_pos
#'
#' @return a matrix of coinfections
#' @export
prep_matrix_data <- function(pos_final, vir_level, total_pos = NULL){
  
  # create df showing number of infections by vir_level, for all sample_ids
  pos_wide <- pos_final %>%
    dplyr::select(predict_sample_id, {{vir_level}}, infection) %>% 
    group_by(across(1:2)) %>% 
    summarise(infection = sum(infection)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols ="predict_sample_id", 
                names_from = {{vir_level}}, 
                values_from = "infection") %>% 
    mutate(across(everything(), ~replace_na(., 0))) # NA to 0
  
  # convert data frame to matrix
  M <- pos_wide %>% 
    dplyr::select(-predict_sample_id) %>% 
    as.matrix() %>% 
    crossprod(.,.)
  
  # reorder alphabetically
  col_order <- sort(row.names(M))
  M <- M[col_order, col_order]
  
  if(vir_level == "viral_family"){
    # need to adjust the diagonals because single infections are getting lumped in
    # divide by two so that co-infections aren't double-counted
    diag(M) <- (diag(M) - total_pos$n) / 2
    }else if(vir_level == "virus"){
      diag(M) <- 0
      }

  return(M)
}
