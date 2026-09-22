#' prep data for heatmaps of test effort
#'
#' @title prep_heatmap_data
#'
#' @param pcr_all all cleaned testing data
#' @param test_summaries summary of individual level testing data
#'
#' @return 
#' @export
prep_heatmap_data <- function(pcr_all, test_summaries){
  
  ## data for heatmap of number of animals tested for each virus group, by host order
  order_test_effort <- pcr_all |> 
    distinct(predict_sample_id, host_order, virus_group_tested) |> 
    group_by(host_order, virus_group_tested) |> 
    summarize(n_tested = n())
  
  order_test_effort <- order_test_effort |> 
    mutate(virus_group_tested = gsub(virus_group_tested, pattern = "Influenzas", 
                                     replacement = "Influenza A viruses"))
  
  ## data for heatmap of number of animals tested for virus group combinations
  # unique virus groups
  viruses <- sort(unique(unlist(strsplit(test_summaries$virus_groups, ", "))))
  
  # all possible combinations of the virus groups
  virus_pairs <- as.data.frame(
    gtools::combinations(n = length(viruses), 
                         r = 2, 
                         v = as.vector(viruses),
                         repeats.allowed = T)
  )
  
  # calculate number of animals tested for each virus group combination
  for(i in 1:nrow(virus_pairs)){
    
    vir1 <- virus_pairs[i, 1]
    vir2 <- virus_pairs[i, 2]
    
    n_tested <- test_summaries %>% 
      dplyr::filter(stringr::str_detect(virus_groups, vir1) &
                      stringr::str_detect(virus_groups, vir2)) %>% 
      pull(predict_sample_id) %>% 
      length
    
    virus_pairs[i, "n_tested"] <- n_tested
  }
  
  virus_pairs <- virus_pairs |> 
    mutate(across(V1:V2, 
                  ~gsub(., pattern = "Influenzas", 
                        replacement = "Influenza A viruses")))
  
  return(list(order_effort = order_test_effort, 
              group_combos_effort = virus_pairs))
}