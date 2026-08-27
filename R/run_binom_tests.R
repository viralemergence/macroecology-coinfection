#' run binomial tests to compared observed coinfection to expected coinfection
#'
#' @title run_binom_tests
#'
#' @param pcr_pos 
#' @param vir_level either "viral_family" or "virus"
#' @param observed_mat observed coinfection matrix
#' @param test_summaries individual level testing data
#'
#' @return 
#' @export

run_binom_tests <- function(pcr_pos, vir_level, observed_mat, test_summaries){
  
  # unique virus targets that were detected at least once
  viruses <- pcr_pos[[vir_level]] %>% unique() %>% sort()
  
  # all possible combinations of the detected virus targets
  virus_pairs <- gtools::combinations(n = length(viruses), 
                                      r = 2, 
                                      v = as.vector(viruses),
                                      repeats.allowed = T)
  
  virus_pairs <- t(virus_pairs)
  
  # set up data frame to hold testing effort
  test_numbers <- data.frame(virus_target_tested = viruses, 
                             n_animals_tested = NA)
  
  # calculate how many animals were tested for each virus target
  for(i in viruses){
    
    n_tested <- test_summaries %>% 
      dplyr::filter(stringr::str_detect(virus_targets, i)) %>% 
      pull(predict_sample_id) %>% 
      length
    
    row_index <- which(test_numbers[,1] == i)
    
    test_numbers[row_index,2] <- n_tested
  }
  
  # calculate observed viral prevalence for each virus target
  # number infected divided by number tested
  prev_observed <- pcr_pos %>%
    select(predict_sample_id, {{vir_level}}, infection) %>%
    distinct() %>%
    group_by(across(2)) %>%
    summarise(n_infected = sum(infection)) %>% 
    left_join(test_numbers) %>% 
    mutate(prev = n_infected/n_animals_tested)
  
  # set up empty matrix to hold expected co-infection prevalence values
  expected_mat = array(NA,
                       dim = c(length(viruses), length(viruses)),
                       dimnames = list(viruses, viruses))
  
  # calculate expected prevalence of coinfection of two virus targets
  for (vir1 in viruses) {
    for (vir2 in viruses) {
      
      prev1 <- prev_observed %>%
        dplyr::filter(!!as.symbol(vir_level) == vir1) %>% pull(prev)
      
      prev2 <- prev_observed %>%
        dplyr::filter(!!as.symbol(vir_level) == vir2) %>% pull(prev)
      
      expected_prev <- prev1*prev2
      
      expected_mat[vir1, vir2] <- expected_prev
    }}
  
  # start putting things together into a nice df
  expected_df <- lapply(1:length(virus_pairs[1, ]),
                        function(x){
                          vir1 <- as.character(virus_pairs[1, x])
                          vir2 <- as.character(virus_pairs[2, x])
                          exp_prev <- expected_mat[vir1, vir2]
                          data.frame(vir1, vir2, exp_prev) %>%
                            mutate(virus_pair = paste(vir1, vir2, sep = "-"))
                        }
  ) %>%
    bind_rows()
  
  # need to reorder so that the rows and columns match with the expected
  observed_mat_reordered <- observed_mat[order(row.names(x = observed_mat)), 
                                         order(colnames(x = observed_mat))]
  
  observed_df <- lapply(1:length(virus_pairs[1, ]),
                        function(x){
                          vir1 <- as.character(virus_pairs[1, x])
                          vir2 <- as.character(virus_pairs[2, x])
                          obs_freq <- observed_mat_reordered[vir1, vir2]
                          data.frame(vir1, vir2, obs_freq) %>%
                            mutate(virus_pair = paste(vir1, vir2, sep = "-"))
                        }
  ) %>%
    bind_rows()
  
  binom_df <- expected_df %>%
    left_join(observed_df[, c("virus_pair", "obs_freq")])
  
  # calculate number of animals tested for each virus target combination
  for(i in 1:nrow(binom_df)){
    
    vir1 <- binom_df[i, 1]
    vir2 <- binom_df[i, 2]
    
    n_tested <- test_summaries %>% 
      dplyr::filter(stringr::str_detect(virus_targets, vir1) &
                      stringr::str_detect(virus_targets, vir2)) %>% 
      pull(predict_sample_id) %>% 
      length
    
    binom_df[i, "n_tested"] <- n_tested
  }
  
  # need to exclude combinations that were not actually tested
  binom_df_no_zeros <- binom_df %>% 
    dplyr::filter_out(n_tested == 0)
  
  # perform binomial tests
  binom_tests <- lapply(1:nrow(binom_df_no_zeros), function(x){
    binom.test(
      x = binom_df_no_zeros[x, "obs_freq"],  # x: number of successes
      n = binom_df_no_zeros[x, "n_tested"], # n: number of trials
      p = binom_df_no_zeros[x, "exp_prev"] # p: expected (hypothesized) probability of success
    )})
  
  binom_df_no_zeros$p <- binom_tests %>% purrr::map("p.value") %>% unlist
  binom_df_no_zeros$obs_prev <- binom_tests %>% purrr::map("estimate") %>% unlist
  
  # Bonferroni adjustment
  adj_factor <- nrow(binom_df_no_zeros)
  
  binom_df_no_zeros %<>%
    mutate(p_adj = p*adj_factor,
           # p is a probability 0-1, so cap at 1 if adjusted value is > 1
           p_adj = case_when(
             p_adj > 1 ~ 1, 
             TRUE ~ p_adj),
           signif = case_when(p_adj < 0.05 ~ "*",
                              p_adj >= 0.05 ~ ""),
           greater_than_random = case_when(exp_prev < obs_prev ~ "Yes",
                                           exp_prev > obs_prev ~ "No"))
  
  # put everything into one df, rearrange some columns
  binom_final <- binom_df %>% 
    left_join(binom_df_no_zeros) %>% 
    relocate(vir1, vir2, virus_pair, n_tested, obs_freq, obs_prev, exp_prev,  
             greater_than_random, p, p_adj, signif)
  
  return(binom_final)
  
}

