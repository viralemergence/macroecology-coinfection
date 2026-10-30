#' functions to prep data for multinomial logistic regression
#'
prep_multinom_data_all <- function(unique_animals){
  
  # prep model data
  multinom_data_prelim <- unique_animals %>% 
    dplyr::filter(
      # only keep animals with known sex
      sex %in% c("male", "female"),
      # only keep animals with known age class, within the 3 largest age classes
      age_class %in% c("adult (reproductive age)",
                       "subadult (immature, independent)", 
                       "juvenile (dependent on dam)")) %>% 
    # shorten names of age classes
    mutate(age_class = forcats::fct_recode(
      age_class,
      adult = "adult (reproductive age)",
      subadult = "subadult (immature, independent)",
      juvenile = "juvenile (dependent on dam)"))
  
  # identify host orders with few animals (will mess up parameter estimation)
  sparse_orders <- multinom_data_prelim |> 
    group_by(host_order) |> 
    dplyr::summarise(n = n()) |> 
    dplyr::filter(n < 10) |> 
    pull(host_order)
  
  # grab only the host orders with variation in the outcome variable (inf_cat)
  # that is, at least 1 animal in each infection category 
  # (uninfected, single infection, coinfection)
  inf_orders <- multinom_data_prelim |> 
    group_by(host_order, inf_cat) |> 
    dplyr::summarise(n = n()) |> 
    janitor::get_dupes(host_order) |> 
    dplyr::filter(dupe_count == 3) |> 
    distinct(host_order) |> 
    pull(host_order)
  
  multinom_data_all <- multinom_data_prelim |> 
    # exclude sparse orders
    dplyr::filter_out(host_order %in% sparse_orders) |> 
    # keep only the orders with outcome variation
    dplyr::filter(host_order %in% inf_orders) |> 
    # create factors
    mutate(across(c(host_order, sex, age_class, captivity_status, inf_cat), 
                  factor)) |> 
    # set reference levels
    mutate(age_class = forcats::fct_relevel(age_class, "adult", "subadult"),
           host_order = forcats::fct_relevel(host_order, "Chiroptera"),
           inf_cat = forcats::fct_relevel(inf_cat, "uninfected", 
                                          "single infection")) |> 
    # keep only the columns needed
    dplyr::select(inf_cat, host_sci_name, host_family, host_order, 
                  captivity_status, sex, age_class, geo_region, 
                  n_virus_groups_tested, n_specimen_types)
  
  return(multinom_data_all)
  
}

prep_multinom_data_bats <- function(multinom_data_all, roosting){
  
  multinom_data_bats_prelim <- multinom_data_all %>% 
    dplyr::filter(host_order == "Chiroptera") %>% 
    dplyr::left_join(roosting, 
                     by = c("host_sci_name" = "scientific_name")) %>% 
    # exclude bats with missing family or missing roosting data
    dplyr::filter(!is.na(host_family),
                  !is.na(cave_roosting))
  
  # sparse_families <- multinom_data_bats_prelim |> 
  #   group_by(host_family) |> 
  #   dplyr::summarise(n = n()) |> 
  #   dplyr::filter(n < 10) |> 
  #   pull(host_family)
  
  inf_families <- multinom_data_bats_prelim |> 
    group_by(host_family, inf_cat) |> 
    dplyr::summarise(n = n()) |> 
    janitor::get_dupes(host_family) |> 
    dplyr::filter(dupe_count == 3) |> 
    distinct(host_family) |> 
    pull(host_family)
  
  multinom_data_bats <- multinom_data_bats_prelim |> 
    dplyr::filter(host_family %in% inf_families)
  
  return(multinom_data_bats)
  
}

prep_multinom_data_rodents <- function(multinom_data_all){
  
  multinom_data_rodents <- multinom_data_all %>% 
    dplyr::filter(host_order == "Rodentia") %>%
    # collapse juveniles and subadults because there are few juveniles
    mutate(age_class = forcats::fct_collapse(
      age_class,
      "adult" = "adult",
      "subadult or juvenile" = c("subadult", "juvenile")))
  
  return(multinom_data_rodents)
  
}
