#' join the pcr and animal data and perform some cleaning
#'
#' @title harmonize_pcr_animal_data
#'
#' @param pcr_raw pcr testing results
#' @param animals_raw animal data
#'
#' @return cleaned pcr results for analysis
#' @export
harmonize_pcr_animal_data <- function(pcr_raw, animals_raw) {
  
  pcr_all = pcr_raw %>% 
    
    # there are duplicates in predict_test_id (excel carryover?), 
    # but they represent genuinely distinct tests
    # so assign new test ids for later summary calculations
    dplyr::mutate(new_test_id = seq(1:nrow(pcr_raw)), .after = 1) %>% 
    
    # to be conservative, we require infection to be confirmed by sequencing
    # (this excludes some PCR positives that were not sequenced)
    mutate(infection = case_when(
      test_result == "Product for Sequencing" & 
        confirmation_result == "Positive" & 
        !is.na(virus) ~ 1, 
      .default = 0)) %>% 
    
    # join the testing and animal data
    # inner join ensures we only keep records where we have test AND animal data
    inner_join(animals_raw, 
               by = c("predict_sample_id" = "predict_individual_id"), 
               suffix = c("", ".dup")) %>% 
    dplyr::select(!ends_with(".dup")) %>% 
    
    # remove domesticated animals since this paper is about wildlife
    dplyr::filter_out(
      captivity_status %in% c("owned domesticated species",
                              "unowned domesticated species")) %>% 
    
    # assign broad geographic regions
    mutate(geo_region = if_else(longitude < 60, 
                                "Africa and West Asia", 
                                "South, East, Southeast Asia")) %>% 
    
    rename(host_sci_name = scientific_name) %>% 
    
    mutate(across("host_sci_name", 
                  ~ str_trim(.x) %>%
                    str_replace_all(" ", "_"))) %>% 
    
    separate_wider_delim(host_sci_name, "_", names = c("host_genus", NA),
                         cols_remove = F,
                         too_few = "align_start",
                         too_many = "drop") %>% 
    
    # some scientific names provided were families or orders
    # this fixes it so families don't end up in the genus column
    mutate(
      host_family = case_when(
        host_genus == "Cercopithecidae" ~ "Cercopithecidae",
        host_genus == "Gliridae" ~ "Gliridae",
        host_genus == "Molossidae" ~ "Molossidae",
        host_genus == "Nycteridae" ~ "Nycteridae",
        host_genus == "Rhinopomatidae" ~ "Rhinopomatidae",
        host_genus == "Sciuridae" ~ "Sciuridae",
        host_genus == "Soricidae" ~ "Soricidae",
        host_genus == "Vespertilionidae" ~ "Vespertilionidae",
        host_genus == "Viverridae" ~ "Viverridae"),
      host_order = case_when(
        host_genus == "Chiroptera" ~ "Chiroptera",
        host_genus == "Rodentia" ~ "Rodentia")
      ) %>% 
    
    mutate(host_genus = str_replace_all(
      host_genus, 
      c("Cercopithecidae|Columbidae|Gliridae|Molossidae|Nycteridae|Rhinopomatidae|Sciuridae|Soricidae|Vespertilionidae|Viverridae|Rodentia|Chiroptera"),
      NA_character_)) %>% 
    
    # rodents/shrews was previously one group--disaggregating here
    mutate(
      taxa_group = case_when(
        host_genus %in% 
          c("Acomys", "Aethomys", "Apodemus", "Arvicanthis", "Atherurus", 
            "Bandicota", "Belomys", "Berylmys", "Bunomys", "Callosciurus",
            "Chiropodomys", "Cricetomys", "Dasymys", "Delanymys", "Dephomys", 
            "Dremomys", "Echiothrix", "Gerbilliscus", "Grammomys", "Graphiurus", 
            "Heimyscus", "Hybomys", "Hylomyscus", "Hylopetes", "Hystrix", 
            "Lemniscomys", "Lenomys", "Leopoldamys", "Lophuromys", "Malacomys", 
            "Mastomys", "Maxomys", "Menetes", "Mus", "Myomyscus", "Niviventer", 
            "Oenomys", "Paruromys", "Pelomys", "Petaurillus", "Petaurista", 
            "Praomys", "Prionomys", "Protoxerus", "Rattus", "Ratufa", 
            "Rhinosciurus", "Rhizomys", "Saccostomus", "Steatomys", "Stochomys", 
            "Sundamys", "Sundasciurus", "Taeromys", "Tamiops", "Taterillus", 
            "Thryonomys", "Trichys", "Uranomys", "Vandeleuria") ~ "rodents",
        host_sci_name %in% c("Gliridae", "Sciuridae", "Rodentia") ~ "rodents",
        
        # "true insectivores" here includes true shrews and moonrats 
        # (all genera are in the order Eulipotyphla)
        host_genus %in% 
          c("Crocidura", "Echinosorex", "Hylomys", "Paracrocidura", "Suncus",
            "Sylvisorex") ~ "true insectivores", 
        host_sci_name == "Soricidae" ~ "true insectivores",
        
        # tree shrews (order Scandentia)
        host_genus %in% c("Dendrogale", "Tupaia") ~ "tree shrews",
        
        # other includes tree hyrax, elephant shrews, and hares
        host_genus %in% c("Dendrohyrax", "Elephantulus", "Lepus") ~ "other",
        
        TRUE ~ taxa_group)) %>% 
    
    # lightly format host name--a very disparate column
    # some animals were only identified to order, family, or genus, 
    # while others were identified to species
    # not super relevant for these analyses bc we used taxa_group for comparisons
    mutate(across("host_sci_name", 
                  ~ str_replace_all(.x, "/", "_") %>%
                    str_remove_all("[.]"))) %>% 
    
    # fix virus families (Bunyaviridae no longer exists as a family)
    mutate(
      viral_family = case_when(
        virus == "Thottapalayam virus" ~ "Hantaviridae",
        TRUE ~ viral_family),
      viral_family = forcats::fct_relevel(
        viral_family, "Hantaviridae", after = 4)
    )
}
