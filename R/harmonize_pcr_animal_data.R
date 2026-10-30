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
    
    rename(host_sci_name = scientific_name,
           virus_group_tested = viral_family_tested) %>% 
    
    # PCR protocols tested for influenza A virus, 
    # only member of the alphainfluenzavirus genus
    mutate(virus_group_tested = gsub(virus_group_tested, pattern = "Influenzas", 
                                     replacement = "Alphainfluenzaviruses")) %>% 
    
    # there are duplicates in predict_test_id (excel carryover?), 
    # but they represent genuinely distinct tests
    # so assign new test ids for later summary calculations
    dplyr::mutate(new_test_id = seq(1:nrow(pcr_raw)), .after = 1) %>% 
    
    # limit our analyses to key virus groups
    dplyr::filter(virus_group_tested %in% c("Adenoviruses", 
                                            "Alphainfluenzaviruses", 
                                            "Alphaviruses", "Arenaviruses", 
                                            "Astroviruses", "Coronaviruses", 
                                            "Filoviruses", "Flaviviruses", 
                                            "Hantaviruses", "Orthobunyaviruses", 
                                            "Paramyxoviruses", "Rhabdoviruses")) %>% 
    
    # to be conservative, we require infection to be confirmed by sequencing
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
                                "Southeast Asia")) %>% 
    
    mutate(across("host_sci_name", 
                  ~ str_trim(.x) %>%
                    str_replace_all(" ", "_"))) %>% 
    
    # for later analyses, it will be helpful to have hosts grouped
    # at a higher taxonomic level than species
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
      c("Cercopithecidae|Gliridae|Molossidae|Nycteridae|Rhinopomatidae|Sciuridae|Soricidae|Vespertilionidae|Viverridae|Chiroptera|Rodentia"),
      NA_character_)) %>% 
    
    mutate(
      host_order = case_when(
        
        taxa_group == "ungulates" ~ "Artiodactyla",
        taxa_group == "carnivores" ~ "Carnivora",
        taxa_group == "bats" ~ "Chiroptera",
        host_genus == "Dendrohyrax" ~ "Hyracoidea",
        host_genus == "Lepus" ~ "Lagomorpha",
        host_genus == "Elephantulus" ~ "Macroscelidea",
        taxa_group == "pangolins" ~ "Pholidota",
        taxa_group == "non-human primates" ~ "Primates",
        
        host_genus %in% c("Crocidura", "Echinosorex", "Hylomys", 
                          "Paracrocidura", "Suncus", "Sylvisorex") ~ "Eulipotyphla", 
        host_sci_name == "Soricidae" ~ "Eulipotyphla",
        host_genus %in% c("Acomys", "Aethomys", "Apodemus", "Arvicanthis", 
                          "Atherurus", "Bandicota", "Belomys", "Berylmys", 
                          "Bunomys", "Callosciurus", "Chiropodomys", 
                          "Cricetomys", "Dasymys", "Delanymys", "Dephomys",
                          "Dremomys", "Echiothrix", "Gerbilliscus", "Grammomys", 
                          "Graphiurus", "Heimyscus", "Hybomys", "Hylomyscus", 
                          "Hylopetes", "Hystrix", "Lemniscomys", "Lenomys", 
                          "Leopoldamys", "Lophuromys", "Malacomys", "Mastomys", 
                          "Maxomys", "Menetes", "Mus", "Myomyscus", 
                          "Niviventer", "Oenomys", "Paruromys", "Pelomys",
                          "Petaurillus", "Petaurista", "Praomys", "Prionomys", 
                          "Protoxerus", "Rattus", "Ratufa", "Rhinosciurus",
                          "Rhizomys", "Saccostomus", "Steatomys", "Stochomys", 
                          "Sundamys", "Sundasciurus", "Taeromys", "Tamiops", 
                          "Taterillus", "Thryonomys", "Trichys", "Uranomys", 
                          "Vandeleuria") ~ "Rodentia",
        host_sci_name %in% c("Gliridae", "Sciuridae", "Rodentia") ~ "Rodentia",
        host_genus %in% c("Dendrogale", "Tupaia") ~ "Scandentia",
        
        host_genus %in% c("Aegypius", "Aquila", "Gyps") ~ "Accipitriformes",
        host_genus %in% c("Anas", "Anser", "Cygnus", "Tadorna") ~ "Anseriformes",
        host_genus %in% c("Hydroprogne", "Larus", "Tringa") ~ "Charadriiformes",
        host_genus == "Spilopelia" ~ "Columbiformes",
        host_genus == "Coturnix" ~ "Galliformes",
        host_genus == "Grus" ~ "Gruiformes",
        host_genus %in% c("Acridotheres", "Corvus", "Gracupica", 
                          "Pycnonotus", "Rhipidura") ~ "Passeriformes",
        host_genus == "Phalacrocorax" ~ "Suliformes")) %>%
    
    # add family for later analyses
    mutate(host_family = case_when(
      
      # Accipitriformes
      host_genus %in% c("Aegypius", "Aquila", "Gyps") ~ "Accipitridae",
      
      # Anseriformes
      host_genus %in% c("Anas", "Anser", "Cygnus", "Tadorna") ~ "Anatidae",
      
      # Artiodactyla
      host_genus %in% c("Cephalophus", "Philantomba") ~ "Bovidae",
      host_genus == "Muntiacus" ~ "Cervidae",
      host_genus %in% c("Potamochoerus", "Sus") ~ "Suidae",
      host_genus == "Tragulus" ~ "Tragulidae",
      
      # Carnivora
      host_genus == "Prionailurus" ~ "Felidae",
      host_genus == "Herpestes" ~ "Herpestidae",
      host_genus == "Mydaus" ~ "Mephitidae",
      host_genus == "Nandinia" ~ "Nandiniidae",
      host_genus == "Helarctos" ~ "Ursidae",
      host_genus %in% c("Chrotogale", "Civettictis", "Paguma", "Paradoxurus",
                        "Viverra") ~ "Viverridae",
      
      # Charadriiformes 
      host_genus %in% c("Hydroprogne", "Larus") ~ "Laridae",
      host_genus == "Tringa" ~ "Scolopacidae",
      
      # Chiroptera
      host_genus == "Craseonycteris" ~ "Craseonycteridae",
      host_genus %in% c("Coleura", "Emballonura", "Saccolaimus", "Taphozous") ~ 
        "Emballonuridae",
      host_genus == "Hipposideros" ~ "Hipposideridae",
      host_genus == "Megaderma" ~ "Megadermatidae",
      host_genus == "Miniopterus" ~ "Miniopteridae",
      host_genus %in% c("Chaerephon", "Cheiromeles", "Otomops", "Mops",
                        "Tadarida") ~ "Molossidae",
      host_genus == "Nycteris" ~ "Nycteridae",
      host_genus %in% c("Acerodon", "Balionycteris", "Casinycteris", "Chironax",
                        "Cynopterus", "Dobsonia", "Dyacopterus", "Eidolon", 
                        "Eonycteris", "Epomophorus", "Epomops", "Hypsignathus",
                        "Lissonycteris", "Macroglossus", "Megaerops",
                        "Megaloglossus", "Micropteropus", "Myonycteris", 
                        "Nanonycteris", "Neopteryx", "Nyctimene", "Penthetor",
                        "Plerotes", "Pteropus", "Rousettus", "Scotonycteris",
                        "Styloctenium", "Thoopterus") ~ "Pteropodidae",
      host_genus == "Rhinolophus" ~ "Rhinolophidae",
      host_genus == "Rhinopoma" ~ "Rhinopomatidae",
      host_genus %in% c("Glauconycteris", "Glischropus", "Hypsugo", "Kerivoula",
                        "Murina", "Myotis", "Neoromicia", "Phoniscus",
                        "Pipistrellus", "Scotoecus", "Scotophilus", 
                        "Tylonycteris") ~ "Vespertilionidae",
      
      # Columbiformes
      host_genus == "Spilopelia" ~ "Columbidae",
      
      # Eulipotyphla
      host_genus %in% c("Echinosorex", "Hylomys") ~ "Erinaceidae",
      host_genus %in% c("Crocidura", "Paracrocidura", "Suncus", "Sylvisorex") ~ 
        "Soricidae",
      
      # Galliformes
      host_genus == "Coturnix" ~ "Phasianidae",
      
      # Gruiformes
      host_genus == "Grus" ~ "Gruidae",
      
      # Lagomorpha
      host_genus == "Lepus" ~ "Leporidae",
      
      # Passeriformes
      host_genus == "Corvus" ~ "Corvidae",
      host_genus == "Pycnonotus" ~ "Pycnonotidae",
      host_genus == "Rhipidura" ~ "Rhipiduridae",
      host_genus %in% c("Acridotheres", "Gracupica") ~ "Sturnidae",
      
      # Pholidota
      host_genus %in% c("Manis", "Phataginus") ~ "Manidae",
      
      # Primates
      host_genus %in% c("Allenopithecus", "Cercocebus", "Cercopithecus",
                        "Chlorocebus", "Colobus", "Erythrocebus", "Lophocebus",
                        "Macaca", "Miopithecus", "Papio", "Procolobus",
                        "Pygathrix", "Trachypithecus") ~ "Cercopithecidae",
      host_genus %in% c("Gorilla", "Pan", "Pongo") ~ "Hominidae",
      host_genus %in% c("Hylobates", "Nomascus") ~ "Hylobatidae",
      host_genus == "Nycticebus" ~ "Lorisidae",
      host_genus == "Tarsius" ~ "Tarsiidae",
      
      # Rodentia
      host_genus == "Graphiurus" ~ "Gliridae",
      host_genus %in% c("Atherurus", "Hystrix", "Trichys") ~ "Hystricidae",
      host_genus == "Elephantulus" ~ "Macroscelididae",
      host_genus %in% c("Acomys", "Aethomys", "Apodemus", "Arvicanthis", 
                        "Bandicota", "Berylmys", "Bunomys", "Chiropodomys", 
                        "Dasymys", "Dephomys", "Echiothrix", "Gerbilliscus",
                        "Grammomys", "Heimyscus", "Hybomys", "Hylomyscus",
                        "Lemniscomys", "Lenomys", "Leopoldamys", "Lophuromys",
                        "Malacomys", "Mastomys", "Maxomys", "Mus", "Myomyscus",
                        "Niviventer", "Oenomys", "Paruromys", "Pelomys",
                        "Praomys", "Rattus", "Stochomys", "Sundamys", "Taeromys",
                        "Taterillus", "Uranomys", "Vandeleuria") ~ "Muridae",
      host_genus %in% c("Cricetomys", "Delanymys", "Prionomys", "Saccostomus", 
                        "Steatomys") ~ "Nesomyidae",
      host_genus == "Dendrohyrax" ~ "Procaviidae",
      host_genus %in% c("Belomys", "Callosciurus", "Dremomys", "Hylopetes", 
                        "Menetes", "Petaurillus", "Petaurista", "Protoxerus",
                        "Ratufa", "Rhinosciurus", "Sundasciurus", "Tamiops") ~ 
        "Sciuridae",
      host_genus == "Rhizomys" ~ "Spalacidae",
      host_genus == "Thryonomys" ~ "Thryonomyidae",
      
      # Scandentia
      host_genus %in% c("Dendrogale", "Tupaia") ~ "Tupaiidae",
      
      # Suliformes
      host_genus == "Phalacrocorax" ~ "Phalacrocoridae",
      
      TRUE ~ host_family)) |> 
    
    dplyr::relocate(c(host_family, host_order), .after = host_genus) |> 
  
    # lightly format host name--a very disparate column
    # some animals were only identified to order, family, or genus, 
    # while others were identified to species
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
