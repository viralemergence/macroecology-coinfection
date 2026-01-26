## For more information about targets see https://books.ropensci.org/targets

## Load packages
suppressPackageStartupMessages(source("packages.R"))
## Load functions stored in /R 
targets::tar_source()

# Set project-wide options
tar_option_set(
  resources = tar_resources(
    qs = tar_resources_qs(preset = "fast")),
  format = "qs"
)

data_input_targets <- tar_plan(
  
  ## read in PREDICT data 
  tar_file(pcr_tests_gz, "data/PREDICT_PCR_Tests.csv.gz"),
  tar_file(animals_csv, "data/PREDICT_Animals_Sampled.csv"),
  
  pcr_raw = vroom(pcr_tests_gz) %>% 
    janitor::clean_names(),
  animals_raw = readr::read_csv(animals_csv) %>% 
    janitor::clean_names() %>% 
    rename(captivity_status = animal_class),
  
  # data used to classify cave roosting drawn from 
  # https://www.mdpi.com/1424-2818/9/3/35 and IUCN Red List
  tar_file_read(roosting, "data/roosting_data.csv", read_csv(file = !!.x)),
  
  # set a color palette for some figures
  pal = RColorBrewer::brewer.pal(8, "Spectral"),
  
  AmyTheme = 
    theme_bw(base_size = 14) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          strip.background = element_rect(fill = "white"), 
          legend.position = "none") 
  
  
)

data_processing_targets <- tar_plan(
  
  pcr = clean_pcr_data(pcr_raw),
  
  # join all testing data with the animal data
  pcr_all = pcr %>% 
    mutate(infection = case_when(!is.na(virus) ~ 1, 
                                 is.na(virus) ~ 0)) %>% 
    left_join(animals_raw, 
              by = c("predict_sample_id" = "predict_individual_id"), 
              suffix = c("", ".dup")) %>% 
    dplyr::select(!ends_with(".dup")) %>% 
    # divide into broad regions
    mutate(geo_region = if_else(longitude < 60, 
                                "Africa and West Asia", 
                                "South, East, Southeast Asia")),
  
  # calculate number of virus families tested, for each animal
  family_testing = pcr_all %>% 
    distinct(predict_sample_id, viral_family_tested) %>% 
    group_by(predict_sample_id) %>% 
    dplyr::summarise(n_viral_families_tested = n()),
  
  # number of unique animals with testing data
  n_unique_all = pcr_all$predict_sample_id %>% unique %>% length, #65662
  
  n_unique_bats = pcr_all %>% 
    dplyr::filter(taxa_group == "bats") %>% 
    dplyr::pull(predict_sample_id) %>% 
    unique %>% 
    length,
  
  n_unique_rodents = pcr_all %>% 
    dplyr::filter(taxa_group == "rodents") %>% 
    dplyr::pull(predict_sample_id) %>% 
    unique %>% 
    length,
  
  # filter the joined testing/animal data to just the positives
  pcr_pos = pcr_all %>% 
    dplyr::filter(infection == 1),
  
  # prep data for later GLMMs
  coinf_df = create_coinf_df(pcr_pos, family_testing),
  model_data_all = prep_model_data_all(coinf_df),
  model_data_bats = prep_model_data_bats(model_data_all, roosting),
  model_data_rodents = prep_model_data_rodents(model_data_all),
  
  # unique virus-animal combos and coinfection status
  pos_final = create_pos_final(pcr_pos),
  
  # number of positives by virus family
  total_pos = pos_final %>% 
    group_by(viral_family) %>% 
    count %>% 
    mutate(number = "Positive"),
  
  total_pos_bats = pos_final %>% 
    dplyr::filter(taxa_group == "bats") %>% 
    group_by(viral_family) %>% 
    count %>% 
    mutate(number = "Positive"),
  
  total_pos_rodents = pos_final %>% 
    dplyr::filter(taxa_group == "rodents") %>% 
    group_by(viral_family) %>% 
    count %>% 
    mutate(number = "Positive"),
  
  # prep virus family coinfection matrix for network analyses
  M = prep_matrix_data(pos_final, 
                       vir_level = "viral_family", 
                       total_pos),
  M_bats = prep_matrix_data(pos_final %>% 
                              dplyr::filter(taxa_group == "bats"), 
                            vir_level = "viral_family",
                            total_pos_bats),
  M_rodents = prep_matrix_data(pos_final %>% 
                                 dplyr::filter(taxa_group == "rodents"), 
                               vir_level = "viral_family",
                               total_pos_rodents),
  
  # prep individual virus coinfection matrix
  MV = prep_matrix_data(pos_final, 
                       vir_level = "virus"),
  
  # prep data for map plotting
  map_data_all = prep_map_data_all(pcr),
  map_data_pos = prep_map_data_pos(pos_final),
  # bizarrely, have to re-run every time or it throws an error
  tar_target(map_data_pos_taxa, 
             prep_map_data_pos_taxa(pos_final),
             cue = tar_cue(mode = "always"))
  
)

analysis_targets <- tar_plan(
  
  viral_fams = total_pos$viral_family,
  viral_fams_bats = total_pos_bats$viral_family,
  viral_fams_rodents = total_pos_rodents$viral_family,
  
  sims_bats = simulate_coinf(pcr_all, "bats"),
  sims_rodents = simulate_coinf(pcr_all, "rodents"),
  sims_birds = simulate_coinf(pcr_all, "birds"),

  # create graph object to visualize coinfection by viral family
  g = prep_vir_fam_graph(M, total_pos, viral_fams, pal),
  g_bats = prep_vir_fam_graph(M_bats, total_pos_bats, viral_fams_bats, pal),
  g_rodents = prep_vir_fam_graph(M_rodents, total_pos_rodents, 
                                 viral_fams_rodents, pal),
  
  # create graph object to visualize coinfection by individual viruses
  g_vir = prep_vir_graph(MV, pos_final, pal, sparse = TRUE),
  
  binom_tests_vir_fam = run_binom_tests(pcr_pos, n_unique_all,
                                        vir_level = "viral_family",
                                        observed_mat = M),
  
  binom_tests_vir_fam_bats = run_binom_tests(pcr_pos %>% 
                                               dplyr::filter(taxa_group == "bats"), 
                                             n_unique_bats,
                                             vir_level = "viral_family",
                                             observed_mat = M_bats),
  
  binom_tests_vir_fam_rodents = run_binom_tests(pcr_pos %>% 
                                                  dplyr::filter(taxa_group == "rodents"), 
                                                n_unique_rodents,
                                                vir_level = "viral_family",
                                                observed_mat = M_rodents),

  model_list = run_models(model_data_all, model_data_bats, model_data_rodents)
  
)

plot_targets <- tar_plan(
  
  # Figure 1 (2 panels)
  fig_coinf_per_taxa = plot_coinf_per_taxa(pos_final, 
                                           pal = c("#9e9ac8", "#54278f")),
  fig_map_pos = plot_map_pos(map_data_pos, pal = c("#9e9ac8", "#54278f")),
  fig_1 = plot_fig_1(fig_coinf_per_taxa, fig_map_pos),
  
  # Figure 2 (2 panels)
  vir_fam_network_panel = plot_vir_fam_network_panel(g, g_bats, g_rodents,
                                                     add_stars = T),
  model_coefs_panel = plot_model_coefs_panel(model_list),
  fig_2 = plot_fig_2(vir_fam_network_panel, model_coefs_panel),
  
  # Figure 3
  fig_3 = plot_vir_network(g_vir),
  
  # Figure S1
  fig_s1 = plot_map_all(map_data_all),
  
  # Figure X
  fig_s2 = plot_map_pos_taxa(map_data_pos_taxa),
  
  # Figure S3
  fig_s3 = plot_sim_coinf_panel(pcr_all, sims_bats, sims_rodents, sims_birds),
  
)

outputs_targets <- tar_plan(
  
  fig_1_tiff = ggsave("figures/fig_1.tiff", fig_1, height = 11, width = 8.5, 
                      units = "in", dpi = 500, compression = "lzw"),
  fig_1_png = ggsave("figures/fig_1.png", fig_1, height = 11, width = 8.5, 
                     units = "in", dpi = 500),
  
  fig_2_tiff = ggsave("figures/fig_2.tiff", fig_2, height = 19, width = 16, 
                      units = "in", dpi = 500, compression = "lzw"),
  fig_2_png = ggsave("figures/fig_2.png", fig_2, height = 19, width = 16, 
                     units = "in", dpi = 500),
  
  fig_3_tiff = ggsave("figures/fig_3.tiff", fig_3, height = 10, width = 12,
                      units = "in", dpi = 500, compression = "lzw"),
  fig_3_png = ggsave("figures/fig_3.png", fig_3, height = 10, width = 12, 
                     units = "in", dpi = 500),
  
  fig_s1_tiff = ggsave("figures/fig_s1.tiff", fig_s1, height = 4, width = 7, 
                       units = "in", dpi = 500, compression = "lzw"),
  fig_s1_png = ggsave("figures/fig_s1.png", fig_s1, height = 4, width = 7, 
                       units = "in", dpi = 500),
  
  fig_s2_tiff = ggsave("figures/fig_s2.tiff", fig_s2, height = 5, width = 7, 
                      units = "in", dpi = 500, compression = "lzw"),
  fig_s2_png = ggsave("figures/fig_s2.png", fig_s2, height = 5, width = 7, 
                     units = "in", dpi = 500),
  
  fig_s3_tiff = ggsave("figures/fig_s3.tiff", fig_s3, height = 3, width = 10, 
                       units = "in", dpi = 500, compression = "lzw"),
  fig_s3_png = ggsave("figures/fig_s3.png", fig_s3, height = 3, width = 10, 
                      units = "in", dpi = 500),
  
)

## Report
report_targets <- tar_plan(
  
  tar_render(
    summary_calculations, path = "reports/summary_calculations.Rmd",
    output_dir = "reports", knit_root_dir = here::here()
  )
)

list(
  data_input_targets,
  data_processing_targets,
  analysis_targets,
  plot_targets,
  outputs_targets,
  report_targets
)
