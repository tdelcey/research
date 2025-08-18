source(here::here("fama_1970", "paths_and_packages.R"))

# load data 

corpus <- readRDS(here(clean_data_path, "corpus.rds"))
ref_corpus <- readRDS(here(clean_data_path,"ref_corpus.rds"))
ref_corpus_info <- readRDS(here(clean_data_path,"ref_corpus_info.rds"))

#select window
nodes_of_the_year <- corpus %>%
  rename(source_id = id) %>%
  filter(!is.na(source_id), 
         year < 2011) %>%
  as.data.table()

edges_of_the_year <- ref_corpus %>%
  filter(!is.na(source_id),
         !is.na(target_id)) %>%
  filter(source_id %in% nodes_of_the_year$source_id) %>%
  unique() %>%
  as.data.table()

graph <- build_dynamic_networks_bb(
  nodes = nodes_of_the_year,
  directed_edges = edges_of_the_year,
  source_id = "source_id",
  target_id = "target_id",
  backbone_method = "statistical",
  statistical_method = "sdsm",
  alpha = 0.05,
  filter_components = TRUE,
  min_share = 0.05,
  keep_singleton = FALSE)


#' `saveRDS(graph, here(clean_data_path, "graph.rds"))`

#### layout #### 

graph <- readRDS(here(clean_data_path, "graph.rds"))

graph_with_layout <- vite::complete_forceatlas2(graph,
                                                kgrav = 1,
                                                first.iter = 10000,
                                                overlap.method = "repel",
                                                overlap.iter = 2000)


#' `saveRDS(graph_with_layout, here(clean_data_path, "graph_with_layout.rds"))`
#' `graph_with_layout <- readRDS(here(clean_data_path, "graph_with_layout.rds"))`


graph_with_cluster <- add_clusters(
  graph_with_layout,
  clustering_method = "leiden",
  objective_function = "modularity",
  resolution = 1,
  seed = 123) %>% 
  # rename for consistency with dynamic analysis
  activate(nodes) %>% 
  mutate(dynamic_cluster_leiden = cluster_leiden) 


#' `saveRDS(graph_with_cluster, here(clean_data_path, "graph_with_cluster.rds"))`
#' `graph_with_cluster <- readRDS(here(clean_data_path, "graph_with_cluster.rds"))`

#### color #### 

### set a palette of color 

# colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
#             "#CC6677", "#882255", "#AA4499", "#117733", "lightblue", "darkred", "#334185","#DDCC77")
# 

# create a tribble with new name 

cluster_label <- tribble(
  ~dynamic_cluster_leiden, ~cluster_label, ~meta_cluster,           ~color,
  "01", "Money",             "Exploring markets",                     "#006d2c",
  "02", "Asset pricing",     "Financial Economics",                   "#756bb1",
  "03", "Information and microstructure",    "Financial Economics",                   "#9e9ac8",
  "04", "Accounting",        "Exploring disciplines",                 "#e6550d",
  "05", "Strategy",          "Exploring disciplines",                 "#a63603",
  "06", "Foreign",           "Exploring markets",                     "#238b45",
  "07", "Law",               "Exploring disciplines",                 "#fd8d3c",
  "08", "Commodities",       "Exploring markets",                     "#fdd835",
  "09", "Betting",           "Exploring markets",                     "#74c476",
  "10", "Forecasting",       "Financial Economics",                   "#6baed6",
  "11", "Marketing",         "Exploring disciplines",                 "#fdd0a2",
  "12", "Varia",             NA,                                      "#bdbdbd",
  "13", "Varia",             NA,                                      "#bdbdbd"
)

df_color <- cluster_label %>% distinct(dynamic_cluster_leiden, color)
  
  
# color graphs

graph_with_color <- networkflow::color_networks(graph_with_cluster,
                                                column_to_color = "dynamic_cluster_leiden",
                                                color = df_color)


# add labels to the graph

graph_with_color <- graph_with_color %>%
  activate(nodes) %>%
  left_join(cluster_label %>% select(-color), by = "dynamic_cluster_leiden")

# grey edges 
to_grey <- cluster_label %>% filter(cluster_label == "Varia") %>% pull(dynamic_cluster_leiden)

graph_with_color <- graph_with_color %>%
  activate(edges) %>%
  mutate(color = ifelse(cluster_leiden %in% to_grey, "grey", color)) 


#### plotting graph ####

labels_xy <- graph_with_color %>% #graphs_with_layout[[i]] %>%
  activate(nodes) %>%
  as_tibble %>%
  group_by(dynamic_cluster_leiden) %>%
  reframe(label_x = mean(x),
          label_y = mean(y),
          color = color,
          cluster_label = cluster_label
          ) %>%
  filter(cluster_label != "Varia") %>% 
  unique()


gg <- ggraph(graph_with_color, "manual", x = x, y = y) +
  geom_edge_arc0(
    aes(color = color, width = weight),
    alpha = 0.5,
    strength = 0.2,
    show.legend = FALSE
  ) +
  scale_edge_width_continuous(range = c(0.1, 0.8)) +
  scale_edge_colour_identity() +
  geom_point_interactive(
    aes(
      x = x,
      y = y,
      fill = color,
      tooltip = paste0(title, "-", first_author, "-", year),
      data_id = source_id
    ),
    size = 1.8,
    pch = 21,
    alpha = 0.7,
    show.legend = FALSE
  ) +
  scale_size_continuous(range = c(1, 5)) +
  scale_fill_identity() +
  theme_void()

# estimate the center of each cluster and remove outliers

nodes_df <- graph_with_color %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  filter(!is.na(meta_cluster)) %>%
  group_by(meta_cluster) %>%
  mutate(
    cx = mean(x),
    cy = mean(y),
    dist_to_center = sqrt((x - cx)^2 + (y - cy)^2),
    q90 = quantile(dist_to_center, 0.9)
  ) %>%
  filter(dist_to_center <= q90)


gg_hull <- gg +
  geom_mark_hull(
    data = nodes_df,
    aes(x = x, y = y, group = meta_cluster, label = meta_cluster),
    concavity = 50,
    alpha = 0.2,
    fill = "grey90",
    colour = "grey40"
  )

gg_label <- gg_hull +
  geom_label_repel(
    data = labels_xy,
    aes(
      x = label_x,
      y = label_y,
      fill = color,
      label = cluster_label,
      alpha = 0.8,
    ),
    alpha = 0.8,
    size = 5,
    fontface = "bold"
  )
  

ggsave(
  paste0("coupling_static", 
         # time_window,
         ".jpg"),
  device = "jpg",
  plot = gg_label,
  path = here(figures_path),
  width = 8*1.5,
  height = 4.5*1.5,
  dpi = 300)

gg_interactive <- girafe(
  ggobj = gg_hull +
  geom_label_interactive(
    data = labels_xy,
    aes(
      x = label_x,
      y = label_y,
      fill = color,
      label = cluster_label,
      tooltip = "Click to show documents in the community",
      data_id = dynamic_cluster_leiden,
    ),
    alpha = 0.8,
    size = 3,
    fontface = "bold"
  ),
  width_svg  = 8*1.5,
  height_svg = 4.5*1.5
)

saveRDS(graph_with_color, here(clean_data_path, "graph_with_color.rds"))
saveRDS(graph_with_color %>% activate(nodes) %>% as_tibble, here(app_path, "corpus_all_periods.rds"))
saveRDS(gg_interactive, here(app_path, "graph_all_period.rds"))


# analysis of clusters 

graph_with_color <- readRDS(here(clean_data_path, "graph_with_color.rds"))

data <- graph_with_color %>%
  activate(nodes) %>%
  as_tibble() %>% 
  filter(!is.na(cluster_label)) 

tidy_titles <- data %>%
  select(source_id, dynamic_cluster_leiden, cluster_label, title) %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word")

# TF-IDF sur les titres
tfidf_words <- tidy_titles %>%
  count(dynamic_cluster_leiden, word, sort = TRUE) %>%
  bind_tf_idf(word, dynamic_cluster_leiden, n) %>%
  group_by(dynamic_cluster_leiden) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  summarise(top_words = paste(word, collapse = ", "))

# TF-IDF sur les journaux
tfidf_journals <- data %>%
  count(dynamic_cluster_leiden, journal) %>%
  bind_tf_idf(journal, dynamic_cluster_leiden, n) %>%
  group_by(dynamic_cluster_leiden) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  summarise(top_journals = paste(journal, collapse = ", "))

# Regrouper les infos par cluster
summary_table <- data %>%
  group_by(dynamic_cluster_leiden, cluster_label, meta_cluster) %>%
  summarise(
    n_nodes = n_distinct(source_id),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)) %>%
  left_join(tfidf_words, by = "dynamic_cluster_leiden") %>%
  left_join(tfidf_journals, by = "dynamic_cluster_leiden") %>%
  arrange(desc(n_nodes)) %>% 
  ungroup() %>%
  select(-dynamic_cluster_leiden)

saveRDS(summary_table, here(clean_data_path, "summary_table_static.rds"))

