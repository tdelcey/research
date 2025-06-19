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


# -------------------------- build dynamic networks for various time window -------------------------- 

# choose the time window 

size_time_window <- 15

# build graphs 

dynamic_graph <- build_dynamic_networks_bb(
  nodes = nodes_of_the_year,
  directed_edges = edges_of_the_year,
  source_id = "source_id",
  target_id = "target_id",
  time_variable = "year",
  time_window = size_time_window,
  overlapping_window = TRUE,
  backbone_method = "statistical",
  statistical_method = "sdsm",
  alpha = 0.1,
  filter_components = TRUE,
  min_share = 0.05,
  keep_singleton = FALSE)


# ---------------------------- Summary of each graphs --------------------------- 


n_nodes <- dynamic_graph %>% 
  map(~ .x %>%
        activate(nodes) %>%
        as_tibble()) %>%
  bind_rows(.) %>%
  group_by(time_window) %>%
  summarise(n_nodes = n_distinct(source_id)) %>%
  ungroup() 

window_size <- size_time_window

# Get min and max years from the data
min_year <- min(nodes_of_the_year$year, na.rm = TRUE)
max_year <- max(nodes_of_the_year$year, na.rm = TRUE)

# Create a data frame of sliding windows
time_window <- tibble(
  window_start = min_year:(max_year - window_size + 1)) %>%
  mutate(window_end = window_start + window_size - 1,
         time_window = paste0(window_start, "-", window_end))

# Count unique documents in each time window
document_freq_by_window <- time_window %>%
  rowwise() %>%
  mutate(n_documents = nodes_of_the_year %>%
           filter(year >= window_start, year <= window_end) %>%
           summarise(n = n_distinct(source_id)) %>%
           pull(n)) %>%
  ungroup()

summary <- n_nodes %>% 
  left_join(document_freq_by_window, by = "time_window")

saveRDS(summary, here(clean_data_path, paste0("summary_", size_time_window, ".rds")))


# ---------------------------- layout the graphs --------------------------- 

## layout 

graphs_with_layout <- list()

# loop over the time windows

for (i in 1:length(dynamic_graph)) {
  
  time_window <- names(dynamic_graph)[[i]]
  message(paste0("Running Force Atlas for the ", time_window, " window."))
  
  if (i == 1) {
    graphs_with_layout[[time_window]] <-
      vite::complete_forceatlas2(
        dynamic_graph[[i]],
        first.iter = 10000,
        kgrav = 1,
        overlap.method = "repel",
        overlap.iter = 2000
      )
  } else {

    past_position <- graphs_with_layout[[i - 1]] %>% activate(nodes) %>% as.data.table()
    past_position <- past_position[, .(source_id, x, y)]
    
    tbl <- dynamic_graph[[i]] %>%
      activate(nodes) %>%
      left_join(past_position, by = "source_id")
    
    graphs_with_layout[[time_window]] <-
      vite::complete_forceatlas2(
        tbl,
        first.iter = 10000,
        kgrav = 1,
        overlap.method = "repel",
        overlap.iter = 2000
      )
  }
}


saveRDS(graphs_with_layout, here(clean_data_path, paste0("graphs_with_layout_", size_time_window, ".rds")))
#' `graphs_with_layout <- readRDS(here(clean_data_path, paste0("graphs_with_layout_", size_time_window, ".rds")))`


# ---------------------------- clustering the graphs --------------------------- 

# compute clusters 
graphs_with_cluster <- add_clusters(graphs_with_layout,
                                    clustering_method = "leiden",
                                    objective_function = "modularity",
                                    resolution = 1,
                                    seed = 123)

# merge clusters between time windows 
graphs_with_cluster <- networkflow::merge_dynamic_clusters(graphs_with_cluster,
                                                           cluster_id = 'cluster_leiden',
                                                           node_id = "source_id",
                                                           threshold_similarity  = 0.51,
                                                           similarity_type = "partial")

saveRDS(graphs_with_cluster, here(clean_data_path, paste0("graphs_with_cluster", size_time_window, ".rds")))
#' `graphs_with_cluster <- readRDS(here(clean_data_path, paste0("graphs_with_cluster", size_time_window, ".rds")))`

# ---------------------------- color the graphs ---------------------------

# color the clusters

graphs_with_color <- networkflow::color_networks(graphs_with_cluster, 
                                                 column_to_color = "dynamic_cluster_leiden")

saveRDS(graphs_with_color, here(clean_data_path, paste0("graphs_with_color_", size_time_window, ".rds")))
#' `graphs_with_color <- readRDS(here(clean_data_path, paste0("graphs_with_color_", size_time_window, ".rds")))`


# ---------------------------- plot the graphs ---------------------------

list_dynamic_clusters <- graphs_with_color %>%
  map(~ .x %>%
        activate(nodes) %>%
        as_tibble() %>%
        select(dynamic_cluster_leiden)) %>%
  bind_rows(.) %>%
  distinct(dynamic_cluster_leiden) %>% 
  mutate(label = NA)

# initialize
list_ggraph <- list()

for (i in 1:length(graphs_with_color)) {

  time_window <- names(graphs_with_color)[[i]]
  
  cat(paste0("Plotting graph for the time window:", time_window),
      fill = TRUE)
  
  # # grey community that represent less 5% of the network
  # 
  # small_clusters <- graphs_with_color[[i]] %>%
  #   activate(nodes) %>%
  #   as_tibble() %>%
  #   group_by(dynamic_cluster_leiden) %>%
  #   filter(size_cluster_leiden < 0.01) %>%
  #   pull(dynamic_cluster_leiden) %>% unique
  # 
  # graphs_with_color[[i]] <- graphs_with_color[[i]] %>%
  #   activate(nodes) %>%
  #   mutate(color = ifelse(dynamic_cluster_leiden %in% small_clusters, "lightgrey", color))
  # 
  # # grey edges
  # 
  # graphs_with_color[[i]] <- graphs_with_color[[i]] %>%
  #   activate(edges) %>%
  #   mutate(color = ifelse(dynamic_cluster_leiden %in% small_clusters, "lightgrey", color))


  # labels cluster  
  
  labels <- list_dynamic_clusters %>% 
    mutate(dynamic_cluster_leiden_label = ifelse(is.na(label), dynamic_cluster_leiden, label)) %>% 
    select(dynamic_cluster_leiden_label, dynamic_cluster_leiden) 

  labels_xy <- graphs_with_color[[i]] %>%
    activate(nodes) %>%
    as_tibble %>%
    # filter(!dynamic_cluster_leiden %in% small_clusters) %>%
    left_join(labels, by = "dynamic_cluster_leiden") %>%
    select(x , y , color, dynamic_cluster_leiden, dynamic_cluster_leiden_label, cluster_leiden) %>%
    group_by(cluster_leiden) %>%
    reframe(
      label_x = mean(x),
      label_y = mean(y),
      color = color,
      cluster_leiden = cluster_leiden, 
      dynamic_cluster_leiden_label = dynamic_cluster_leiden_label,
      dynamic_cluster_leiden = dynamic_cluster_leiden,
      
    ) %>%
    unique()
  
  gg <- ggraph(graphs_with_color[[i]], "manual", x = x, y = y) +
    geom_edge_arc0(
      aes(color = color, width = weight),
      alpha = 0.5,
      strength = 0.2,
      show.legend = FALSE
    ) +
    scale_edge_width_continuous(range = c(0.05, 1)) +
    scale_edge_colour_identity() +
    geom_point_interactive(
      aes(
        x = x,
        y = y,
        # size = n,
        fill = color,
        tooltip = paste0(title, first_author, year),
        # data_id = source_id
      ),
      size = 3,
      pch = 21,
      alpha = 0.7,
      show.legend = FALSE
    ) +
    scale_size_continuous(range = c(2, 9)) +
    scale_fill_identity() +
    geom_label_repel_interactive(
      data = labels_xy,
      aes(
        x = label_x, 
        y = label_y, 
        fill = color, 
        label = dynamic_cluster_leiden_label, 
        tooltip = "Click to see the nodes in this community",
        data_id = dynamic_cluster_leiden
      ),
      alpha = 0.9, 
      size = 4, 
      fontface = "bold"
    ) + 
    theme_void()
  
  
  # save with width and height 
  
  width <- 16/1.5
  height <- 9/1.5
  
  ggsave(
    paste0("gg_graph_fa22_", time_window, ".jpg"),
    device = "jpg",
    plot = gg,
    path = here(figures_path, "spacialization", size_time_window),
    width = width,
    height = height,
    dpi = 300
  )
  
  
  list_ggraph[[as.character(time_window)]] <- girafe(ggobj = gg,
                                                     width_svg  = width,
                                                     height_svg = height)
  
}


# save graph 

saveRDS(graphs_with_color, here(clean_data_path, paste0("graphs_with_color_", size_time_window, ".rds")))
#' `graphs_with_color <- readRDS(here(clean_data_path, paste0("graphs_with_color_", size_time_window, ".rds")))`

# save data for shiny app  
# plots 

saveRDS(list_ggraph, here(app_path, paste0("list_ggraph_", size_time_window, ".rds")))

# corpus
corpus <- graphs_with_color %>% 
  map(~ .x %>% 
        activate(nodes) %>% 
        as_tibble()) %>%
  bind_rows() %>% 
  mutate(first_year = str_extract(time_window, "^\\d{4}"),
         last_year = str_extract(time_window, "\\d{4}$")) %>%
  select(source_id, title, year, time_window, first_year, last_year, first_author, journal, n, cluster_leiden, dynamic_cluster_leiden) %>% 
  unique()

saveRDS(corpus, here(app_path, paste0("corpus_", size_time_window, ".rds")))




# ----------------------------------- analysis of 5 main clusters ---------------------------- #


graphs_with_cluster <- readRDS(here(clean_data_path, paste0("graphs_with_cluster_", size_time_window, ".rds")))


time_windows <- c("1971-1985",
                  "1979-1993",
                  "1988-2002",
                  "1996-2010")

# filter the list of graphs

selected_graphs <- graphs_with_cluster[names(graphs_with_cluster) %in% time_windows]

#labels

list_dynamic_clusters <- selected_graphs %>%
  map(~ .x %>%
        activate(nodes) %>%
        as_tibble() %>%
        select(dynamic_cluster_leiden)) %>%
  bind_rows(.) %>%
  distinct(dynamic_cluster_leiden)

library(tibble)

tribble_label <- tribble(
  ~dynamic_cluster_leiden, ~cluster_label,                    ~color,
  # 1971
  "cl_1",    "FE / options",                                  "#bcbddc",
  "cl_3",    "FE / theory on efficiency",                     "#6baed6",
  "cl_4",    "FE / corporate",                                "lightblue",
  "cl_5",    "ED / accounting",                               "#e6550d",
  "cl_6",    "EM / money",                                    "#006d2c",
  "cl_8",    "EM / commodities",                              "#fdd835",
  "cl_9",    "FE / structure",                                "#756bb1",
  "cl_11",   "EM / betting",                                  "#74c476",
  "cl_12",   "ED / law",                                      "#fd8d3c",
  "cl_15",   "FE / early asset pricing",                      "#6baed6",
  
  # 1979
  "cl_31",   "FE / predictability & volatility",              "#6baed6",
  "cl_42",   "EM / foreign",                                  "#238b45",
  "cl_81",   "ED / strategy",                                 "#a63603",
  "cl_88",   "FE / structure",                                "#756bb1",
  "cl_98",   "EM / housing",                                  "#a1d99b",
  "cl_109",  NA,                                              "grey",
  "cl_111",  "ED / insurance",                                "#fdd0a2",
  
  # 1988
  "cl_148",  "ED / social responsability",                    "#fdae6b",
  "cl_161",  "FE / technical analysis",                       "#9e9ac8",
  "cl_170",  "FE / fund performance",                         "#bcbddc",
  "cl_210",  "EM / housing",                                  "#a1d99b",
  "cl_224",  "ED / marketing",                                "#fdd0a2",
  "cl_233",  "EM / international",                            "#238b45",
  "cl_236",  "FE / experimental",                             "#6a51a3",
  
  # 1996
  "cl_250",  "EM / critical perspective",                     "#999999",
  "cl_252",  "EM / international",                            "#c2e699",
  "cl_258",  "EM / betting",                                  "#74c476",
  "cl_285",  "ED / marketing",                                "#fdd0a2",
  "cl_292",  "FE / technical analysis",                       "#9e9ac8",
  "cl_308",  "EM / energy",                                   "#fee391",
  "cl_329",  "FE / information flow",                         "#08519c",
  "cl_347",  "FE / behavioral",                               "#6baed6",
  "cl_353",  NA,                                              "grey",
)

# color

df_color <- tribble_label %>% distinct(dynamic_cluster_leiden, color) 

selected_graphs <- networkflow::color_networks(selected_graphs,
                                               column_to_color = "dynamic_cluster_leiden",
                                               color = df_color)

for (i in 1:length(selected_graphs)) {


graph <- selected_graphs[[i]]
window <- names(selected_graphs)[[i]]
cat(paste0("Running analysis for the ", window, " window."))
# analysis of clusters

data <- graph %>%
  activate(nodes) %>%
  as_tibble() %>% 
  left_join(tribble_label, by = c("dynamic_cluster_leiden" = "dynamic_cluster_leiden")) 

  # %>% 
  # filter(!is.na(cluster_label)) 

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
  group_by(dynamic_cluster_leiden, cluster_label) %>%
  summarise(
    n_nodes = n_distinct(source_id),
    min_year = min(year, na.rm = TRUE),
    max_year = max(year, na.rm = TRUE)) %>%
  left_join(tfidf_words, by = "dynamic_cluster_leiden") %>%
  left_join(tfidf_journals, by = "dynamic_cluster_leiden") %>%
  arrange(desc(n_nodes)) %>% 
  ungroup() %>%
  select(-dynamic_cluster_leiden)

saveRDS(summary_table, here(clean_data_path, paste0("summary_table_dynamic_", window, ".rds")))

}




# ----------------------------- plot the clusters -----------------------------


for (i in 1:length(selected_graphs)) {
  
  time_window <- names(selected_graphs)[[i]]
  
  cat(paste0("Plotting graph for the time window:", time_window),
      fill = TRUE)
  
  graph <- selected_graphs[[i]] %>%
    activate(nodes) %>%
    left_join(tribble_label %>% select(-color), by = "dynamic_cluster_leiden") %>% 
    mutate(cluster_label2 = paste0(dynamic_cluster_leiden, "-", cluster_label)) 
  
  # grey edges 
  to_grey <- graph %>% filter(is.na(cluster_label)) %>% pull(dynamic_cluster_leiden)
  
  graph <- graph %>%
    activate(edges) %>%
    mutate(color = ifelse(cluster_leiden %in% to_grey, "grey", color))
  
  
  labels_xy <- graph %>%
    activate(nodes) %>%
    as_tibble %>%
    select(x , y , color, dynamic_cluster_leiden, cluster_leiden, cluster_label) %>%
    group_by(cluster_leiden) %>%
    reframe(
      label_x = mean(x),
      label_y = mean(y),
      color = color,
      cluster_leiden = cluster_leiden, 
      dynamic_cluster_leiden = dynamic_cluster_leiden,
      cluster_label = cluster_label,
    ) %>%
    unique()
  
  gg <- ggraph(graph, "manual", x = x, y = y) +
    geom_edge_arc0(
      aes(color = color, width = weight),
      alpha = 0.5,
      strength = 0.2,
      show.legend = FALSE
    ) +
    scale_edge_width_continuous(range = c(0.05, 1)) +
    scale_edge_colour_identity() +
    geom_point_interactive(
      aes(
        x = x,
        y = y,
        # size = n,
        fill = color,
        tooltip = paste0(title, first_author, year),
        # data_id = source_id
      ),
      size = 3,
      pch = 21,
      alpha = 0.7,
      show.legend = FALSE
    ) +
    scale_size_continuous(range = c(2, 9)) +
    scale_fill_identity() +
    geom_label_repel_interactive(
      data = labels_xy,
      aes(
        x = label_x, 
        y = label_y, 
        fill = color, 
        label = cluster_label, 
        tooltip = "Click to see the nodes in this community",
        data_id = dynamic_cluster_leiden
      ),
      alpha = 0.9, 
      size = 4, 
      fontface = "bold"
    ) + 
    theme_void()
  
  
  # save with width and height 
  
  width <- 16/1.5
  height <- 9/1.5
  
  ggsave(
    paste0("gg_graph_fa22_", time_window, ".jpg"),
    device = "jpg",
    plot = gg,
    path = here(figures_path),
    width = width,
    height = height,
    dpi = 300
  )
  
}

saveRDS(selected_graphs, here(clean_data_path, paste0("selected_graphs_", size_time_window, ".rds")))
