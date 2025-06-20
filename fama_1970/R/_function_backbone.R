build_dynamic_networks_bb <- function(nodes,
                                      directed_edges,
                                      source_id,
                                      target_id,
                                      time_variable = NULL,
                                      time_window = NULL,
                                      backbone_method = c("statistical", "structured"),
                                      statistical_method = c("sdsm", "fdsm", "fixedfill", "fixedfrow", "fixedcol"),
                                      alpha = alpha,
                                      coupling_measure = c("coupling_angle", "coupling_strength", "coupling_similarity"),
                                      edges_threshold = 1,
                                      overlapping_window = FALSE,
                                      compute_size = FALSE,
                                      keep_singleton = FALSE,
                                      filter_components = FALSE,
                                      ...,
                                      verbose = TRUE) {
  
  size <- node_size <- N <- method <- NULL

  # Making sure the table is a datatable
  nodes <- data.table::data.table(nodes)
  directed_edges <- data.table::data.table(directed_edges)
  
  # Checking the methods
  backbone_methods = c("statistical", "structured")
  
  coupling_measures <- c("coupling_angle",
                         "coupling_strength",
                         "coupling_similarity")
  
  statistical_methods <- c("sdsm", "fdsm", "fixedfill", "fixedfrow", "fixedcol")
  
  
  if (length(backbone_method) > 1) {
    cli::cli_abort(
      c(
        "You did not choose any method for extracting the backbone. You have to choose between: ",
        "*" = "\"statistical\";",
        "*" = "\"structured\"."
      )
    )
  }
  
  if (!backbone_method %in% backbone_methods) {
    cli::cli_abort(
      c(
        "You did not choose any method for extracting the backbone. You have to choose between: ",
        "*" = "\"statistical\";",
        "*" = "\"structured\";"
      )
    )
  }
  
  # check various setting for the structured methods
  
  if (backbone_method == "structured") {
    
    # Checking various problems: lacking method,
    if (length(coupling_measure) > 1) {
      cli::cli_abort(
        c(
          "For structured backbone extraction, you have to choose a coupling measure among: ",
          "*" = "\"coupling_angle\";",
          "*" = "\"coupling_strength\";",
          "*" = "\"coupling_similarity\"."
        )
      )
    }
    
    if (!coupling_measure %in% coupling_measures) {
      cli::cli_abort(
        c(
          "For structured backbone extraction, you have to choose a coupling measure among: ",
          "*" = "\"coupling_angle\";",
          "*" = "\"coupling_strength\";",
          "*" = "\"coupling_similarity\"."
        )
      )
    }
    
  }
  
  # check various setting for the statistical methods
  if (backbone_method == "statistical") {
    # check if a model is given
    if (length(statistical_method) > 1) {
      cli::cli_abort(
        c(
          "For statistical backbone extraction, you have to choose a model: ",
          "*" = "\"sdsm\";",
          "*" = "\"fdsm\";",
          "*" = "\"fixedfill\".",
          "*" = "\"fixedfrow\".",
          "*" = "\"fixedcol\"."
        )
      )
    }
    
    if (!statistical_method %in% statistical_methods) {
      cli::cli_abort(
        c(
          "For statistical backbone extraction, you have to choose a model: ",
          "*" = "\"sdsm\";",
          "*" = "\"fdsm\";",
          "*" = "\"fixedfill\".",
          "*" = "\"fixedfrow\".",
          "*" = "\"fixedcol\"."
        )
      )
    }
    
    # check if alpha is given
    if (is.null(alpha)) {
      cli::cli_abort(
        "For statistical backbone extraction, you have to choose a signifiance level alpha."
      )
    }
    
  }
  
  # warning if the source_id is not unique
  if (nodes[, .N, source_id, env = list(source_id = source_id)][N > 1, .N] > 0) {
    cli::cli_alert_warning(
      "Some identifiers in your column {.field {source_id}} in your nodes table are not unique. You need only one row per node."
    )
  }
  
  # check settings for intertemporal networks
  if (!is.null(time_window) & is.null(time_variable)) {
    cli::cli_abort(
      "You cannot have a {.emph time_window} if you don't give any column with a temporal variable. Put a column in {.emph time_variable} or remove the {.emph time_window}."
    )
  }

  
  # VERBOSE
  
  if (verbose == TRUE) {
    if (length(statistical_method > 0))
      cli::cli_alert_info(paste(
        "We extract the network backbone using the",
        backbone_method,
        "method."
      ))
    
    if (keep_singleton == FALSE)
      cli::cli_alert_info("Keep_singleton == FALSE: removing the nodes that are alone with no edge. \n\n")
  }
  
  
  # CHECKING THE DATA
  
  # NODES 
  nodes_coupling <- data.table::copy(nodes)
  nodes_coupling[, source_id := as.character(source_id), env = list(source_id = source_id)]
  
  if (is.null(time_variable)) {
    time_variable <- "fake_column"
    nodes_coupling[, time_variable := 1, env = list(time_variable = time_variable)]
  }
  
  
  if (!target_id %in% colnames(nodes_coupling) &
      compute_size == TRUE) {
    cli::cli_abort(
      "You don't have the column {.field {target_id}} in your nodes table. Set {.emph compute_size} to {.val FALSE}."
    )
  }
  
  if (compute_size == TRUE) {
    nodes_coupling[, target_id := as.character(target_id), env = list(target_id = target_id)]
  }
  
  # EDGES 
  
  edges <- data.table::copy(directed_edges)
  edges <- edges[, .SD, .SDcols = c(source_id, target_id)] # we keep only the columns we need
  edges[, c(source_id, target_id) := lapply(.SD, as.character), .SDcols = c(source_id, target_id)] # we need to have character columns
  
  
  
  ######################### Dynamics networks *********************
  
  # define the time window
  nodes_coupling <- nodes_coupling[order(time_variable), env = list(time_variable = time_variable)]
  nodes_coupling[, time_variable := as.integer(time_variable), env = list(time_variable = time_variable)]
  
  first_year <- nodes_coupling[, min(as.integer(time_variable)), env = list(time_variable = time_variable)]
  last_year <- nodes_coupling[, max(as.integer(time_variable)), env = list(time_variable = time_variable)]
  
  
  if (!is.null(time_window)) {
    if (last_year - first_year + 1 < time_window) {
      cli::cli_alert_warning(
        "Your time window is larger than the number of distinct values of {.field {time_variable}}"
      )
    }
  }
  
  if (is.null(time_window)) {
    all_years <- first_year
    time_window <- last_year - first_year + 1
  } else {
    if (overlapping_window == TRUE) {
      last_year <- last_year - time_window + 1
      all_years <- first_year:last_year
    } else {
      all_years <- seq(first_year, last_year, by = time_window)
      if (all_years[length(all_years)] + (time_window - 1) > last_year) {
        cli::cli_warn(
          "Your last network is shorter than the other(s) because the cutting by time window does not give a round count.
                The last time unity in your data is {.val {last_year}}, but the upper limit of your last time window is
                {.val {all_years[length(all_years)] + (time_window - 1)}}."
        )
      }
    }
  }
  
  # Prepare our list
  tbl_coup_list <- list()
  
  for (year in all_years) {
    nodes_of_the_year <- nodes_coupling[time_variable >= year &
                                          time_variable < (year + time_window), env = list(time_variable = time_variable, year = year)]
    
    if (time_variable != "fake_column") {
      nodes_of_the_year[, time_window := paste0(year, "-", year + time_window - 1), env = list(year = year)]
      
      if (verbose == TRUE)
        cli::cli_h1(
          "Generation of the network for the {.val {year}}-{.val {year + time_window - 1}} time window."
        )
    } else {
      nodes_of_the_year <- nodes_of_the_year[, -c("fake_column")]
    }
    
    edges_of_the_year <- edges[source_id %in% nodes_of_the_year[, source_id], env = list(source_id = source_id)]
    
    # size of nodes
    if (compute_size == TRUE) {
      nb_cit <- edges_of_the_year[source_id %in% nodes_of_the_year[, source_id], .N, target_id, env = list(source_id = source_id, target_id = target_id)]
      
      colnames(nb_cit)[colnames(nb_cit) == "N"] <- "node_size"
      
      if ("node_size" %in% colnames(nodes_coupling) == TRUE)
      {
        cli::cli_warn(
          "You already have a column name {.field node_size}. The content of the column will be replaced."
        )
      }
      
      nodes_of_the_year <- data.table::merge.data.table(nodes_of_the_year,
                                                        nb_cit,
                                                        by = target_id,
                                                        all.x = TRUE)
      
      nodes_of_the_year[is.na(node_size), node_size := 0]
    }
    
    
    
    # backbone
    
    if (backbone_method == "statistical") {
      # prepare backbone function
      backbone_functions <-
        data.table::data.table(
          biblio_function = c(
            rlang::expr(backbone::sdsm),
            rlang::expr(backbone::fdsm),
            rlang::expr(backbone::fixedfrow),
            rlang::expr(backbone::fixedcol),
            rlang::expr(backbone::fixedfill)
          ),
          method = c("sdsm", "fdsm", "fixedfrow", "fixedcol", "fixedfill")
        )
      
      backbone_functions <- backbone_functions[method == statistical_method][["biblio_function"]][[1]]
      
      # Evaluate the expression and catch internal errors to backbone package
      
      tryCatch({
        # using backbone with edgelist is simpler but lead to error in backbone function
        edges_of_the_year <-
          rlang::expr((!!backbone_functions)(
            B = as.data.frame(edges_of_the_year),
            alpha = rlang::inject(alpha)
          )) %>%
          eval() %>%
          as.data.table()
        
      }, error = function(e) {
        stop(
          "The backbone function failed with an error. Read the backbone documentation for more information. Error message: ",
          e$message
        )
      })
    }
    
    
    # coupling
    if (backbone_method == "structured") {
      biblio_functions <-
        data.table::data.table(
          biblio_function = c(
            rlang::expr(biblionetwork::biblio_coupling),
            rlang::expr(biblionetwork::coupling_strength),
            rlang::expr(biblionetwork::coupling_similarity)
          ),
          method = c(
            "coupling_angle",
            "coupling_strength",
            "coupling_similarity"
          )
        )
      
      biblio_function <- biblio_functions[method == coupling_measure][["biblio_function"]][[1]]
      
      # evaluate the expression and catch internal errors to biblionetwork package
      
      tryCatch({
        edges_of_the_year <-
          rlang::expr((!!biblio_function)(
            dt = edges_of_the_year,
            source = rlang::inject(source_id),
            ref = rlang::inject(target_id),
            weight_threshold = rlang::inject(edges_threshold)
          )
          ) %>%
          eval()
        
        
        
      }, error = function(e) {
        stop(
          "The coupling function failed with an error. Read the biblionetwork documentation for more information. Error message: ",
          e$message
        )
      })
      
    }
    
    
    edges_of_the_year[, source_id := from]
    edges_of_the_year[, target_it := to]
    
    # remove nodes with no edges
    if (keep_singleton == FALSE) {
      nodes_of_the_year <- nodes_of_the_year[source_id %in% edges_of_the_year$from |
                                               source_id %in% edges_of_the_year$to, env = list(source_id = source_id)]
    }
    
    # make tbl
    if (length(all_years) == 1)
    {
      tbl_coup_list <- tidygraph::tbl_graph(
        nodes = nodes_of_the_year,
        edges = edges_of_the_year,
        directed = FALSE,
        node_key = source_id
      )
    } else {
      tbl_coup_list[[paste0(year, "-", year + time_window - 1)]] <-
        tidygraph::tbl_graph(
          nodes = nodes_of_the_year,
          edges = edges_of_the_year,
          directed = FALSE,
          node_key = source_id
        )
    }
  }
  
  if (filter_components == TRUE) {
    
    if (verbose == TRUE) {
      
      if (!is.null(min_share)) {
        cli::cli_alert_info(
          "We keep the components with a share of at least {min_share}."
        )
      }
      
      if (!is.null(nb_components)) {
        cli::cli_alert_info(
          "We keep the top {nb_components} largest component(s)."
        )
      }
    }
      
    tbl_coup_list <- filter_components_dynamic(tbl_coup_list, 
                                               ...)
    
  }
  return (tbl_coup_list)
}


###### custom filter components 

filter_components_dynamic <- function(graphs, 
                                      nb_components = NULL, 
                                      min_share = NULL, 
                                      keep_component_columns = FALSE, 
                                      verbose = FALSE) {
  
  # Helper function to apply filtering to a single graph
  filter_single_graph <- function(graph) {
    
    # checking 
    
      if (!is.null(min_share) & !is.null(nb_components) | is.null(min_share) & is.null(nb_components)) {
        cli::cli_abort(
          "You must specify either {.field nb_components} or {.field min_share}."
        )
      }
    
    
    graph <- graph %N>%
      dplyr::mutate(
        components_att = tidygraph::group_components(type = "weak")
      ) %>%
      dplyr::group_by(components_att) %>%
      dplyr::mutate(size_components = dplyr::n()) %>%
      dplyr::ungroup()
    
    # Summarize component sizes
    component_sizes <- graph %N>%
      as.data.frame() %>%
      dplyr::count(components_att, name = "size") %>%
      dplyr::mutate(share = size / sum(size)) %>%
      dplyr::arrange(desc(size)) %>%
      dplyr::mutate(cum_share = cumsum(share),
                    rank = dplyr::row_number())
    
    # Decide how many components to keep
    if (!is.null(min_share)) {
      selected_components <- component_sizes %>%
        dplyr::filter(share >= min_share) %>%
        dplyr::pull(components_att)
      
      if (verbose) {
        cli::cli_alert_info(
          "Keeping components with share of at least {min_share}."
        )
      }
      
    } else if (!is.null(nb_components)) {
      selected_components <- component_sizes %>%
        dplyr::slice_head(n = nb_components) %>%
        dplyr::pull(components_att)
      
      if (verbose) {
        cli::cli_alert_info(
          "Keeping top {nb_components} largest component(s)."
        )
      }
    } else {
      cli::cli_abort("You must specify either {.field nb_components} or {.field min_share}.")
    }
    
    # Filter graph
    graph <- graph %N>%
      dplyr::filter(components_att %in% selected_components)
    
    # Clean up columns if needed
    if (!keep_component_columns) {
      graph <- graph %>% dplyr::select(-components_att, -size_components)
    }
    
    return(graph)
  }
  
  # Apply to list or single graph
  if (inherits(graphs, "list")) {
    graphs <- lapply(graphs, filter_single_graph)
  } else if (inherits(graphs, "tbl_graph")) {
    graphs <- filter_single_graph(graphs)
  } else {
    cli::cli_abort("Your {.field graphs} object must be a {.cls tbl_graph} or a list of {.cls tbl_graph}.")
  }
  
  return(graphs)
}
