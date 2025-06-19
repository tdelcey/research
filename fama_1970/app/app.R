#' To manage dependencies, run
#' `renv::init()` # first time
#' `renv::snapshot()` # to update the renv.lock

# Dependencies
library(shiny)
library(tidyverse)
library(tidytext)
library(DT)
library(shinyWidgets)
library(ggiraph)
library(here)
library(ggwordcloud)
library(shinycssloaders)

# ---- Load Data ----

# Load the data

list_ggraph_all <- readRDS(here("data/graph_all_period.rds"))
list_ggraph_10 <- readRDS(here("data/list_ggraph_15.rds"))

corpus_all <- readRDS(here("data/corpus_all_periods.rds")) %>% rename(n_citations = n)
corpus_10 <- readRDS(here("data/corpus_15.rds")) %>% rename(n_citations = n)

# Load the data

#list_ggraph_all <- readRDS(here("fama_1970/app/data/graph_all_period.rds"))
#list_ggraph_10 <- readRDS(here("fama_1970/app/data/list_ggraph_15.rds"))

#corpus_all <- readRDS(here("fama_1970/app/data/corpus_all_periods.rds")) %>% rename(n_citations = n)
#corpus_10 <- readRDS(here("fama_1970/app/data/corpus_15.rds")) %>% rename(n_citations = n)




# --- ui ----


introduction_text <- paste(
  "This dashboard visualizes the static and dynamic networks of research papers citing Fama (1970).",
  "See the paper for the full description of the methodology. The code is available on",
  "<a href='https://github.com/tdelcey/research/tree/main/fama_1970'>GitHub</a>",
  "<br><br>",
  "The networks are interactive: click on a nodes or a cluster in the graph to see its metadata.",
  "<br><br>"
)





# Reusable tab module
networkTab <- function(id_suffix, list_ggraph, corpus) {
  ns <- function(id)
    paste0(id, "_", id_suffix)
  
  is_single_graph <- inherits(list_ggraph, "girafe")
  
  tagList(
    if (!is_single_graph) {
      fluidRow(column(
        6,
        offset = 3,
        sliderTextInput(
          ns("year"),
          "Select Year:",
          choices = names(list_ggraph),
          selected = names(list_ggraph)[1],
          width = "100%"
        )
      ))
    },
    fluidRow(column(12, withSpinner(
      girafeOutput(ns("network"), width = "100%", height = "600px")
    ))),
    # show tf idf of titles and top journal
    tags$hr(),
    fluidRow(
      column(
        4,
        h4("Words cloud (TF-IDF) for titles"),
        div(
          style = "color: grey; margin-bottom: 10px;",
          "Cliquez sur un cluster du graphique pour voir ses métadonnées."
        ),
        withSpinner(plotOutput(ns(
          "wordcloud_tfidf"
        ), height = "200px"))
      ),
      column(
        4,
        h4("Top journal (frequency and TF-IDF)"),
        div(
          style = "color: grey; margin-bottom: 10px;",
          "Cliquez sur un cluster du graphique pour voir ses métadonnées."
        ),
        withSpinner(DTOutput(ns("top_journal"), height = "200px"))
      ),
      column(
        4,
        h4("Time distribution of the selected cluster"),
        div(
          style = "color: grey; margin-bottom: 10px;",
          "Cliquez sur un cluster du graphique pour voir ses métadonnées."
        ),
        withSpinner(plotOutput(ns(
          "nodes_distribution"
        ), height = "100px")),
        tags$hr(),
        withSpinner(plotOutput(ns(
          "cluster_distribution"
        ), height = "100px"))
      )
    ),
    # plot time distribution of nodes in selected cluster
    
    # if single graph, delete temporal distribution
    # if (!is_single_graph) {
    #   column(12,
    #          h5("Distribution temporelle du cluster sélectionné"),
    #          withSpinner(plotOutput(ns("cluster_distribution"), height = "200px")),
    #          tags$hr(),
    #          h5("Nuage de mots (TF-IDF) pour les titres"),
    #          withSpinner(plotOutput(ns("wordcloud_tfidf"), height = "200px"))
    #
    #   )
    # },
    # show tables of the selected cluster
    tags$hr(),
    fluidRow(column(
      12,
      h4("Informations sur le cluster sélectionné"),
      div(
        style = "color: grey; margin-bottom: 10px;",
        "Cliquez sur un cluster du graphique pour voir ses métadonnées."
      ),
      DTOutput(ns("cluster_info"))
    ))
  )
}


# UI
ui <- fluidPage(
  titlePanel("Fama (1970) Dashboard"),
  
  HTML(introduction_text),
  
  tabsetPanel(
    tabPanel("All periods", networkTab("all", list_ggraph_all, corpus_all)),
    tabPanel("10-year window", networkTab("10", list_ggraph_10, corpus_10)),
  )
)


# ---- Server ----


# Server
server <- function(input, output, session) {
  setupTab <- function(id_suffix, list_ggraph, corpus) {
    ns <- function(id)
      paste0(id, "_", id_suffix)
    
    # initialize selected clusters and the selected year
    selected_clusters <- reactiveValues(data = NULL)
    
    is_single_graph <- inherits(list_ggraph, "girafe")
    
    selected_year <- if (is_single_graph) {
      NULL
    } else {
      reactive(input[[ns("year")]])
    }
    
    # Render the network graph, if not single graph, filter by selected year
    output[[ns("network")]] <- renderGirafe({
      gg <- if (is_single_graph) {
        list_ggraph
      } else {
        list_ggraph[[selected_year()]]
      }
      girafe_options(gg, opts_selection(type = "single"))
    })
    
    # if the user clicks on a node, filter the selected data from the corpus
    observeEvent(input[[paste0(ns("network"), "_selected")]], {
      if (!is.null(input[[paste0(ns("network"), "_selected")]])) {
        cluster_selected <- input[[paste0(ns("network"), "_selected")]]
        
        selected_clusters$data <- corpus %>%
          filter(
            (!!if (!is_single_graph)
              corpus$time_window == selected_year()
             else
               TRUE),
            dynamic_cluster_leiden == cluster_selected
          ) %>%
          select(c(
            first_author,
            year,
            title,
            journal,
            n_citations,
            dynamic_cluster_leiden
          )) %>%
          unique() %>%
          arrange(desc(n_citations))
      }
    })
    
    # Render the cluster information in a table
    output[[ns("cluster_info")]] <- renderDT({
      req(selected_clusters$data)
      datatable(
        selected_clusters$data %>% select(-dynamic_cluster_leiden),
        options = list(pageLength = 15)
      )
    })
    
    # From this selected corpus, compute information about the cluster
    
    # in how many time windows the cluster appears
    output[[ns("cluster_distribution")]] <- renderPlot({
      req(selected_clusters$data)
      cluster_id <- unique(selected_clusters$data$dynamic_cluster_leiden)
      
      if (is_single_graph) {
        return(NULL)
      } else {
      corpus %>%
        filter(dynamic_cluster_leiden %in% cluster_id) %>%
        count(dynamic_cluster_leiden, time_window) %>%
        ggplot(aes(x = time_window, y = n)) +
        geom_col(fill = "steelblue") +
        theme_minimal() +
        labs(x = "Time Window", y = "Documents") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    })
    
    # cluster_distribution, in how many time windows the cluster appears
    
    output[[ns("nodes_distribution")]] <- renderPlot({
      req(selected_clusters$data)
      cluster_id <- unique(selected_clusters$data$dynamic_cluster_leiden)
      
      corpus %>%
        filter(dynamic_cluster_leiden %in% cluster_id) %>%
        count(year, dynamic_cluster_leiden) %>%
        ggplot(aes(x = as.integer(year), y = n)) +
        geom_col(fill = "steelblue") +
        theme_minimal() +
        labs(x = "Year", y = "Documents") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    
    # Wordcloud for the titles of the selected cluster
    output[[ns("wordcloud_tfidf")]] <- renderPlot({
      req(selected_clusters$data)
      cluster_id <- unique(selected_clusters$data$dynamic_cluster_leiden)
      
      titles_df <- corpus %>%
        filter(!!if (!is_single_graph)
          corpus$time_window == selected_year()
          else
            TRUE) %>%
        select(source_id, title, dynamic_cluster_leiden)
      
      tidy_titles <- titles_df %>%
        unnest_tokens(word, title) %>%
        anti_join(stop_words, by = "word")
      
      tfidf <- tidy_titles %>%
        count(dynamic_cluster_leiden, word, sort = TRUE) %>%
        bind_tf_idf(word, dynamic_cluster_leiden, n)
      
      tfidf %>%
        filter(dynamic_cluster_leiden %in% cluster_id) %>%
        slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
        ggplot(aes(
          label = word,
          size = tf_idf,
          color = tf_idf
        )) +
        geom_text_wordcloud() +
        scale_size_area(max_size = 10) +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    
    # estimate tf-idf of journal
    
    output[[ns("top_journal")]] <- renderDT({
      req(selected_clusters$data)
      cluster_id <- unique(selected_clusters$data$dynamic_cluster_leiden)
      
      data <- corpus %>%
        filter(!!if (!is_single_graph)
          corpus$time_window == selected_year()
          else
            TRUE) %>%
        group_by(dynamic_cluster_leiden) %>%
        count(journal, dynamic_cluster_leiden) %>%
        group_by(dynamic_cluster_leiden) %>%
        bind_tf_idf(journal, dynamic_cluster_leiden, n) %>%
        arrange(desc(n)) %>%
        filter(dynamic_cluster_leiden %in% cluster_id) %>%
        ungroup %>%
        select(journal, n, tf_idf)
      
      datatable(data,
                options = list(
                  pageLength = 3,
                  # remove show entries
                  lengthChange = FALSE,
                  # remove search bar
                  searching = FALSE
                ))
      
    })

  }
  
  # Run function for each tab
  setupTab("all", list_ggraph_all, corpus_all)
  setupTab("10", list_ggraph_10, corpus_10)
}

# Run app
shinyApp(ui = ui, server = server)
