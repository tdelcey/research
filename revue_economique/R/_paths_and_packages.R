pacman::p_load(
  # data management
  "tidyverse",
  "dplyr",
  "janitor",
  "here",
  "readxl",
  "ggplot2",
  "ggraph",
  "ggiraph",
  "gganimate",
  "plotly",
  "ggsci",
  "ggrepel",
  "ggnewscale",
  "ggthemes",
  "scico",
  # text
  "tm",
  "SnowballC",
  "tidytext",
  "textcat",
  "text2vec",
  "stringr",
  "stringi",
  "tokenizers",
  "stringdist",
  "spacyr",
  "stm",
  "topicmodels",
  "ldatuning",
  # scrapping
  "rvest",
  "xml2",
  "XML",
  # network
  "biblionetwork",
  "networkflow",
  "tidygraph",
  "igraph",
  "vite",
  # computational efficiency
  "rstudioapi",
  "furrr",
  "data.table"
)

# paths

project_path <- "C:/cloud/research/revue_economique"
data_path <- "C:/cloud/data/revue_economique_project"


private_data_path <- here(data_path, "intermediate_data")
clean_corpus_path <- here(project_path, "data")
figures_path <- here(project_path, "figures")
app_path <- here(project_path, "app")


# Fixer les options globales
options(
  scipen = 999, # éviter la notation scientifique
  digits = 3, # limiter les décimales
  encoding = "UTF-8"
)

# Fixer les options knitr
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.path = "figures/",
  dpi = 300,
  fig.width = 8,
  fig.height = 5,
  dev = "png"
)
