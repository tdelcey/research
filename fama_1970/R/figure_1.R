
# data 
source(here::here("fama_1970", "paths_and_packages.R"))

citing_fama <- readRDS(here(wos_data_path, "CITING_FAMA.rds"))
ds_articles <- arrow::open_dataset(here(WOS_data_path, "all_art.parquet"))

# filter for specific references 
data_summary <- citing_fama %>%
  filter(ItemID_Ref %in% c(366345, 366346, 60414706)) %>%
  mutate(
    review = case_when(
      ItemID_Ref == 366345 ~ "Fama (1970)",
      ItemID_Ref == 366346 ~ "Fama (1991)",
      ItemID_Ref == 60414706 ~ "Fama (1998)"
    )
  ) %>%
  select(ID_Art, review) %>%
  left_join(ds_articles %>%
              select(ID_Art, Annee_Bibliographique) %>%
              collect())

data_plot <- data_summary %>%
  filter(Annee_Bibliographique < 2011) %>%
  count(review, Annee_Bibliographique) %>%
  mutate(total = sum(n))


# Plotting the data

gg <- data_plot %>%
  ggplot() +
  geom_point(aes(
    x = Annee_Bibliographique,
    y = n,
    color = review,
    shape = review
  ),
  size = 2) +
  scale_color_manual(
    name = "",
    values = c(
      "Fama (1970)" = "#2171b5",
      "Fama (1991)" = "#a63603",
      "Fama (1998)" = "#fee391"
    )
  ) +
  labs(x = "", y = "", color = "") +
  labs(color  = "", shape = "") +
  theme_light(base_size = 16)

# Save the plot 
ggsave(
  here("fama_1970", "image", "reviews_citations_all.jpg"),
  gg,
  width = 8,
  height = 4.5,
  dpi = 300
)


