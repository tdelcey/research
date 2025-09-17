# Thème ggplot personnalisé for black and white articles

theme_article_custom <- function(
  base_size = 18,
  legend_position = "bottom",
  base_family = "TeX Gyre Termes"
) {
  requireNamespace("ggplot2")

  theme_core <- ggplot2::theme_light(
    base_size = base_size,
    base_family = base_family
  ) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.key = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text = ggplot2::element_text(face = "bold", color = "black"),
      panel.grid.major = ggplot2::element_line(color = "gray85"),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(list(
    theme_core,
    ggplot2::scale_fill_grey(start = 0.9, end = 0.2),
    ggplot2::scale_color_grey(start = 0.2, end = 0.8)
  ))
}

#' Identify potential duplicates in corpus of documents with at least a title and an author
#'
#' This function detects potential duplicates in thesis metadata by comparing titles within groups
#' of the same author. It calculates string distances between pairs of titles using the Optimal
#' String Alignment (OSA) algorithm and filters results based on predefined thresholds.
#'
#' @param data_dt A `data.table` containing the thesis metadata, with at least three columns:
#'   - `authors`: Normalized author names used for grouping.
#'   - `title`: Normalized thesis titles.
#'   - `id`: Unique identifiers for each thesis.
#' @param threshold_distance Numeric. The maximum absolute string distance between two titles for them
#'   to be considered duplicates.
#' @param threshold_normalization Numeric. The maximum normalized string distance (distance divided by
#'   the product of the title lengths) for two titles to be considered duplicates.
#'
#' @return A `data.table` containing the following columns:
#'   - `id`: The identifier for the primary thesis in the duplicate group.
#'   - `id`: The identifier for the duplicate thesis.
#'   - `authors`: The author associated with the duplicates.
#'   - `text1`: The first title in the comparison.
#'   - `text2`: The second title in the comparison.
#'   - `distance`: The absolute string distance between the titles.
#'   - `normalized_distance`: The normalized string distance between the titles.
#'   If no duplicates are found, the function returns `NULL`.
#'
#' @details
#' The function first groups titles by `authors`, then compares all pairs of titles within each group.
#' String distances are calculated using the OSA algorithm, which accounts for single-character
#' substitutions, deletions, and transpositions. The results are filtered based on the provided
#' thresholds to minimize false positives.
#'
#' @examples
#' # Sample data
#' data_dt <- data.table(
#'   authors = c("smith john", "smith john", "doe jane"),
#'   title = c("My document Title", "My document titlé", "Another document"),
#'   id = c("ID1", "ID2", "ID3")
#' )
#'
#' # Detect duplicates with specific thresholds
#' find_duplicates(data_dt, threshold_distance = 2, threshold_normalization = 0.05)
#'
#' @export

find_duplicates <- function(
  data_dt,
  threshold_distance,
  threshold_normalization,
  workers
) {
  # as data.table

  data_dt <- as.data.table(data_dt)

  # Group data by authors to avoid unnecessary comparisons
  data_dt <- data_dt[, .(titles = list(title), ids = list(id)), by = authors]
  data_dt <- data_dt[lengths(titles) > 1] # Keep only groups with more than one title for safety (should not be necessary if data is clean)

  # Define a helper function for processing a single group
  process_group <- function(titles, ids, author) {
    # Compare all title pairs within the group
    comparison <- CJ(titles, titles, sorted = FALSE, unique = TRUE)
    setnames(comparison, c("text1", "text2"))
    # comparison <- comparison[text1 <= text2]  # Avoid redundant comparisons

    # Calculate string distance and normalized distance
    comparison[,
      distance := stringdist::stringdist(text1, text2, method = "osa")
    ]
    comparison[,
      normalized_distance := distance / (str_count(text1) * str_count(text2))
    ]

    if (nrow(comparison) > 0) {
      comparison[, authors := author]

      title_match <- comparison %>%
        as.data.table() %>%
        merge(
          data.table(title1 = titles, id_1 = ids),
          by.x = "text1",
          by.y = "title1",
          allow.cartesian = TRUE
        ) %>%
        merge(
          data.table(title2 = titles, id_2 = ids),
          by.x = "text2",
          by.y = "title2",
          allow.cartesian = TRUE
        ) %>%
        .[
          id_1 != id_2,
          .(id_1, id_2, authors, text1, text2, distance, normalized_distance)
        ]

      return(title_match)
    }
    return(NULL)
  }

  # Set up parallel processing
  plan(multisession, workers = workers)

  # Use future_map to parallelize the processing of each group
  results <- future_map(
    1:nrow(data_dt),
    ~ process_group(
      titles = data_dt$titles[[.x]],
      ids = data_dt$ids[[.x]],
      author = data_dt$authors[.x]
    ),
    .progress = TRUE
  )

  if (length(results) > 0) {
    results <- results %>%
      rbindlist()
    setkey(results, key = id_1)
    duplicates <- results[
      normalized_distance < threshold_normalization &
        distance < threshold_distance,
    ]
    setnames(duplicates, "id_1", "id")
    duplicates <- unique(duplicates)

    return(duplicates)
  } else {
    return(NULL)
  }
}


# personal tidy extractor of estimate effect object.
# Insteading of using stm::plot.estimateEffect() to plot the effect of years, we extract simulated betas in a tidy format to plot the effect of years.

extract_ee <- function(
  x,
  covariate,
  model = NULL,
  topics = x$topics,
  method = "pointestimate",
  cov.value1 = NULL,
  cov.value2 = NULL,
  moderator = NULL,
  moderator.value = NULL,
  npoints = 500,
  nsims = 500,
  ci.level = .95,
  custom.labels = NULL,
  labeltype = "numbers",
  n = 7,
  frexw = .5
) {
  cthis <- stm:::produce_cmatrix(
    prep = x,
    covariate = covariate,
    method = method,
    cov.value1 = cov.value1,
    cov.value2 = cov.value2,
    moderator = moderator,
    npoints = npoints,
    moderator.value = moderator.value
  )

  simbetas <- stm:::simBetas(parameters = x$parameters, nsims = nsims)

  uvals <- cthis$cdata[[covariate]]
  offset <- (1 - ci.level) / 2
  labels <- stm:::createLabels(
    labeltype = labeltype,
    covariate = covariate,
    method = method,
    cdata = cthis$cdata,
    cov.value1 = cov.value1,
    cov.value2 = cov.value2,
    model = model,
    n = n,
    topics = x$topics,
    custom.labels = custom.labels,
    frexw = frexw
  )

  out <- lapply(topics, function(i) {
    sims <- cthis$cmatrix %*% t(simbetas[[which(x$topics == i)]])

    if (method == "difference") {
      diff <- sims[1, ] - sims[2, ]
      out_inner <- data.frame(
        method = method,
        topic = i,
        covariate = covariate,
        covariate.value = paste0(cov.value1, "-", cov.value2),
        estimate = mean(diff),
        std.error = sd(diff),
        ci.level = ci.level,
        ci.lower = quantile(diff, offset),
        ci.upper = quantile(diff, 1 - offset),
        label = labels[which(x$topics == i)]
      )
    } else {
      out_inner <- data.frame(
        method = method,
        topic = i,
        covariate = covariate,
        covariate.value = uvals,
        estimate = apply(sims, 1, mean),
        std.error = apply(sims, 1, sd),
        ci.level = ci.level,
        ci.lower = apply(sims, 1, quantile, probs = offset),
        ci.upper = apply(sims, 1, quantile, probs = (1 - offset)),
        label = labels[which(x$topics == i)]
      )
    }

    if (!is.null(moderator)) {
      out_inner$moderator <- moderator
      out_inner$moderator.value <- moderator.value
    }

    rownames(out_inner) <- NULL
    return(out_inner)
  })
  out <- do.call("rbind", out)

  return(out)
}
