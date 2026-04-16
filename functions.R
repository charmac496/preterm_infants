library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(glmnet)
library(cluster)   
library(dtw)             
library(ordinalNet)     
library(MASS)          
library(mediation)
library(gtsummary) 
library(flextable)
library(forcats)

#' Rename columns in a dataframe using a mapping vector
#'
#' This function renames columns in a dataframe by matching old names to new names.
#' Only renames columns that actually exist in the dataframe.
#'
#' @param df A dataframe
#' @param old_names Character vector of original column names to rename
#' @param new_names Character vector of new column names (must be same length as old_names)
#' @return A dataframe with renamed columns
#' @importFrom dplyr rename sym
#' @export
renames <- function(df, old_names, new_names) {
  # Validate that input vectors have equal length
  if (length(old_names) != length(new_names)) {
    stop("Original vector length: ", length(old_names), 
         ", replacement vector length: ", length(new_names), 
         ", not equal", sep = "")
  }
  
  # Iterate through each pair of old and new names
  for (i in seq_along(old_names)) {
    # Only rename if the old column name exists in the dataframe
    if (old_names[i] %in% colnames(df)) {
      df <- df %>% dplyr::rename(!!new_names[i] := !!sym(old_names[i]))
    }
  }
  return(df)
}


#' Replace character patterns in all columns of a dataframe
#'
#' This function applies multiple pattern replacements to all columns in a dataframe
#' using gsub. Useful for cleaning character data across the entire dataset.
#'
#' @param df A dataframe
#' @param patterns Character vector of patterns to search for
#' @param replacements Character vector of replacement strings (same length as patterns)
#' @return A dataframe with replaced values in all columns
#' @export
renamec <- function(df, patterns, replacements) {
  # Validate input vectors have equal length
  if (length(patterns) != length(replacements)) {
    stop("Original vector length: ", length(patterns), 
         ", replacement vector length: ", length(replacements), 
         ", not equal", sep = "")
  }
  
  # Apply each pattern replacement sequentially to all columns
  for (i in seq_along(patterns)) {
    df[] <- lapply(df, function(x) gsub(patterns[i], replacements[i], x))
  }
  return(df)
}

#' Expand a delimited column into multiple logical columns (by substring presence)
#'
#' This function processes a column that contains delimited values (e.g., "a,b,c").
#' It splits each cell by a separator, identifies all unique tokens (excluding those
#' specified in `esc`), and for each token creates a logical column indicating
#' whether the original cell contains that token as a substring.
#'
#' If a column with the generated name already exists, the new column is combined
#' with the existing one via logical OR (i.e., the column becomes TRUE if either is TRUE).
#'
#' @param dat A data frame.
#' @param col Name of the column to expand (character).
#' @param sep Separator string used to split the values (passed to `str_split`).
#' @param esc Character vector of tokens to exclude. No column will be created
#'        for these tokens.
#' @param ca Logical; if `TRUE`, new column names are `{col}_{token}`; if `FALSE`,
#'        the token itself is used as the column name. Default `TRUE`.
#' @return A list with two components:
#'   \item{df}{The modified data frame with new logical columns added.}
#'   \item{nn}{Character vector of the names of newly created columns.}
#'
#' @details
#' For each unique token `t` found in the split values (and not in `esc`):
#' \itemize{
#'   \item A temporary logical vector is created: `TRUE` if the original
#'         `dat[[col]]` contains `t` as a substring (using fixed matching);
#'         `FALSE` otherwise. Any `NA` in the original column becomes `FALSE`.
#'   \item The new column name is determined: if `ca = TRUE` then `paste(col, t, sep = "_")`,
#'         otherwise `t`.
#'   \item If a column with that name already exists in `dat`, the new column
#'         is combined with it using the OR operator (`|`), meaning the existing
#'         column is updated to `TRUE` if either the existing value or the new
#'         indicator is `TRUE`.
#'   \item Otherwise, the new column is added to `dat` and its name is recorded.
#' }
#'
#' @examples
#' df <- data.frame(x = c("A,B,C", "A,B", "B,C", "C,D", NA))
#' result <- enlist_logical(df, "x", ",", esc = "D", ca = TRUE)
#' result$df
#' #   x x_A x_B x_C
#' # 1 A,B,C TRUE TRUE TRUE
#' # 2   A,B TRUE TRUE FALSE
#' # 3   B,C FALSE TRUE TRUE
#' # 4   C,D FALSE FALSE TRUE
#' # 5  <NA> FALSE FALSE FALSE
#'
#' @importFrom stringr str_split str_detect fixed
#' @importFrom purrr discard
#' @export
enlist_logical <- function(dat, col, sep, esc = list(), ca = TRUE) {
  new_names <- character(0)
  
  # Extract all unique non-excluded values from the column
  # 1. Split each cell by separator -> list of character vectors
  # 2. Unlist to a single vector
  # 3. Remove NA values
  # 4. Remove values in esc
  # 5. Keep unique
  enlisted <- dat[[col]] %>% 
    str_split(sep) %>% 
    unlist() %>% 
    discard(is.na) %>% 
    discard(\(x) x %in% esc) %>% 
    unique()
  
  # Create a logical column for each unique value
  for (name in enlisted) {
    new_name <- if (ca) paste(col, name, sep = "_") else name
    
    # Detect if the original column contains 'name' as a substring
    # str_detect returns NA for NA inputs; convert NA to FALSE
    detected <- str_detect(dat[[col]], fixed(name))
    if (anyNA(detected)) detected[is.na(detected)] <- FALSE
    temp_col <- detected
    
    if (new_name %in% names(dat)) {
      # Column already exists -> combine using OR
      dat[[new_name]] <- dat[[new_name]] | temp_col
    } else {
      dat[[new_name]] <- temp_col
      new_names <- c(new_names, new_name)
    }
  }
  
  return(list(df = dat, nn = new_names))
}

#' Convert character strings to numeric values with flexible unit handling
#'
#' This function converts character vectors to numeric values by:
#' - Optionally converting common missing value markers (e.g., "NA", "N/A") to NA.
#' - Optionally removing specified patterns (`units_to_remove`) completely.
#' - Optionally detecting unit patterns (`units`) and multiplying by conversion factors.
#' - Handling value ranges (e.g., "10-20") by returning the midpoint, while
#'   ignoring scientific notation (e.g., "1.2e-3", "2.5e+2") which contain
#'   a hyphen or plus as part of the exponent.
#'
#' @param x Character vector to convert.
#' @param na_strings Character vector of strings that should be treated as missing
#'        values (converted to NA). Default: `c("NA", "N/A")`. Set to `NULL` to disable.
#' @param units_to_remove Character vector of patterns to strip from the string
#'        before numeric conversion. These patterns are removed entirely.
#'        Special regular expression characters are automatically escaped,
#'        so you can pass simple strings like `"+"` without backslashes.
#'        Default: `NULL` (no patterns removed).
#' @param units Named numeric vector where names are fixed patterns to detect
#'        (case‑sensitive, fixed string matching), and values are conversion
#'        factors to multiply the numeric value. The factors should be chosen
#'        so that all input values are converted to a **common target unit**.
#'        For example, if you want all time values to become **hours**, use:
#'        `c("秒" = 1/3600, "分" = 1/60, "分钟" = 1/60, "min" = 1/60, "小时" = 1)`.
#'        If multiple patterns match, the conversion factor of the **first** match
#'        (according to the order of `units`) is used. The matched pattern is also
#'        removed from the string.
#'        Default: `NULL` (no unit conversion).
#' @param range_sep Character string specifying the separator for ranges.
#'        Default: `"-"`.
#' @param multiply_units Logical; if `TRUE`, apply the conversion factors.
#'        Default: `TRUE`. Ignored if `units` is `NULL` or empty.
#' @param warn Logical; if `TRUE`, issue a warning when some values could not
#'        be converted to numeric (i.e., become `NA`). Default: `TRUE`.
#' @return Numeric vector with converted values. Unconvertible values become `NA`.
#' @export
num_cha <- function(x,
                    na_strings = c("NA", "N/A"),
                    units_to_remove = NULL,
                    units = NULL,
                    range_sep = "-",
                    multiply_units = TRUE,
                    warn = TRUE) {
  
  # Step 0: Convert user‑specified missing value markers to NA
  if (!is.null(na_strings) && length(na_strings) > 0) {
    # Case‑insensitive matching for NA strings
    x[toupper(x) %in% toupper(na_strings)] <- NA
  }
  
  # Step 1: Remove specified patterns (if any)
  vec_clean <- x
  if (!is.null(units_to_remove) && length(units_to_remove) > 0) {
    pattern_remove <- paste(sapply(units_to_remove, str_escape), collapse = "|")
    vec_clean <- gsub(pattern_remove, "", vec_clean)
  }
  
  # Step 2: Initialize conversion factor
  factor <- rep(1, length(vec_clean))
  
  # Step 3: Detect and remove unit patterns (if any)
  if (multiply_units && !is.null(units) && length(units) > 0) {
    for (i in seq_along(units)) {
      pat <- names(units)[i]
      conv <- units[i]
      idx <- grepl(pat, vec_clean, fixed = TRUE)        # fixed = TRUE → case‑sensitive
      factor[idx] <- conv
      vec_clean <- gsub(pat, "", vec_clean, fixed = TRUE)
    }
  }
  
  # Step 4: Detect scientific notation and ranges
  is_scientific <- grepl("[Ee][+-]", vec_clean)
  is_range <- grepl(range_sep, vec_clean) & !is_scientific
  
  # Step 5: Convert to numeric
  vec_num <- rep(NA_real_, length(vec_clean))
  
  # Non-range values (including scientific notation)
  non_range_idx <- !is_range
  if (any(non_range_idx)) {
    vec_num[non_range_idx] <- suppressWarnings(as.numeric(vec_clean[non_range_idx]))
  }
  
  # Range values (midpoint)
  if (any(is_range)) {
    parts <- strsplit(vec_clean[is_range], range_sep, fixed = TRUE)
    left <- suppressWarnings(as.numeric(sapply(parts, `[`, 1)))
    right <- suppressWarnings(as.numeric(sapply(parts, `[`, 2)))
    range_vals <- (left + right) / 2
    vec_num[is_range] <- range_vals
  }
  
  # Step 6: Apply conversion factors
  if (multiply_units && !is.null(units) && length(units) > 0) {
    vec_num <- vec_num * factor
  }
  
  # Step 7: Warn about unconvertible values (excluding those already NA)
  if (warn) {
    orig_na <- is.na(x)  # includes the newly converted NA from na_strings
    new_na <- is.na(vec_num)
    problematic <- new_na & !orig_na
    if (any(problematic)) {
      problematic_vals <- unique(x[problematic])
      warning("Some values could not be converted to numeric:\n",
              paste("  ", problematic_vals, collapse = "\n"),
              call. = FALSE)
    }
  }
  
  return(vec_num)
}

#' Apply numeric conversion to multiple columns
#'
#' This function applies `num_cha()` to specified columns in a data frame,
#' passing any additional arguments to `num_cha()`.
#'
#' @param x A data frame.
#' @param y Character vector of column names to convert.
#' @param ... Additional arguments passed to `num_cha()` (e.g., `units`, `units_to_remove`).
#' @return Data frame with converted columns.
#' @export
num_m_col <- function(x, y, ...) {
  for (col in y) {
    x[[col]] <- num_cha(x[[col]], ...)
  }
  return(x)
}

#' Convert logical-like character values to logical type
#'
#' This function maps common Chinese/English indicators to logical values and
#' automatically converts columns that contain the string "TRUE" (after mapping)
#' to logical type.
#'
#' @param df A data frame.
#' @param map A named list specifying the mapping from character values to
#'        logical values (TRUE, FALSE, NA). The names are the values to replace,
#'        and the values are the replacements. Default:
#'        list("是" = TRUE, "有" = TRUE, "否" = FALSE, "无" = FALSE,
#'             "不详" = NA, "未做" = NA, "未检查" = NA)
#' @return Data frame with converted logical columns.
#' @export
log_m_col <- function(df, map = NULL) {
  # Default mapping
  if (is.null(map)) {
    map <- list(
      "是" = TRUE, "有" = TRUE,
      "否" = FALSE, "无" = FALSE,
      "不详" = NA, "未做" = NA, "未检查" = NA
    )
  }
  
  # Apply mapping
  for (pattern in names(map)) {
    replacement <- map[[pattern]]
    df[df == pattern] <- replacement
  }
  
  # Identify columns that contain the string "TRUE" (case-sensitive)
  cols <- sapply(df, function(col) any(col == "TRUE", na.rm = TRUE))
  # Convert those columns to logical type
  df[cols] <- lapply(df[cols], as.logical)
  
  return(df)
}

#' Create time-value sequence for DTW clustering
#'
#' Helper function that converts a row of wide-format time series data into
#' a matrix of time points and values, removing NA values.
#' Column names are assumed to be numeric time points (or coercible to numeric).
#'
#' @param row A data frame row (a single row) with columns representing time points.
#' @return A matrix with columns "time" and "value", containing only non-NA observations.
create_sequence <- function(row) {
  values <- unlist(row)
  times <- as.numeric(names(row))
  cbind(time = times[!is.na(values)], value = values[!is.na(values)])
}

#' Cluster time series data using Dynamic Time Warping
#'
#' This function performs hierarchical clustering on longitudinal data using DTW distance.
#' It handles missing values by filtering individuals with insufficient observations.
#'
#' @param data A dataframe with columns: subject ID, time point, and variable of interest.
#' @param var_name Character string specifying the variable to cluster.
#' @param optimal_k Optional integer specifying number of clusters; if NULL, shows elbow plots.
#' @param id_col Character string naming the subject ID column. **Required**.
#' @param time_col Character string naming the time point column. **Required**.
#' @param plot Logical; if TRUE and `optimal_k` is NULL, display diagnostic plots. Default: `TRUE`.
#' @return If `optimal_k` is provided, returns a wide-format dataframe with cluster assignments;
#'         otherwise, returns `NULL` (invisibly) after optionally plotting.
#' @importFrom dplyr select pivot_wider arrange
#' @importFrom dtw dtw symmetric2
#' @importFrom cluster silhouette pam
#' @export
cluster_by_dtw <- function(data, var_name, optimal_k = NULL,
                           id_col, time_col,plot = TRUE) {
  # Convert to wide format: each subject becomes a row, time points become columns
  # Column names are set to the values in `time_col` (assumed numeric or coercible)
  data_wide <- data %>%
    dplyr::select(!!sym(id_col), !!sym(time_col), all_of(var_name)) %>%
    pivot_wider(
      id_cols = !!sym(id_col),
      names_from = !!sym(time_col),
      values_from = all_of(var_name),
      names_sort = TRUE
    ) %>%
    arrange(!!sym(id_col))
  
  # Remove subjects with only one non-missing value (cannot compute distance)
  time_cols <- setdiff(names(data_wide), id_col)
  data_wide <- data_wide[rowSums(!sapply(data_wide[time_cols], is.na)) > 1, ]
  
  # Create sequences for DTW distance calculation
  sequences <- lapply(1:nrow(data_wide), function(i) {
    row_data <- data_wide[i, time_cols, drop = FALSE]
    create_sequence(row_data)
  })
  
  # Compute pairwise DTW distance matrix
  n <- nrow(data_wide)
  dtw_dist <- matrix(NA, n, n)
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      dtw_dist[i, j] <- dtw(sequences[[i]][, "value"], sequences[[j]][, "value"],
                            step.pattern = symmetric2, keep.internals = TRUE)$distance
    }
  }
  dtw_dist[lower.tri(dtw_dist)] <- t(dtw_dist)[lower.tri(dtw_dist)]
  dtw_dist_full <- as.dist(dtw_dist)
  
  # Perform hierarchical clustering
  hc <- hclust(dtw_dist_full, method = "ward.D2")
  
  # Determine optimal number of clusters if not specified
  if (is.null(optimal_k)) {
    if (plot) {
      plot(hc)
      wss <- sapply(1:10, function(k) sum(kmeans(dtw_dist_full, centers = k)$withinss))
      plot(1:10, wss, type = "b", xlab = "Number of clusters", ylab = "Within-cluster sum of squares")
      sil_width <- sapply(2:10, function(k) {
        pam_result <- pam(dtw_dist_full, k = k)
        mean(silhouette(pam_result)[, 3])
      })
      plot(2:10, sil_width, type = "b", xlab = "Number of clusters", ylab = "Average silhouette width")
    }
    return(invisible(NULL))
  } else {
    clusters <- cutree(hc, k = optimal_k)
    cluster_col_name <- paste0(var_name, "_cluster")
    data_wide[[cluster_col_name]] <- factor(clusters)
    return(data_wide)
  }
}

#' Plot time series trajectories for a cluster with median and IQR ribbon
#'
#' This function creates a ggplot of individual trajectories (semi-transparent lines)
#' for a single cluster, overlaid with the median line and an interquartile range ribbon.
#' It is designed to visualize clustering results for longitudinal data.
#' Ordered factor response variables are automatically converted to numeric for plotting,
#' while preserving the original factor levels for axis labels.
#'
#' @param data Data frame containing individual observations (long format).
#' @param stats Data frame containing cluster-level summary statistics (median, Q25, Q75)
#'        for each time point within a cluster.
#' @param cluster_id Value identifying the cluster to plot (must exist in both `data` and `stats`).
#' @param cluster_col Character string naming the cluster column in both `data` and `stats`.
#' @param id_col Character string naming the individual identifier column in `data`.
#' @param time_col Character string naming the time column (numeric) in both `data` and `stats`.
#' @param value_col Character string naming the observed value column in `data` (can be numeric or ordered factor).
#' @param median_col Character string naming the median column in `stats` (numeric or ordered factor).
#' @param q25_col Character string naming the first quartile column in `stats` (numeric or ordered factor).
#' @param q75_col Character string naming the third quartile column in `stats` (numeric or ordered factor).
#' @param cluster_labels Optional named character vector or function. If a named vector,
#'        names should be cluster values (as characters) and values the desired labels.
#'        If a function, it will be applied to `cluster_id` to generate the title.
#'        Default `NULL` uses the raw cluster value as title.
#' @param individual_alpha Transparency for individual lines (0-1). Default 0.4.
#' @param individual_linewidth Width of individual lines. Default 0.5.
#' @param individual_color Color for individual lines. Default "gray60".
#' @param median_color Color for the median line. Default "red".
#' @param median_linewidth Width of the median line. Default 1.5.
#' @param ribbon_fill Fill color for the IQR ribbon. Default "blue".
#' @param ribbon_alpha Transparency for the ribbon fill. Default 0.2.
#' @param xlab Label for x-axis. Default `time_col`.
#' @param ylab Label for y-axis. Default `value_col`.
#' @param x_breaks Optional numeric vector of breaks for x-axis.
#' @param x_minor_breaks Optional numeric vector of minor breaks for x-axis.
#' @param ... Additional arguments passed to `labs()` (e.g., `caption`, `subtitle`).
#'
#' @return A `ggplot` object.
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme element_text scale_x_continuous scale_y_continuous
#' @export
plot_cluster_trajectories <- function(data,
                                      stats,
                                      cluster_id,
                                      cluster_col,
                                      id_col,
                                      time_col,
                                      value_col,
                                      median_col,
                                      q25_col,
                                      q75_col,
                                      cluster_labels = NULL,
                                      individual_alpha = 0.4,
                                      individual_linewidth = 0.5,
                                      individual_color = "gray60",
                                      median_color = "red",
                                      median_linewidth = 1.5,
                                      ribbon_fill = "blue",
                                      ribbon_alpha = 0.2,
                                      xlab = NULL,
                                      ylab = NULL,
                                      x_breaks = NULL,
                                      x_minor_breaks = NULL,
                                      ...) {
  
  # Validate required columns in data and stats
  required_cols_data <- c(id_col, time_col, value_col, cluster_col)
  required_cols_stats <- c(cluster_col, time_col, median_col, q25_col, q75_col)
  for (col in required_cols_data) {
    if (!col %in% names(data)) stop("Column '", col, "' not found in `data`.")
  }
  for (col in required_cols_stats) {
    if (!col %in% names(stats)) stop("Column '", col, "' not found in `stats`.")
  }
  
  # Filter data for the requested cluster
  cluster_data <- data[data[[cluster_col]] == cluster_id, ]
  if (nrow(cluster_data) == 0) stop("No observations in `data` for cluster_id = ", cluster_id)
  cluster_stats <- stats[stats[[cluster_col]] == cluster_id, ]
  if (nrow(cluster_stats) == 0) stop("No observations in `stats` for cluster_id = ", cluster_id)
  
  # Convert ordered factor to numeric for proper plotting, store levels for axis labels
  y_levels <- NULL
  if (is.factor(cluster_data[[value_col]])) {
    y_levels <- levels(cluster_data[[value_col]])
    cluster_data[[value_col]] <- as.numeric(cluster_data[[value_col]])
    warning("Converted factor `", value_col, "` to numeric for plotting. Labels: ", paste(y_levels, collapse = ", "))
  }
  
  for (col in c(median_col, q25_col, q75_col)) {
    if (is.factor(cluster_stats[[col]])) {
      cluster_stats[[col]] <- as.numeric(cluster_stats[[col]])
    }
  }
  
  # Ensure time column is numeric
  if (!is.numeric(cluster_data[[time_col]])) {
    cluster_data[[time_col]] <- as.numeric(cluster_data[[time_col]])
  }
  if (!is.numeric(cluster_stats[[time_col]])) {
    cluster_stats[[time_col]] <- as.numeric(cluster_stats[[time_col]])
  }
  
  # Determine plot title
  if (is.null(cluster_labels)) {
    title <- as.character(cluster_id)
  } else if (is.function(cluster_labels)) {
    title <- cluster_labels(cluster_id)
  } else {
    id_char <- as.character(cluster_id)
    title <- ifelse(id_char %in% names(cluster_labels), cluster_labels[id_char], id_char)
  }
  
  # Default axis labels
  if (is.null(xlab)) xlab <- time_col
  if (is.null(ylab)) ylab <- if (!is.null(y_levels)) paste0(value_col, " (numeric level)") else value_col
  
  # Build the plot
  p <- ggplot() +
    geom_line(data = cluster_data,
              aes(x = .data[[time_col]], y = .data[[value_col]], group = .data[[id_col]]),
              alpha = individual_alpha, linewidth = individual_linewidth, color = individual_color) +
    geom_line(data = cluster_stats,
              aes(x = .data[[time_col]], y = .data[[median_col]]),
              color = median_color, linewidth = median_linewidth) +
    geom_ribbon(data = cluster_stats,
                aes(x = .data[[time_col]], ymin = .data[[q25_col]], ymax = .data[[q75_col]]),
                fill = ribbon_fill, alpha = ribbon_alpha) +
    labs(title = title, x = xlab, y = ylab, ...) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # If original response was a factor, relabel y-axis with original levels
  if (!is.null(y_levels)) {
    p <- p + scale_y_continuous(breaks = seq_along(y_levels), labels = y_levels)
  }
  
  # Customize x-axis breaks if provided
  if (!is.null(x_breaks) || !is.null(x_minor_breaks)) {
    p <- p + scale_x_continuous(breaks = x_breaks, minor_breaks = x_minor_breaks)
  }
  
  return(p)
}


#' Generate and combine cluster trajectory plots for all clusters
#'
#' A convenience wrapper around `plot_cluster_trajectories` that creates a plot
#' for each unique cluster in the data and optionally arranges them using `patchwork`.
#'
#' @param data Same as in `plot_cluster_trajectories`.
#' @param stats Same as in `plot_cluster_trajectories`.
#' @param cluster_col Same as in `plot_cluster_trajectories`.
#' @param ... All other arguments passed to `plot_cluster_trajectories` (e.g.,
#'        `id_col`, `time_col`, `value_col`, `median_col`, `q25_col`, `q75_col`,
#'        `cluster_labels`, styling parameters).
#' @param combine Logical; if `TRUE`, combine plots using `patchwork::wrap_plots()`.
#'        Default `TRUE`. Requires `patchwork` package.
#' @param ncol Number of columns for combined plots (ignored if `combine = FALSE`).
#' @param nrow Number of rows for combined plots (ignored if `combine = FALSE`).
#' @param title Optional overall title for the combined plot.
#'
#' @return If `combine = TRUE` and `patchwork` is available, a combined `ggplot`
#'         object; otherwise, a list of `ggplot` objects (one per cluster).
#' @export
plot_all_clusters <- function(data,
                              stats,
                              cluster_col,
                              ...,
                              combine = TRUE,
                              ncol = NULL,
                              nrow = NULL,
                              title = NULL) {
  
  clusters <- unique(c(data[[cluster_col]], stats[[cluster_col]]))
  clusters <- sort(clusters[!is.na(clusters)])
  
  plots <- lapply(clusters, function(cl) {
    plot_cluster_trajectories(data = data,
                              stats = stats,
                              cluster_id = cl,
                              cluster_col = cluster_col,
                              ...)
  })
  
  if (!combine) {
    return(plots)
  }
  
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    warning("Package 'patchwork' is needed for combining plots. Returning list of plots instead.")
    return(plots)
  }
  
  combined <- patchwork::wrap_plots(plots, ncol = ncol, nrow = nrow)
  if (!is.null(title)) {
    combined <- combined + patchwork::plot_annotation(title = title,
                                                      theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)))
  }
  return(combined)
}

#' Merge multiple dataframes dynamically based on group membership
#'
#' This function merges a main dataframe with different target dataframes based on
#' group membership. Useful when different groups require different supplementary data.
#'
#' @param df Main dataframe
#' @param data_list Named list where names correspond to group values and values are target dataframes
#' @param group_col Name of the grouping column in the main dataframe
#' @param merge_keys Character vector of column names to merge on
#' @return A dataframe with merged results for all groups
#' @export
dynamic_merge <- function(df, data_list, group_col, merge_keys) {
  # Validate group column exists
  if (!group_col %in% names(df)) {
    stop("Group column '", group_col, "' not found in main dataframe")
  }
  
  groups <- unique(df[[group_col]])
  result_list <- lapply(groups, function(g) {
    df_sub <- df[df[[group_col]] == g, ]
    target_df <- data_list[[g]]
    
    if (!is.null(target_df)) {
      # Validate merge keys exist in both dataframes
      if (!all(merge_keys %in% names(df_sub)) || !all(merge_keys %in% names(target_df))) {
        warning("Missing merge keys for group '", g, "', skipping merge")
        return(df_sub)
      }
      merged <- merge(df_sub, target_df, by = merge_keys, all.x = TRUE)
    } else {
      warning("No target dataframe found for group '", g, "'")
      merged <- df_sub
    }
    return(merged)
  })
  do.call(rbind, result_list)
}

#' Remove columns with constant values
#'
#' This function identifies and removes columns that have only one unique non-NA value.
#'
#' @param data A dataframe
#' @return A list containing:
#'   \item{df}{Dataframe with constant columns removed}
#'   \item{fc}{Named list of removed columns and their constant values}
#' @export
single_removal <- function(data) {
  s_forfeited <- list()
  
  # Identify columns with only one unique non-NA value
  for (each in names(data)) {
    if (length(na.omit(unique(data[[each]]))) <= 1) {
      s_forfeited[[each]] <- ifelse(
        length(na.omit(unique(data[[each]]))) == 1,
        na.omit(unique(data[[each]]))[1],
        NA
      )
    }
  }
  
  # Remove identified columns
  data <- data[, !names(data) %in% names(s_forfeited), drop = FALSE]
  return(list(df = data, fc = s_forfeited))
}


#' Remove columns with insufficient non-NA values
#'
#' This function removes columns that have fewer non-missing values than a threshold.
#'
#' @param data A dataframe
#' @param threshold Minimum number of non-NA values required to keep a column
#' @return A list containing:
#'   \item{df}{Dataframe with low-frequency columns removed}
#'   \item{fc}{Named list of removed columns and their sample sizes}
#' @export
l_s_removal <- function(data, threshold) {
  s_forfeited <- list()
  
  # Identify columns with insufficient non-NA values
  for (each in names(data)) {
    if (length(data[[each]][!is.na(data[[each]])]) < threshold) {
      s_forfeited[[each]] <- paste0(length(data[[each]][!is.na(data[[each]])]), "samples")
    }
  }
  
  # Remove identified columns
  data <- data[, !names(data) %in% names(s_forfeited), drop = FALSE]
  return(list(df = data, fc = s_forfeited))
}


#' Advanced variable filtering and recategorization
#'
#' This function performs comprehensive variable cleaning:
#' - Removes columns with insufficient non-NA values (threshold based on sample size).
#' - For numeric columns, removes those where a single value dominates beyond a threshold.
#' - For categorical variables, either removes columns with too many rare categories
#'   or recategorizes rare categories into an "others" group.
#' - For ordered factors, collapses rare categories from the ends (lowest and highest)
#'   until cumulative frequencies meet the threshold.
#'
#' @param data A data frame.
#' @param threshold Integer threshold used for rare category detection and filtering.
#'        The interpretation is context-dependent:
#'        \itemize{
#'          \item In the initial removal step, columns with fewer than
#'                \code{round(nrow(data) / threshold)} non-NA values are removed.
#'          \item For categorical variables, a category is considered "rare" if its
#'                count is less than \code{threshold}.
#'          \item For ordered factors, categories at the ends are collapsed if their
#'                cumulative counts are less than \code{threshold}.
#'          \item A column is removed if the number of categories with count >=
#'                \code{threshold} is <= 1.
#'        }
#' @return A list containing:
#'   \item{df}{Cleaned data frame.}
#'   \item{ff}{List of removed columns and their value distributions.}
#'   \item{fc}{List of recategorized columns and their original distributions.}
#' @details
#' The function proceeds as follows:
#' 1. Remove columns with insufficient non-NA values (using \code{l_s_removal}).
#' 2. For each remaining column:
#'    - If the number of unique non-NA values is less than the number of non-NA rows
#'      divided by \code{threshold}, further processing is performed.
#'    - For numeric columns: if the maximum category count >= total count - \code{threshold},
#'      the column is removed (dominated by a single value).
#'    - For categorical (factor) columns:
#'        * If only 0 or 1 categories have count >= \code{threshold}, the column is removed.
#'        * Otherwise, rare categories (count < \code{threshold}) are collapsed into "others".
#'    - For ordered factors: rare categories at the extremes (lowest and highest) are
#'      collapsed into the nearest non‑rare category. A warning is issued.
#' 3. Return the cleaned data frame along with information about removed and changed columns.
#' @export
l_s_s_removal <- function(data, threshold) {
  # First remove columns with insufficient non-NA values
  s_forfeited <- l_s_removal(data, round(nrow(data) / threshold))[["fc"]]
  s_changed <- list()
  
  for (each in names(data)) {
    # Check if the column has too few unique values relative to sample size
    if (length(na.omit(unique(data[[each]]))) < length(na.omit(data[[each]])) / threshold) {
      s_n <- table(na.omit(data[[each]]))
      
      if (is.numeric(data[[each]])) {
        # For numeric columns, check if a single value dominates
        if (max(s_n) >= sum(s_n) - threshold) {
          s_forfeited[[each]] <- s_n
        }
      } else {
        # For categorical columns
        if (sum(s_n >= threshold) <= 1) {
          # Too many rare categories - remove column
          s_forfeited[[each]] <- s_n
        } else {
          # Recategorize rare categories
          s_changed[[each]] <- s_n
          
          if (is.ordered(data[[each]])) {
            # For ordered factors, collapse from both ends
            lc <- cumsum(s_n)      # Cumulative from smallest
            rc <- cumsum(rev(s_n))  # Cumulative from largest
            
            if (lc[1] < threshold | rc[1] < threshold) {
              # Collapse rare levels at ends
              data[[each]][data[[each]] %in% names(rc)[rc < threshold]] <- 
                names(rc)[match(FALSE, rc < threshold)]
              data[[each]][data[[each]] %in% names(lc)[lc < threshold]] <- 
                names(lc)[match(FALSE, lc < threshold)]
              data[[each]] <- factor(data[[each]])
              warning(paste0("Ordered variable,", each, ", changed. Please remember to reset the names."))
            }
          } else {
            # For unordered factors, collapse all rare categories into "others"
            if (TRUE %in% (s_n < threshold)) {
              withCallingHandlers({
                data[[each]][data[[each]] %in% names(s_n)[s_n < threshold]] <- "others"
                data[[each]] <- factor(data[[each]])
              },
              warning = function(w) {
                if (NA %in% data[[each]]) invokeRestart("muffleWarning")
              })
            }
          }
        }
      }
    }
  }
  
  # Remove columns flagged for removal
  data <- data[, !names(data) %in% names(s_forfeited), drop = FALSE]
  return(list(df = data, ff = s_forfeited, fc = s_changed))
}

#' Convert factors to dummy variables
#'
#' This function converts factor variables into dummy variables (one-hot encoding).
#' For ordered factors, it can optionally create cumulative (monotonic) dummies.
#'
#' @param df A dataframe
#' @param var Character vector of variables to convert; if empty, uses all ordered factors
#' @param cum Logical; if TRUE, creates cumulative dummies for ordered factors (e.g., <= level k)
#' @param org Logical; if TRUE, keeps original factor columns; if FALSE, removes them
#' @return A list containing:
#'   \item{df}{Dataframe with added dummy variables}
#'   \item{fv}{Character vector of newly created dummy variable names}
#' @importFrom stats model.matrix reformulate
#' @export
factor_dumb <- function(df, var = c(), cum = TRUE, org = TRUE) {
  # If no variables specified, use all ordered factors
  if (!length(var)) {
    for (each in colnames(df)) {
      if (class(df[[each]])[1] == "ordered") {
        var <- c(var, each)
      }
    }
  }
  
  var_new <- c()
  for (each in var) {
    # Skip numeric columns and already-binary logical columns
    if ((!is.numeric(df[[each]])) && 
        (!setequal(unique(na.omit(df[[each]])), c(TRUE, FALSE)))) {
      
      a <- is.ordered(df[[each]])
      df[[each]] <- factor(df[[each]])
      lev <- levels(df[[each]])
      na <- is.na(df[[each]])
      
      # Create temporary dataframe to handle NA values
      temp_data <- df
      
      # Temporarily fill NA for model.matrix to preserve all rows
      if (sum(na) > 0) {
        temp_data[[each]] <- as.character(temp_data[[each]])
        temp_data[[each]][na] <- "TEMPORARY_NA"
        temp_data[[each]] <- factor(temp_data[[each]], 
                                    levels = c(lev, "TEMPORARY_NA"), 
                                    ordered = FALSE)
      } else {
        temp_data[[each]] <- factor(temp_data[[each]], ordered = FALSE)
      }
      
      # Create dummy variables
      if (length(unique(temp_data[[each]])) == 2) {
        col_name <- paste0(each, temp_data[[each]][1])
        temp <- data.frame(x = ifelse(temp_data[[each]] == temp_data[[each]][1], TRUE, FALSE))
        names(temp) <- col_name
      } else {
        temp <- data.frame(model.matrix(reformulate(each), data = temp_data))[, -1, drop = FALSE]
      }
      
      # Remove temporary NA dummy and restore NA values
      if (paste0(each, "TEMPORARY_NA") %in% colnames(temp)) {
        temp <- temp[, !grepl("TEMPORARY_NA", colnames(temp)), drop = FALSE]
        temp <- single_removal(temp)$df
        temp[na, ] <- NA
      }
      
      var_new <- append(var_new, colnames(temp))
      
      # Create cumulative dummies for ordered factors if requested
      if (a * cum) {
        for (i in (ncol(temp) - 1):1) {
          temp[, i] <- temp[, i] + temp[, i + 1]
        }
      }
      
      # Convert to logical type
      temp <- lapply(temp, as.logical)
      df <- cbind(df, temp)
      
      # Remove original column if requested
      if (!org) df[[each]] <- NULL
    }
  }
  return(list(df = df, fv = var_new))
}

#' Perform univariate tests for multiple dependent variables
#'
#' This function performs univariate analysis (t-test, Wilcoxon, Fisher's exact,
#' correlation) between independent and dependent variables, with automatic test selection
#' based on variable types and normality assumptions. Errors in individual tests
#' are caught and reported without stopping the entire analysis.
#'
#' @param df A dataframe
#' @param iv Character vector of independent variable names
#' @param dv Character vector of dependent variable names
#' @param threshold Significance threshold for identifying significant variables
#' @return A list containing:
#'   \item{all}{All test results for each DV, with BH-adjusted p-values}
#'   \item{significant}{Significant results (p < threshold) for each DV}
#'   \item{errors}{Optional list of error messages per DV and IV (if any)}
#' @importFrom stats shapiro.test wilcox.test t.test fisher.test cor.test kruskal.test
#' @importFrom p.adjust
#' @export
uni_test <- function(df, iv, dv, threshold = 0.05) {
  dftd <- list()
  varom <- list()
  all_results <- list()
  sig_results <- list()
  error_log <- list()  # Store errors for debugging
  
  for (DV in dv) {
    # Initialize results dataframe
    uni_all <- data.frame(
      IV = character(),
      p.value = numeric(),
      method = character(),
      statistic = numeric(),
      statistic_type = character(),
      stringsAsFactors = FALSE
    )
    
    # Prepare data: remove missing values for current DV
    dftd[[DV]] <- df[!is.na(df[[DV]]), ]
    varom[[DV]] <- single_removal(dftd[[DV]])[["fc"]]
    dftd[[DV]] <- single_removal(dftd[[DV]])[["df"]]
    varom[[DV]] <- append(varom[[DV]], l_s_s_removal(dftd[[DV]], 4)[["ff"]])
    dftd[[DV]] <- l_s_s_removal(dftd[[DV]], 4)[["df"]]
    
    if (!DV %in% names(dftd[[DV]])) {
      next
    }
    
    # Identify variables that can be analyzed
    analysis_ivs <- iv[iv != DV & !(iv %in% names(varom[[DV]])) & iv %in% names(dftd[[DV]])]
    
    # Case 1: DV is logical (binary)
    if (is.logical(df[[DV]])) {
      for (IV in analysis_ivs) {
        tryCatch({
          if (class(dftd[[DV]][[IV]])[1] == "numeric") {
            # Check normality within groups
            norm_test <- lapply(by(dftd[[DV]][[IV]], dftd[[DV]][[DV]], function(x) 
              ifelse(length(unique(x[!is.na(x)])) <= 2, 0, shapiro.test(x)$p.value)), 
              function(x) x >= 0.05)
            
            if (FALSE %in% norm_test) {
              # Non-normal: use Wilcoxon rank-sum test
              wtt <- wilcox.test(reformulate(DV, response = IV), data = dftd[[DV]])
              new_row <- data.frame(
                IV = IV, p.value = wtt$p.value, method = wtt$method,
                statistic = wtt$statistic, statistic_type = names(wtt$statistic)[1],
                stringsAsFactors = FALSE
              )
            } else {
              # Normal: use t-test
              tt <- t.test(dftd[[DV]][[IV]] ~ dftd[[DV]][[DV]])
              new_row <- data.frame(
                IV = IV, p.value = tt$p.value, method = tt$method,
                statistic = tt$statistic, statistic_type = names(tt$statistic)[1],
                stringsAsFactors = FALSE
              )
            }
            uni_all <- rbind(uni_all, new_row)
            
          } else if (class(dftd[[DV]][[IV]])[1] == "ordered") {
            # For ordered factors, use Wilcoxon
            wtt <- wilcox.test(as.numeric(dftd[[DV]][[IV]]) ~ dftd[[DV]][[DV]])
            new_row <- data.frame(
              IV = IV, p.value = wtt$p.value, method = wtt$method,
              statistic = wtt$statistic, statistic_type = names(wtt$statistic)[1],
              stringsAsFactors = FALSE
            )
            uni_all <- rbind(uni_all, new_row)
            
          } else {
            # For categorical, use Fisher's exact test
            ft <- fisher.test(xtabs(~ dftd[[DV]][[IV]] + dftd[[DV]][[DV]], 
                                    data = dftd[[DV]]), workspace = 2e7)
            new_row <- data.frame(
              IV = IV, p.value = ft$p.value, method = ft$method,
              statistic = ifelse(!is.null(ft$estimate), ft$estimate, NA),
              statistic_type = ifelse(!is.null(ft$estimate), "odds ratio", NA),
              stringsAsFactors = FALSE
            )
            uni_all <- rbind(uni_all, new_row)
          }
        }, error = function(e) {
          msg <- sprintf("Error with DV='%s', IV='%s': %s", DV, IV, e$message)
          warning(msg)
          error_log[[DV]][[IV]] <<- e$message
          return(NULL)
        })
      }
    }
    
    # Case 2: DV is ordered (ordinal)
    if (is.ordered(df[[DV]])) {
      for (IV in analysis_ivs) {
        tryCatch({
          if (class(dftd[[DV]][[IV]])[1] %in% c("numeric", "ordered")) {
            # Spearman correlation for numeric or ordered IV
            corspe <- cor.test(as.numeric(dftd[[DV]][[IV]]), as.numeric(dftd[[DV]][[DV]]), 
                               method = "spearman", exact = FALSE)
            new_row <- data.frame(
              IV = IV, p.value = corspe$p.value, method = corspe$method,
              statistic = corspe$estimate, statistic_type = names(corspe$estimate)[1],
              stringsAsFactors = FALSE
            )
            uni_all <- rbind(uni_all, new_row)
            
          } else {
            if (length(unique(na.omit(dftd[[DV]][[IV]]))) == 2) {
              # Binary categorical: use Wilcoxon
              wtt <- wilcox.test(as.numeric(dftd[[DV]][[DV]]) ~ dftd[[DV]][[IV]])
              new_row <- data.frame(
                IV = IV, p.value = wtt$p.value, method = wtt$method,
                statistic = wtt$statistic, statistic_type = names(wtt$statistic)[1],
                stringsAsFactors = FALSE
              )
              uni_all <- rbind(uni_all, new_row)
              
            } else {
              # Multicategory: use Kruskal-Wallis
              kr <- kruskal.test(reformulate(DV, response = IV), data = dftd[[DV]])
              new_row <- data.frame(
                IV = IV, p.value = kr$p.value, method = kr$method,
                statistic = kr$statistic, statistic_type = names(kr$statistic)[1],
                stringsAsFactors = FALSE
              )
              uni_all <- rbind(uni_all, new_row)
            }
          }
        }, error = function(e) {
          msg <- sprintf("Error with DV='%s', IV='%s': %s", DV, IV, e$message)
          warning(msg)
          error_log[[DV]][[IV]] <<- e$message
          return(NULL)
        })
      }
    }
    
    # Post-process results
    all_results[[DV]] <- uni_all[!is.na(uni_all$p.value), ]
    if (nrow(all_results[[DV]]) > 0) {
      all_results[[DV]] <- all_results[[DV]][order(all_results[[DV]]$p.value), ]
      all_results[[DV]][["q"]] <- p.adjust(all_results[[DV]]$p.value, method = "BH")
      sig_results[[DV]] <- all_results[[DV]][all_results[[DV]]$p.value < threshold, ]
    } else {
      sig_results[[DV]] <- uni_all[0, ]  # empty data frame
    }
    
    print(paste("After univariate test, for", DV, 
                ", there are potential independent variables:", 
                paste(sig_results[[DV]]$IV, collapse = ","), sep = " "))
  }
  
  # Return results with optional error log
  out <- list(all = all_results, significant = sig_results)
  if (length(error_log) > 0) out$errors <- error_log
  return(out)
}

#' Merge multiple univariate analysis result dataframes into a publication-ready table
#'
#' @param result_list A named list of dataframes, each containing univariate results for one outcome.
#' @param id_col Character, name of the column containing variable names. Default "IV".
#' @param p_col Character, name of the column containing p-values. Default "p.value".
#' @param method_col Character, name of the column containing test method names. Default "method".
#' @param p_thresholds Numeric vector of p-value cutoffs (any order, will be sorted ascending).
#'   Default c(0.1, 0.05, 0.01, 0.001).
#' @param p_symbols Character vector of significance symbols, same length as p_thresholds.
#'   Will be reordered to match the ascending sorted thresholds.
#'   Default c(".", "*", "**", "***").
#' @param color_palette Character vector of colors. Must have length = length(p_thresholds) + 2.
#'   Default: c("grey50", "black", "orange", "red", "darkred", "purple").
#' @param method_abbr Named character vector for abbreviating test methods.
#' @param digits Integer, number of decimal places for p-values. Default 3.
#' @param na_display Character to display for missing values. Default "\u2014" (em dash).
#' @param iv_labels Named character vector for renaming independent variables (rows).
#' @param dv_labels Named character vector for renaming dependent variables / outcomes (columns).
#' @param output_format One of "flextable", "gt", or "kable".
#'
#' @return A table object of the specified format.
#' @export
univariate_table <- function(
    result_list,
    id_col = "IV",
    p_col = "p.value",
    method_col = "method",
    p_thresholds = c(0.1, 0.05, 0.01, 0.001),
    p_symbols = c(".", "*", "**", "***"),
    color_palette = c("grey50", "black", "orange", "red", "darkred", "purple"),
    method_abbr = c(
      "t.test" = "t",
      "wilcox.test" = "Wilcoxon",
      "fisher.test" = "Fisher",
      "kruskal.test" = "Kruskal",
      "spearman" = "Spearman"
    ),
    digits = 3,
    na_display = "\u2014",
    iv_labels = NULL,
    dv_labels = NULL,
    output_format = c("flextable", "gt", "kable")
) {
  
  output_format <- match.arg(output_format)
  
  # --- Input validation ---
  if (length(p_thresholds) != length(p_symbols)) {
    stop("p_thresholds and p_symbols must have the same length.")
  }
  if (length(color_palette) != length(p_thresholds) + 2) {
    stop("color_palette must have length = length(p_thresholds) + 2.")
  }
  
  # --- Sort thresholds ascending (smallest first) and align symbols ---
  ord <- order(p_thresholds, decreasing = FALSE)
  p_thresholds <- p_thresholds[ord]
  p_symbols <- p_symbols[ord]
  n_thresh <- length(p_thresholds)
  
  # --- 1. Combine all dataframes into long format ---
  long_df <- dplyr::bind_rows(result_list, .id = "Outcome") %>%
    dplyr::select(dplyr::all_of(c("Outcome", id_col, p_col, method_col)))
  
  # --- 2. Create p-value matrix and method matrix (wide format) ---
  p_mat <- long_df %>%
    dplyr::select(-dplyr::all_of(method_col)) %>%
    tidyr::pivot_wider(names_from = "Outcome", values_from = dplyr::all_of(p_col))
  
  method_mat <- long_df %>%
    dplyr::select(-dplyr::all_of(p_col)) %>%
    tidyr::pivot_wider(names_from = "Outcome", values_from = dplyr::all_of(method_col))
  
  features <- p_mat[[id_col]]
  p_mat <- as.data.frame(p_mat[, -1, drop = FALSE])
  rownames(p_mat) <- features
  
  method_mat <- as.data.frame(method_mat[, -1, drop = FALSE])
  rownames(method_mat) <- features
  
  common_cols <- intersect(colnames(p_mat), colnames(method_mat))
  p_mat <- p_mat[, common_cols, drop = FALSE]
  method_mat <- method_mat[, common_cols, drop = FALSE]
  
  # --- Apply IV and DV renaming ---
  if (!is.null(iv_labels)) {
    new_rownames <- rownames(p_mat)
    matched <- match(new_rownames, names(iv_labels))
    new_rownames[!is.na(matched)] <- iv_labels[matched[!is.na(matched)]]
    rownames(p_mat) <- new_rownames
    rownames(method_mat) <- new_rownames
  }
  
  if (!is.null(dv_labels)) {
    new_colnames <- colnames(p_mat)
    matched <- match(new_colnames, names(dv_labels))
    new_colnames[!is.na(matched)] <- dv_labels[matched[!is.na(matched)]]
    colnames(p_mat) <- new_colnames
    colnames(method_mat) <- new_colnames
  }
  
  # --- 3. Helper: assign significance symbol based on p-value ---
  get_symbol <- function(p) {
    if (is.na(p)) return("")
    for (i in seq_along(p_thresholds)) {
      if (p <= p_thresholds[i]) return(p_symbols[i])
    }
    return("")
  }
  
  # --- 4. Helper: abbreviate test method ---
  abbr_method <- function(m) {
    if (is.na(m)) return(na_display)
    abbr <- method_abbr[m]
    if (is.na(abbr)) m else abbr
  }
  
  # --- 5. Build cell text matrix (display content) ---
  cell_text <- matrix(na_display, nrow = nrow(p_mat), ncol = ncol(p_mat))
  for (i in seq_len(nrow(p_mat))) {
    for (j in seq_len(ncol(p_mat))) {
      p <- p_mat[i, j]
      m <- method_mat[i, j]
      if (is.na(p) || is.na(m)) next
      sym <- get_symbol(p)
      p_fmt <- formatC(p, format = "f", digits = digits)
      cell_text[i, j] <- sprintf("%s: %s%s", abbr_method(m), p_fmt, sym)
    }
  }
  rownames(cell_text) <- rownames(p_mat)
  colnames(cell_text) <- colnames(p_mat)
  
  # --- 6. Color assignment based on p-value ---
  get_color <- function(p) {
    if (is.na(p)) return(color_palette[1])
    idx <- findInterval(p, p_thresholds, rightmost.closed = FALSE)
    if (idx == n_thresh) {
      return(color_palette[2])                     # non-significant
    } else if (idx == 0) {
      return(color_palette[length(color_palette)]) # most significant
    } else {
      return(color_palette[2 + idx])
    }
  }
  
  # --- 7. Generate output table ---
  if (output_format == "flextable") {
    tbl_df <- as.data.frame(cell_text, stringsAsFactors = FALSE)
    tbl_df <- cbind(Variable = rownames(tbl_df), tbl_df)
    ft <- flextable::flextable(tbl_df) %>%
      flextable::theme_vanilla() %>%
      flextable::align(j = 1, align = "left", part = "all") %>%
      flextable::align(j = 2:ncol(tbl_df), align = "center", part = "all") %>%
      flextable::bold(j = 1, part = "body") %>%
      flextable::autofit()
    
    for (i in seq_len(nrow(p_mat))) {
      for (j in seq_len(ncol(p_mat))) {
        p_val <- p_mat[i, j]
        clr <- get_color(p_val)
        ft <- flextable::color(ft, i = i, j = j + 1, color = clr)
      }
    }
    return(ft)
    
  } else if (output_format == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop("Package 'gt' is required for output_format = 'gt'")
    }
    tbl_df <- as.data.frame(cell_text, stringsAsFactors = FALSE)
    tbl_df <- cbind(Variable = rownames(tbl_df), tbl_df)
    gt_tbl <- gt::gt(tbl_df, rowname_col = "Variable") %>%
      gt::tab_style(
        style = gt::cell_text(align = "left"),
        locations = gt::cells_stub()
      ) %>%
      gt::cols_align(align = "center", columns = dplyr::everything())
    warning("Color styling not fully implemented for gt output. Use flextable for full features.")
    return(gt_tbl)
    
  } else if (output_format == "kable") {
    if (!requireNamespace("kableExtra", quietly = TRUE)) {
      stop("Package 'kableExtra' is required for output_format = 'kable'")
    }
    tbl_df <- as.data.frame(cell_text, stringsAsFactors = FALSE)
    tbl_df <- cbind(Variable = rownames(tbl_df), tbl_df)
    kbl <- kableExtra::kbl(tbl_df, format = "html", align = "c") %>%
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
    for (i in seq_len(nrow(p_mat))) {
      for (j in seq_len(ncol(p_mat))) {
        p_val <- p_mat[i, j]
        clr <- get_color(p_val)
        kbl <- kableExtra::row_spec(kbl, i, color = clr, column = j + 1)
      }
    }
    return(kbl)
  }
}

#' Generate robust starting values for ordinal logistic regression
#'
#' This function generates stable starting values for polr models by first fitting
#' a binary logistic regression on a median-split version of the ordinal outcome.
#'
#' @param formula Model formula
#' @param data Dataframe containing the variables
#' @param method Model method, typically "logistic" for polr
#' @return A named vector of starting values (beta coefficients and cutpoints)
#' @importFrom stats model.frame model.matrix model.response glm.fit qlogis
#' @export
generate_robust_start <- function(formula, data, method = "logistic") {
  # Create model frame, allowing NA (they will be excluded later)
  mf <- model.frame(formula, data, na.action = na.pass)
  complete_cases <- complete.cases(mf)
  
  if (sum(complete_cases) < nrow(data) * 0.5) {
    warning("More than 50% of observations excluded due to missingness. Starting values may be unstable.")
  }
  
  mf_complete <- mf[complete_cases, ]
  y <- model.response(mf_complete)
  
  if (!is.factor(y)) stop("Response variable must be a factor.")
  
  x <- model.matrix(formula, mf_complete)[, -1, drop = FALSE]
  n <- nrow(x)
  q <- length(levels(y)) - 1
  
  # Binarize ordinal outcome for initial fit
  y_numeric <- as.integer(y)
  y_median <- median(y_numeric)
  
  # Try two different binarization strategies to achieve better balance
  y_binary_option1 <- as.integer(y_numeric > y_median)
  y_binary_option2 <- as.integer(y_numeric >= y_median)
  
  prop_option1 <- mean(y_binary_option1)
  prop_option2 <- mean(y_binary_option2)
  
  balance_option1 <- abs(prop_option1 - 0.5)
  balance_option2 <- abs(prop_option2 - 0.5)
  
  # Choose the more balanced option
  if (balance_option1 <= balance_option2) {
    y_binary <- y_binary_option1
    if (prop_option1 < 0.1 || prop_option1 > 0.9) {
      warning(sprintf("Binary split yields imbalanced classes (%.1f%% class 1). Starting values may be unstable.", 
                      prop_option1 * 100))
    }
  } else {
    y_binary <- y_binary_option2
    if (prop_option2 < 0.1 || prop_option2 > 0.9) {
      warning(sprintf("Binary split yields imbalanced classes (%.1f%% class 1). Starting values may be unstable.", 
                      prop_option2 * 100))
    }
  }
  
  # Validate binary outcome
  if (any(is.na(y_binary))) {
    stop("NA values in binary outcome. Check data.")
  }
  
  if (sum(y_binary) < 2 || sum(1 - y_binary) < 2) {
    stop("Insufficient samples in one class after binarization.")
  }
  
  # Fit binary logistic regression
  tryCatch({
    fit <- glm.fit(cbind(1, x), y_binary, family = binomial(),control = list(maxit = 2000))
    
    if (!fit$converged) {
      warning("Binary logistic regression did not converge. Using zero starting values.")
      beta_start <- rep(0, ncol(x))
      alpha_start <- 0
    } else {
      beta_start <- fit$coefficients[-1]
      alpha_start <- fit$coefficients[1]
      if (any(is.na(beta_start))) {
        warning("Binary logistic regression produced NA coefficients. Replacing with 0.")
        beta_start[is.na(beta_start)] <- 0
      }
    }
  }, error = function(e) {
    warning(sprintf("Error fitting binary logistic regression: %s. Using zero starting values.", e$message))
    beta_start <<- rep(0, ncol(x))
    alpha_start <<- 0
  })
  
  # Generate cutpoint starting values
  spacing <- qlogis((1:q) / (q + 1))
  if (method != "logistic") spacing <- spacing / 1.7
  zeta_start <- -alpha_start + spacing - spacing[ceiling(q / 2)]
  
  names(beta_start) <- colnames(x)
  names(zeta_start) <- paste0("cut", 1:q)
  
  return(c(beta_start, zeta_start))
}

#' Perform multivariate analysis with LASSO variable selection and p‑value filtering
#'
#' @description
#' This function implements a two‑stage approach for building multivariable models
#' across multiple dependent variables (DVs).
#'
#' **Stage 1 – LASSO selection with forced variables (Strategy 2)**
#' - For each DV, a LASSO‑penalized model is fitted using all candidate predictors
#'   identified from the univariate analysis (`uni_result`).
#' - Variables listed in `keep_var` are **forced** into the model by setting their
#'   penalty factor to zero in `ordinalNetTune` (ordinal outcome) or `cv.glmnet`
#'   (binary outcome). This ensures they are never shrunk to zero.
#' - The set of predictors with non‑zero coefficients at the optimal penalty
#'   parameter is retained for the next stage.
#'
#' **Stage 2 – Full model fitting and p‑value‑based simplification**
#' - A full (unpenalized) model is fitted using the LASSO‑selected predictors
#'   (which already include any forced variables). Numeric predictors are centered
#'   to improve numerical stability of `polr`.
#' - P‑values are extracted from this model. For ordinal outcomes (`polr`),
#'   p‑values are computed via a t‑distribution approximation using residual
#'   degrees of freedom; for binary outcomes (`glm`), standard z‑test p‑values
#'   are used.
#' - A Benjamini‑Hochberg (BH) correction is applied to all coefficients except
#'   intercepts and cutpoints. Predictors with adjusted p‑value < `threshold` are
#'   retained for the final model.
#' - If the Hessian matrix of the full model is singular (e.g., due to complete
#'   separation), the standard p‑value extraction becomes unreliable. In such
#'   cases, variable simplification is instead performed using **likelihood ratio
#'   tests (LRT)** via backward elimination (`drop1` for binary models or custom
#'   LRT for `polr` models). The same `threshold` is applied to the LRT p‑values.
#'
#' **Stage 3 – Final model refit**
#' - A final model is refit using only the retained predictors.
#' - Forced variables (`keep_var`) are re‑added after simplification to ensure
#'   their presence in the final model, regardless of statistical significance.
#'
#' @param df A data frame containing all variables.
#' @param uni_result A named list, typically the `$significant` component returned
#'   by `uni_test`. Each element corresponds to a DV and contains at least a
#'   character vector `IV` listing candidate independent variables.
#' @param threshold Numeric significance threshold for BH‑adjusted p‑values
#'   (and for LRT when Hessian is singular). Default is `0.05`.
#'   Use `threshold = 1` to retain all LASSO‑selected variables.
#' @param keep_var Character vector of variable names to be **forced** into the
#'   LASSO selection step (penalty factor = 0) and **re‑added** after p‑value
#'   filtering to guarantee their inclusion in the final model.
#'
#' @return A named list of final fitted model objects (one per DV). Each element is
#'   either a `polr` object (ordinal DV) or a `glm` object (binary DV). If model
#'   fitting fails completely, the corresponding entry is `NULL`.
#'
#' @importFrom glmnet cv.glmnet
#' @importFrom ordinalNet ordinalNetTune
#' @importFrom MASS polr
#' @importFrom stats glm reformulate p.adjust drop1
#' @export
mul_test <- function(df, uni_result, threshold = 0.05, keep_var = c()) {
  dftd <- list()
  varf <- list()
  var2 <- list()
  var3 <- list()
  fir_re <- list()
  var4 <- list()
  fin <- list()
  
  for (DV in names(uni_result)) {
    # ----------------------------------------------------------------------
    # 1. Data preparation
    # ----------------------------------------------------------------------
    dftd[[DV]] <- na.omit(df[, unique(c(uni_result[[DV]]$IV, keep_var, DV))])
    e_DV <- dftd[[DV]][[DV]]
    
    # Identify factor variables for dummy coding
    for (IV in uni_result[[DV]]$IV) {
      if (!is.numeric(dftd[[DV]][[IV]]) & !is.logical(dftd[[DV]][[IV]])) {
        varf[[DV]] <- c(varf[[DV]], IV)
      }
    }
    
    # Convert factors to dummy variables
    if (length(varf[[DV]]) > 0) {
      dftd[[DV]] <- factor_dumb(dftd[[DV]], varf[[DV]], org = FALSE)$df
    }
    
    # ----------------------------------------------------------------------
    # 2. LASSO variable selection
    # ----------------------------------------------------------------------
    if (ncol(dftd[[DV]]) > 2) {
      set.seed(109)
      if (is.ordered(e_DV)) {
        # Ordinal outcome: ordinalNet with LASSO
        ord_log_cv <- ordinalNetTune(
          as.matrix(dftd[[DV]][, colnames(dftd[[DV]]) != DV]),
          e_DV,
          alpha = 1,
          nFolds = max(3, floor(nrow(dftd[[DV]]) / 20)),
          standardize = TRUE,
          maxiterOut = 10000
        )
        coefs <- ord_log_cv$fit[[1]][which.max(rowMeans(ord_log_cv$loglik)), ]
        var2[[DV]] <- names(coefs[coefs != 0 & !grepl("Intercept", names(coefs))])
      } else {
        # Binary outcome: glmnet
        fitcv <- cv.glmnet(
          as.matrix(dftd[[DV]][, colnames(dftd[[DV]]) != DV]),
          as.matrix(dftd[[DV]][, DV]),
          family = "binomial",
          type.measure = "deviance",
          alpha = 1,
          nfolds = 5,
          standardize = TRUE,
          maxit = 10000000
        )
        coef <- coef(fitcv, s = "lambda.1se")
        if (length(rownames(coef)[which(coef != 0)][-1]) == 0) {
          coef <- coef(fitcv, s = fitcv$lambda[tapply(seq_along(fitcv$cvm), fitcv$cvm, max)[as.character(min(fitcv$cvm))]])
        }
        var2[[DV]] <- rownames(coef)[which(coef != 0)][-1]
      }
      message(sprintf("After LASSO, for %s: %s",
                      DV, paste(var2[[DV]], collapse = ", ")))
    } else {
      var2[[DV]] <- keep_var
    }
    
    # ----------------------------------------------------------------------
    # 3. Map dummy variable names back to original variable names
    # ----------------------------------------------------------------------
    dftd[[DV]] <- df   # restart from original data
    var3[[DV]] <- c()
    
    for (var_v in var2[[DV]]) {
      for (var_o in uni_result[[DV]]$IV) {
        if (length(grep(var_o, var_v))) {
          if (var_o != var_v & length(unique(na.omit(df[[var_o]]))) > 2) {
            dftd[[DV]] <- factor_dumb(dftd[[DV]], c(var_o))$df
            var3[[DV]] <- append(var3[[DV]], var_v)
          } else {
            var3[[DV]] <- append(var3[[DV]], var_o)
          }
        }
      }
    }
    
    var3[[DV]] <- c(var3[[DV]], keep_var)
    var3[[DV]] <- unique(var3[[DV]])
    var3[[DV]] <- var3[[DV]][!is.na(var3[[DV]])]
    dftd[[DV]] <- dftd[[DV]][, !duplicated(colnames(dftd[[DV]]))]
    
    # ----------------------------------------------------------------------
    # 4. Center numeric predictors for numerical stability
    # ----------------------------------------------------------------------
    temp_df <- na.omit(dftd[[DV]][, unique(c(var3[[DV]], DV))])
    
    numeric_vars <- names(temp_df)[sapply(temp_df, is.numeric) &
                                     names(temp_df) != DV &
                                     !sapply(temp_df, function(x) all(unique(na.omit(x)) %in% c(0, 1)))]
    
    if (length(numeric_vars) > 0) {
      for (var in numeric_vars) {
        if (sd(temp_df[[var]], na.rm = TRUE) > 0) {
          temp_df[[var]] <- temp_df[[var]] - mean(temp_df[[var]], na.rm = TRUE)
        }
      }
    }
    
    temp_df_1 <- na.omit(temp_df)
    
    # ----------------------------------------------------------------------
    # 5. Fit full model with LASSO-selected variables
    # ----------------------------------------------------------------------
    if (is.ordered(temp_df_1[[DV]])) {
      fir_re[[DV]] <- tryCatch({
        polr(reformulate(var3[[DV]], response = DV),
             data = temp_df_1,
             Hess = TRUE,
             method = "logistic",
             control = list(maxit = 1000, reltol = 1e-8))
      }, error = function(e) {
        warning(sprintf("POLR fitting failed for %s: %s", DV, e$message))
        tryCatch({
          start_vals <- generate_robust_start(reformulate(var3[[DV]], response = DV), temp_df_1)
          polr(reformulate(var3[[DV]], response = DV),
               data = temp_df_1,
               Hess = TRUE,
               method = "logistic",
               start = start_vals,
               control = list(maxit = 2000, reltol = 1e-6))
        }, error = function(e2) {
          warning(sprintf("POLR with robust start also failed for %s: %s", DV, e2$message))
          return(NULL)
        })
      })
    } else {
      fir_re[[DV]] <- glm(reformulate(var3[[DV]], response = DV),
                          family = binomial(),
                          data = temp_df_1,
                          control = list(maxit = 100))
    }
    
    # ----------------------------------------------------------------------
    # 6. Model simplification (p-value filtering or LRT)
    # ----------------------------------------------------------------------
    if (!is.null(fir_re[[DV]]) && !inherits(fir_re[[DV]], "try-error")) {
      if (inherits(fir_re[[DV]], "polr")) {
        # Check whether Hessian is invertible
        hess_ok <- tryCatch({
          vc <- suppressWarnings(vcov(fir_re[[DV]]))
          all(is.finite(vc))
        }, error = function(e) FALSE)
        
        if (!hess_ok) {
          # ---- Hessian singular: use LRT p-values instead of Wald ----
          warning(sprintf("Hessian singular for %s; using LRT (drop1) p-values.", DV))
          
          lrt_table <- tryCatch(
            drop1(fir_re[[DV]], test = "Chisq"),
            error = function(e) {
              warning(sprintf("drop1 failed for %s: %s. Returning unsimplified model.", DV, e$message))
              return(NULL)
            }
          )
          
          if (is.null(lrt_table)) {
            fin[[DV]] <- fir_re[[DV]]
            next
          }
          
          # Extract LRT p-values for each term (excluding <none>)
          lrt_pvals <- lrt_table[-1, "Pr(>Chi)"]
          names(lrt_pvals) <- rownames(lrt_table)[-1]
          
          # Map term names to coefficient names (for subsequent matching with var3)
          coef_names_all <- names(coef(fir_re[[DV]]))
          term_to_coefs <- list()
          for (term in names(lrt_pvals)) {
            matched <- grep(paste0("^", term, "$|^", term, "[^a-zA-Z0-9_]"), coef_names_all, value = TRUE)
            if (length(matched) == 0) matched <- term
            term_to_coefs[[term]] <- matched
          }
          
          # Assign LRT p-values to corresponding coefficients
          pvalue <- setNames(rep(NA_real_, length(coef_names_all)), coef_names_all)
          for (term in names(lrt_pvals)) {
            pvalue[term_to_coefs[[term]]] <- lrt_pvals[term]
          }
          pvalue <- pvalue[!is.na(pvalue)]
          
        } else {
          # ---- Hessian OK: standard Wald p-values ----
          coef_summary <- coef(summary(fir_re[[DV]]))
          t_values <- coef_summary[, "t value"]
          df_res <- df.residual(fir_re[[DV]])
          pvalue <- 2 * pt(abs(t_values), df = df_res, lower.tail = FALSE)
          names(pvalue) <- rownames(coef_summary)
        }
      } else {
        # Binary outcome (glm): use standard Wald p-values
        pvalue <- coef(summary(fir_re[[DV]]))[, 4]
      }
      
      # ---- Common p-value adjustment and variable selection ----
      # Identify indices to remove from BH correction:
      # 1. Intercepts and cutpoints (polr)
      # 2. Coefficients corresponding to forced variables (keep_var)
      rem <- grep("Intercept|\\|", names(pvalue))
      for (kv in keep_var) {
        rem <- c(rem, grep(kv, names(pvalue), fixed = TRUE))
      }
      rem <- unique(rem)
      
      if (length(pvalue) > length(rem)) {
        padj <- p.adjust(pvalue[-rem], method = "BH")
        sig_names <- names(padj[padj < threshold])
      } else {
        sig_names <- character(0)
      }
      
      # Build final variable set: keep those with significant coefficients
      var4[[DV]] <- character(0)
      for (each in var3[[DV]]) {
        if (any(grepl(each, sig_names, fixed = TRUE))) {
          var4[[DV]] <- c(var4[[DV]], each)
        }
      }
      
      # Re-add forced variables (ensure they are retained)
      var4[[DV]] <- unique(c(var4[[DV]], keep_var))
      
      # --------------------------------------------------------------------
      # 7. Refit final simplified model
      # --------------------------------------------------------------------
      if (length(var4[[DV]]) == 0) {
        warning(sprintf("No predictors retained for %s. Returning intercept-only model.", DV))
        if (is.ordered(temp_df[[DV]])) {
          fin[[DV]] <- polr(as.formula(paste(DV, "~ 1")), data = temp_df, Hess = TRUE, method = "logistic")
        } else {
          fin[[DV]] <- glm(as.formula(paste(DV, "~ 1")), family = binomial(), data = temp_df)
        }
        next
      }
      
      temp_df_final <- temp_df[, c(var4[[DV]], DV)]
      temp_df_final <- na.omit(temp_df_final)
      
      if (is.ordered(temp_df_final[[DV]])) {
        fin[[DV]] <- tryCatch({
          polr(reformulate(var4[[DV]], response = DV),
               data = temp_df_final,
               Hess = TRUE,
               method = "logistic",
               control = list(maxit = 1000, reltol = 1e-8))
        }, error = function(e) {
          warning(sprintf("Final POLR fitting failed for %s: %s", DV, e$message))
          return(fir_re[[DV]])   # fallback to unsimplified model
        })
      } else {
        fin[[DV]] <- glm(reformulate(var4[[DV]], response = DV),
                         family = binomial(),
                         data = temp_df_final,
                         control = list(maxit = 100))
      }
    } else {
      warning(sprintf("Model fitting completely failed for %s", DV))
      fin[[DV]] <- NULL
    }
  }
  
  return(fin)
}


#' Run mediation analysis for multiple outcome models
#'
#' This function performs mediation analysis for a list of outcome models,
#' preprocessing data appropriately and testing each mediator. It uses the
#' mediation package and handles various variable types.
#'
#' @param model_list A named list of fitted models (glm or polr objects)
#' @param data Original dataframe
#' @param pop_var Character vector of demographic variables for preprocessing
#' @param mid_var Character vector of mediator variable names
#' @param treat Name of treatment/independent variable 
#' @param boot Logical; if TRUE, use bootstrap for inference
#' @return A list containing:
#'   \item{processed_data}{Preprocessed dataframe}
#'   \item{mediator_list}{Processed mediator variable names}
#'   \item{all_results}{All mediation results}
#'   \item{significant_results}{Significant mediation results}
#'   \item{significant_summaries}{Summaries of significant results}
#' @importFrom mediation mediate
#' @importFrom MASS polr
#' @importFrom forcats fct_other
#' @export
run_mediation_analysis <- function(
    model_list,
    data,
    pop_var,
    mid_var,
    treat,
    boot = TRUE
) {
  
  require(mediation)
  
  cat("Step 1: Creating dummy variables for demographic variables...\n")
  
  # Convert demographic variables to dummies (keeping original columns)
  factor_dumb_result <- factor_dumb(data, pop_var, org = TRUE)
  data_g_mid <- factor_dumb_result$df
  
  cat("Step 2: Creating dummy variables for mediators...\n")
  m <- factor_dumb(data_g_mid, mid_var, org = FALSE)
  mid_var <- c(mid_var, m$fv)
  data_g_mid <- m$df
  mid_var <- mid_var[mid_var %in% colnames(data_g_mid)]
  
  # Ensure treatment variable is numeric
  data_g_mid[[treat]] <- as.numeric(data_g_mid[[treat]])
  
  med_ans <- list()
  
  cat("Step 3: Encode binary variables as 0/1...\n")
  for (mid in mid_var) {
    if (is.logical(data_g_mid[[mid]])) {
      data_g_mid[[mid]] <- as.numeric(data_g_mid[[mid]])
    }
    
    if (length(unique(na.omit(data_g_mid[[mid]]))) == 2 && 
        !setequal(unique(na.omit(data_g_mid[[mid]])), c(0, 1))) {
      val <- na.omit(data_g_mid[[mid]])[1]
      data_g_mid[[mid]] <- ifelse(data_g_mid[[mid]] == val, 1, 0)
      mid_c <- paste0(mid, "_", val)
      mid_var[mid_var == mid] <- mid_c
      colnames(data_g_mid)[colnames(data_g_mid) == mid] <- mid_c
    }
  }
  
  cat("Preprocessing complete. Processed mediators:\n")
  print(mid_var)
  
  cat("\nStarting mediation analysis...\n")
  
  for (i in seq_along(model_list)) {
    each <- model_list[[i]]
    DV <- names(each$model)[1]
    
    cat(sprintf("\nAnalyzing model %d/%d: %s\n", i, length(model_list), DV))
    
    med_ans[[DV]] <- list()
    cov <- names(each$model)[-1]
    
    fin_cla <- class(each)[1]
    
    # Analyze each mediator
    for (mid in mid_var) {
      cat("  Testing mediator:", mid, "\n")
      
      # Create unique prefix for temporary objects in global environment
      unique_prefix <- paste0("mediate_init_", sample(10000:99999, 1), "_")
      
      # Assign data and formulas to global environment for mediation analysis
      global_data_name <- paste0(unique_prefix, "data")
      assign(global_data_name, data_g_mid, envir = .GlobalEnv)
      
      global_mid_formula_name <- paste0(unique_prefix, "formula_mid")
      mid_for <- reformulate(cov, response = mid)
      assign(global_mid_formula_name, mid_for, envir = .GlobalEnv)
      
      global_y_formula_name <- paste0(unique_prefix, "formula_y")
      current_formula_temp <- reformulate(c(cov, mid), response = DV)
      assign(global_y_formula_name, current_formula_temp, envir = .GlobalEnv)
      
      # Attempt mediation analysis
      mediate_result <- tryCatch(
        {
          # Fit mediator model
          if (class(get(global_data_name, envir = .GlobalEnv)[[mid]]) == "numeric" && 
              length(unique(na.omit(get(global_data_name, envir = .GlobalEnv)[[mid]]))) > 2) {
            fam <- "gaussian()"
          } else {
            fam <- "binomial()"
          }
          
          mid_mod_call_str <- sprintf(
            "glm(formula = %s, data = %s, family = %s)",
            global_mid_formula_name, global_data_name, fam
          )
          mid_mod <- eval(parse(text = mid_mod_call_str))
          
          # Fit outcome model
          if (fin_cla == "polr") {
            method_to_use <- if (!is.null(each$method)) each$method else "logistic"
            start_vals <- generate_robust_start(
              get(global_y_formula_name, envir = .GlobalEnv),
              get(global_data_name, envir = .GlobalEnv),
              method = method_to_use
            )
            
            polr_call_str <- sprintf(
              "MASS::polr(formula = %s, data = %s, Hess = TRUE, method = '%s', start = c(%s))",
              global_y_formula_name, global_data_name, method_to_use,
              paste(start_vals, collapse = ", ")
            )
            fin_mod <- eval(parse(text = polr_call_str))
            
          } else {
            glm_call_str <- sprintf(
              "glm(formula = %s, data = %s, family = %s)",
              global_y_formula_name, global_data_name, deparse(each$family$family)
            )
            fin_mod <- eval(parse(text = glm_call_str))
          }
          
          # Perform mediation analysis
          mediation::mediate(mid_mod, fin_mod,
                             treat = treat,
                             mediator = mid,
                             boot = boot,
                             sims = ifelse(boot, 1000, 1))
        },
        error = function(e) {
          cat("    First attempt failed, trying simplified covariates...\n")
          
          # Simplify covariate structure by collapsing dummy variables
          cov_temp <- cov
          data_g_mid_temp <- data_g_mid[!is.na(data_g_mid[[mid]]), ]
          
          for (pop in pop_var) {
            if (TRUE %in% grepl(pop, cov_temp) & !pop %in% cov_temp) {
              keep_var <- gsub(pop, "", cov_temp[grepl(pop, cov_temp)])
              
              if (length(keep_var) >= 2) {
                if (is.ordered(data_g_mid[[pop]])) {
                  data_g_mid_temp[[paste0(pop, "_temp")]] <- 
                    rowSums(data_g_mid_temp[, cov_temp[grepl(pop, cov_temp)]])
                } else {
                  data_g_mid_temp[[paste0(pop, "_temp")]] <- 
                    forcats::fct_other(data_g_mid_temp[[pop]], keep = keep_var)
                }
                cov_temp <- cov_temp[-grep(pop, cov_temp)]
                cov_temp <- c(cov_temp, paste0(pop, "_temp"))
              }
            }
          }
          
          # Create new temporary objects with simplified covariates
          unique_prefix <- paste0("mediate_temp_", sample(10000:99999, 1), "_")
          
          global_data_name <- paste0(unique_prefix, "data")
          assign(global_data_name, data_g_mid_temp, envir = .GlobalEnv)
          
          global_y_formula_name <- paste0(unique_prefix, "formula_y")
          current_formula_temp <- reformulate(c(cov_temp, mid), response = DV)
          assign(global_y_formula_name, current_formula_temp, envir = .GlobalEnv)
          
          global_m_formula_name <- paste0(unique_prefix, "formula_m")
          med_formula_temp <- reformulate(cov_temp, response = mid)
          assign(global_m_formula_name, med_formula_temp, envir = .GlobalEnv)
          
          # Refit models with simplified covariates
          if (fin_cla == "polr") {
            method_to_use <- if (!is.null(each$method)) each$method else "logistic"
            start_vals <- generate_robust_start(current_formula_temp, data_g_mid_temp, method = method_to_use)
            
            polr_call_str <- sprintf(
              "MASS::polr(formula = %s, data = %s, Hess = TRUE, method = '%s', start = c(%s))",
              global_y_formula_name, global_data_name, method_to_use,
              paste(start_vals, collapse = ", ")
            )
            fin_mod_temp <- eval(parse(text = polr_call_str))
            
          } else {
            glm_call_str <- sprintf(
              "glm(formula = %s, data = %s, family = %s)",
              global_y_formula_name, global_data_name, deparse(each$family$family)
            )
            fin_mod_temp <- eval(parse(text = glm_call_str))
          }
          
          # Fit mediator model
          if (class(data_g_mid_temp[[mid]]) == "numeric" && 
              length(unique(na.omit(data_g_mid_temp[[mid]]))) > 2) {
            fam <- "gaussian()"
          } else {
            fam <- "binomial()"
          }
          
          mid_mod_call_str <- sprintf(
            "glm(formula = %s, data = %s, family = %s)",
            global_m_formula_name, global_data_name, fam
          )
          mid_mod_temp <- eval(parse(text = mid_mod_call_str))
          
          # Perform mediation analysis with simplified models
          mediate_result <- mediate(mid_mod_temp, fin_mod_temp,
                                    treat = treat,
                                    mediator = mid,
                                    boot = boot,
                                    sims = ifelse(boot, 1000, 1))
          
          # Clean up temporary objects
          cleanup_names <- c(global_data_name, global_y_formula_name, global_m_formula_name)
          rm(list = cleanup_names, envir = .GlobalEnv)
          
          return(mediate_result)
        }
      )
      
      # Clean up initial temporary objects
      cleanup_names <- c(global_data_name, global_mid_formula_name, global_y_formula_name)
      rm(list = cleanup_names, envir = .GlobalEnv)
      
      # Store results
      med_ans[[DV]][[mid]] <- mediate_result
      
      # Print summary
      if (!inherits(mediate_result, "try-error")) {
        cat("    Results: ACME(control)=", round(mediate_result$d0, 3),
            "(p=", round(mediate_result$d0.p, 3), "), ",
            "ACME(treated)=", round(mediate_result$d1, 3),
            "(p=", round(mediate_result$d1.p, 3), ")\n", sep = "")
      }
    }
  }
  
  # Identify significant results
  cat("\nIdentifying significant mediation effects...\n")
  significant_results <- list()
  significant_summaries <- list()
  
  for (DV in names(med_ans)) {
    for (mid in names(med_ans[[DV]])) {
      result <- med_ans[[DV]][[mid]]
      
      if (!is.null(result) && 
          !inherits(result, "try-error") &&
          !is.null(result$d0.p) && !is.null(result$d1.p) &&
          (TRUE %in% c(result$d0.p <= 0.05) || TRUE %in% c(result$d1.p <= 0.05))) {
        
        key <- paste0(DV, "_", mid)
        significant_results[[key]] <- result
        significant_summaries[[key]] <- summary(result)
        
        cat(sprintf("Significant effect: %s via %s\n", DV, mid))
        cat(sprintf("  ACME(control): %.3f (p=%.3f)\n", result$d0, result$d0.p))
        cat(sprintf("  ACME(treated): %.3f (p=%.3f)\n", result$d1, result$d1.p))
      }
    }
  }
  
  if (length(significant_results) == 0) {
    cat("No significant mediation effects found.\n")
  }
  
  cat("\nAnalysis complete!\n")
  
  return(list(
    processed_data = data_g_mid,
    mediator_list = mid_var,
    all_results = med_ans,
    significant_results = significant_results,
    significant_summaries = significant_summaries
  ))
}

#' Create list of formatted regression tables
#'
#' This function generates a list of gtsummary tables for multiple models.
#' Variables in each table are ordered by ascending p-value (most significant first).
#' Optionally, a single variable can be forced to the top of the table, with the
#' remaining variables still ordered by p-value.
#'
#' @param model_list Named list of fitted model objects (e.g., from glm or polr).
#' @param keep_var Optional character string naming a single variable to place at
#'        the very top of each table. Must be either `NULL` (no variable forced
#'        to the top) or a single non-empty character string. If the specified
#'        variable is missing from a particular model, a warning is issued and
#'        that table is sorted only by p-value (without a forced top variable).
#'        Default is `NULL`.
#' @param extra_modify_map Optional named list of functions for additional
#'        modifications per table. The names must match those in `model_list`.
#'        Each function takes a `tbl_regression` object and returns a modified one.
#' @return A list of gtsummary `tbl_regression` objects, with the same names as
#'         `model_list`. Within each table, rows are ordered by ascending p-value,
#'         with `keep_var` placed first if provided and present.
#' @importFrom gtsummary tbl_regression add_global_p add_q italicize_levels
#'             modify_caption modify_header remove_abbreviation
#' @importFrom purrr imap map map_dfr
#' @importFrom dplyr filter select distinct arrange
#' @export
make_tbl_list <- function(model_list, 
                          keep_var = NULL,
                          extra_modify_map = NULL) {
  
  # Validate keep_var argument
  if (!is.null(keep_var)) {
    if (length(keep_var) != 1 || is.na(keep_var) || nchar(trimws(keep_var)) == 0) {
      stop("`keep_var` must be NULL or a single non-empty character string.", call. = FALSE)
    }
  }
  
  use_keep <- !is.null(keep_var)
  
  # Generate base tables
  tbl_list <- imap(model_list, function(model, name) {
    tbl <- tbl_regression(
      model,
      pvalue_fun = ~style_pvalue(.x, digits = 3),
      show_single_row = where(is.logical)
    ) %>%
      add_global_p() %>%
      add_q() %>%
      italicize_levels() %>%
      modify_caption(paste("**", name, "**")) %>%
      modify_header(estimate = "**β**") %>%
      remove_abbreviation("OR = Odds Ratio")
    
    if (!is.null(extra_modify_map[[name]])) {
      tbl <- extra_modify_map[[name]](tbl)
    }
    return(tbl)
  })
  
  # Reorder each table: always sort by p-value (ascending), with optional keep_var at top
  reordered_list <- map(tbl_list, function(tbl) {
    tbl_data <- tbl$table_body
    var_pvals <- tbl_data %>%
      dplyr::filter(row_type == "label") %>%
      dplyr::select(variable, p.value) %>%
      distinct()
    
    # Check existence of keep_var in this model if requested
    if (use_keep && !(keep_var %in% var_pvals$variable)) {
      warning("Variable '", keep_var, "' not found in model table. It will be ignored for ordering in this table.")
      use_keep_local <- FALSE
    } else {
      use_keep_local <- use_keep
    }
    
    # Order all variables by p-value (ascending)
    ordered_vars <- var_pvals %>% arrange(p.value) %>% pull(variable)
    
    # If keep_var is valid and exists, move it to the front
    if (use_keep_local) {
      ordered_vars <- c(keep_var, setdiff(ordered_vars, keep_var))
    }
    
    # Reorder the table body rows according to the new variable order
    reordered_data <- map_dfr(ordered_vars, function(var) {
      tbl_data %>% dplyr::filter(variable == var)
    })
    tbl$table_body <- reordered_data
    tbl
  })
  
  return(reordered_list)
}

#' Stack multiple regression tables into a single table
#'
#' This function stacks a list of regression tables and adds captions,
#' footnotes, and optional export functionality.
#'
#' @param tbl_list List of gtsummary table objects
#' @param caption Main table caption
#' @param footnote_map Named list mapping variable names to footnote text
#' @param output_file Optional file path for saving as docx
#' @return Combined gtsummary table object
#' @importFrom gtsummary tbl_stack modify_caption modify_column_hide bold_labels italicize_levels modify_footnote_body as_flex_table
#' @importFrom flextable save_as_docx
#' @export
stack_tables <- function(tbl_list, 
                         caption = "temp", 
                         footnote_map = NULL,
                         output_file = NULL) {
  
  combined <- tbl_stack(tbl_list, group_header = names(tbl_list)) %>%
    modify_caption(paste0("**", caption, "**")) %>%
    modify_column_hide(columns = c(variable)) %>%
    bold_labels() %>%
    italicize_levels()
  
  # Add footnotes using reduce for cumulative application
  if (!is.null(footnote_map)) {
    combined <- reduce(
      names(footnote_map),
      function(tbl, var_name) {
        modify_footnote_body(
          tbl,
          footnote = footnote_map[[var_name]],
          columns = "label",
          rows = variable == var_name & row_type == "label"
        )
      },
      .init = combined
    )
  }
  
  # Save if output file is specified
  if (!is.null(output_file)) {
    combined %>%
      as_flex_table() %>%
      save_as_docx(path = output_file)
  }
  
  return(combined)
}

