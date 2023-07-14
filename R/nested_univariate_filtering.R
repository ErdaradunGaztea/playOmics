rank_features <- function(resample, target, filter_name, n_threads = 1) {
  t_id <- target[["id_variable"]]
  t_var <- target[["target_variable"]]
  t_pos <- target[["positive_class"]]

  filter <- mlr3filters::flt(filter_name)
  # Set threads for all filters which support it
  mlr3::set_threads(filter, n_threads)

  resample %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      task = rsample::training(splits) %>%
        dplyr::select(-any_of(t_id)) %>%
        mlr3::as_task_classif(id = id, target = !!t_var, positive = !!t_pos) %>%
        list(),
      score = filter$calculate(task) %>%
        # Unfortunately mlr3filters can only convert to a data.table
        data.table::as.data.table() %>%
        list()
    ) %>%
    dplyr::pull(score) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(feature) %>%
    dplyr::summarise(
      variance = var(score, na.rm = TRUE),
      score = mean(score, na.rm = TRUE)
    ) %>%
    dplyr::arrange(dplyr::desc(score))
}

select_features <- function(data, ranking, target, cutoff_method, cutoff_treshold) {
  vars <- switch(
    cutoff_method,
    top_n = ranking %>%
      dplyr::slice_max(order_by = score, n = cutoff_treshold, with_ties = FALSE, na_rm = TRUE),
    percentage = ranking %>%
      dplyr::slice_max(order_by = score, prop = cutoff_treshold / 100, with_ties = FALSE, na_rm = TRUE),
    threshold = ranking %>%
      dplyr::filter(score > cutoff_treshold),
    stop(sprintf("Cutoff method '%s' not available.", cutoff_method))
  )

  data %>%
    dplyr::select(
      !!target$id_variable,
      dplyr::all_of(dplyr::pull(vars, feature)),
      !!target$target_variable
    )
}

#' Univariate feature selection
#'
#' Select variables related to a target. Features are selected based on ranking that is created depending of selected method.
#' Then, three methods of selection can be chosen:
#'
#' @param data Name of a dataframe name containing the phenotype/clinical data
#' @param target Name of a column with statuses
#' @param filter_name Name of a filter to be applied (https://mlr.mlr-org.com/articles/tutorial/filter_methods.html#current-methods, column "Classif")
#' @param cutoff_method One of the following: top_n, percentage, threshold
#' @param cutoff_treshold Depending of a cutoff method, a number of features to be selected, percentage of variables to be selected, a threshold above which features are selected.
#' @param n_threads Number of threads for feature selection (as default set to 1)
#'
#' @return filtered set of data
#'
#' @examples
#' filtered_data <- nested_filtering(data = data_prepared, target = target, filter_name = "auc", cutoff_method = "top_n", cutoff_treshold = 10, n_threads = 10)
#' @export
nested_filtering <- function(
    data, target, filter_name = "auc", cutoff_method = "top_n",
    cutoff_treshold = 10, n_threads = 1, nfold = 5
) {
  purrr::imap(data, function(frame, label) {
    logger::log_info("Ranking {label} data")

    resample <- rsample::vfold_cv(
      frame,
      v = min(nfold, nrow(frame)),
      strata = target$target_variable
    )

    ranking <- rank_features(
      resample, target, filter_name = filter_name, n_threads = n_threads
    )

    ret <- select_features(
      frame, ranking, target,
      cutoff_method = cutoff_method, cutoff_treshold = cutoff_treshold
    )

    logger::log_info("Kept {ncol(ret)} features")

    ret
  })
}
