#' Define your target
#'
#'
#'
#' @param target_variable Name of a column with statuses
#' @param positive_class_indication Name of a class within target variable that should be treated as success (positive outcome)
#' @param phenotype_df Name of a dataframe name containing the phenotype/clinical data
#' @param id_variable Name of a column with subjects/samples IDs
#'
#' @return An object containing the most important information about defined target
#'
#' @examples
#' my_target <- define_target(phenotype_df = "clinical_data", target_variable = "survived", id_variable = "patient_id", positive_class_indication = "1")
#'
#' @export
define_target <- function(target_variable, positive_class_indication, phenotype_df, id_variable){
  list(
    target_variable =  make.names(target_variable), # to be coherent with data prepared for modelling
    positive_class = positive_class_indication,
    phenotype_df = phenotype_df,
    id_variable =  make.names(id_variable) # to be coherent with data prepared for modelling
  )
}

#' Title
#'
#' description
#'
#' @param data
#' @param target A target object defined previously
#'
#' @return List of datasets, each dataset includes target column
#'
#' @examples
#' data_with_target <- add_target(data = my_data, target = my_target)
#'
#' @export
prepare_data_for_modelling <- function(data, target) {
  target_data <-
    data[[target$phenotype_df]] %>%
    dplyr::select(target$id_variable, target$target_variable) %>%
    dplyr::mutate(!!rlang::sym(target$target_variable) := as.factor(!!rlang::sym(target$target_variable)))

  keep_elements_names <- names(data)

  data <-
    lapply(names(data), function(x){
      logger::log_info("Preparing {x} data")
      if(x == target$phenotype_df){ # don't change phenotype df
        data[[x]]
      } else {
        data[[x]] %>%
          dplyr::left_join(target_data, by = target$id_variable) %>%
          dplyr::filter(!is.na(!!rlang::sym(target$target_variable)))
      }
    })

  names(data) <- keep_elements_names

  lapply(data, function(mydata) {
    # one-hot encoding for factors and characters
    if(length(setdiff(names(mydata %>% dplyr::select(which(sapply(.,class)!="numeric"))), c(target$id_variable, target$target_variable))) > 0){
      mydata <-
        mydata %>%
        recipes::recipe( ~ .) %>%
        recipes::step_dummy(recipes::all_nominal_predictors(), -target$id_variable, -target$target_variable, one_hot = T) %>%
        recipes::prep() %>%
        recipes::bake(mydata) %>%
        dplyr::mutate_if(is.logical, as.integer)
    } else {
      mydata
    }

    names(mydata) <- make.names(names(mydata))

    mydata %>%
      dplyr::filter(!is.na(!!rlang::sym(target$target_variable)))
  })
}
