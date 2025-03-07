#' Annotate open-ended survey responses within a `svydesign` object
#' into CoNLL-U format
#'
#' Creates a dataframe in CoNLL-U format from a `svydesign` object including
#' text using the [udpipe] package and a language model plus
#' weights if these are included in the `svydesign` object and any columns added
#' through `add_cols`.
#'
#' @param svydesign A `svydesign` object which contains an open-ended question.
#' @param question The column in the dataframe which contains the open-ended
#'  question.
#' @param id The column in the dataframe which contains the ids for the
#'  responses.
#' @param model A language model available for [udpipe]. `"ftb"`
#'  (default) or `"tdt"` are recognised as shorthand for "finnish-ftb" and
#'  "finnish-tdt". The full list is available in the [udpipe] documentation or
#'  via `fst_print_available_models()`.
#' @param use_weights Optional, whether to use weights within the `svydesign`
#' @param add_cols Optional, a column (or columns) from the dataframe which
#'  contain other information you'd like to retain (for instance, dimension
#'  columnns for splitting the data for comparison plots).
#'
#' @return Dataframe of annotated text in CoNLL-U format plus any additional
#'  columns.
#' @export
#'
#' @examples
#' \dontrun{
#' i <- "fsd_id"
#' svy_child <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' fst_format_svydesign(svy_child, question = 'q7', id = 'fsd_id')
#' fst_format_svydesign(svy_child, question = 'q7', id = i, use_weights = FALSE)
#' cols <- c('gender', 'major_region')
#' fst_format_svydesign(svy_child, 'q7', 'fsd_id', add_cols = cols)
#'
#' svy_dev <- survey::svydesign(id = ~1, weights = ~paino, data = dev_coop)
#' fst_format_svydesign(svy_dev, 'q11_1', 'fsd_id', add_cols = 'gender, region')
#'
#' fst_format_svydesign(svy_dev, 'q11_2', 'fsd_id', 'finnish-ftb')
#' unlink("finnish-ftb-ud-2.5-191206.udpipe")
#' unlink("finnish-tdt-ud-2.5-191206.udpipe")
#' }
fst_format_svydesign <- function(svydesign,
                                 question,
                                 id,
                                 model = "ftb",
                                 use_weights = TRUE,
                                 add_cols = NULL
                                 ) {
  data <- svydesign$variables %>%
    dplyr::mutate(new_col = trimws(.data[[question]])) %>%
    dplyr::mutate_if(is.character, dplyr::na_if, "")
  if (model == "ftb") {
    if (!file.exists("finnish-ftb-ud-2.5-191206.udpipe")) {
      udpipe::udpipe_download_model(language = "finnish-ftb")
    }
    model_ftb <- udpipe::udpipe_load_model(
      file = "finnish-ftb-ud-2.5-191206.udpipe"
    )
    annotated_data <- as.data.frame(
      udpipe::udpipe_annotate(model_ftb, x = data$new_col, doc_id = data[[id]])
    )
  } else if (model == "tdt") {
    if (!file.exists("finnish-tdt-ud-2.5-191206.udpipe")) {
      udpipe::udpipe_download_model(language = "finnish-tdt")
    }
    model_tdt <- udpipe::udpipe_load_model(
      file = "finnish-tdt-ud-2.5-191206.udpipe"
    )
    annotated_data <- as.data.frame(
      udpipe::udpipe_annotate(model_tdt, x = data$new_col, doc_id = data[[id]])
    )
  } else {
    name2 <- paste0(model, '-ud-2.5-191206.udpipe')
    if (!file.exists(name2)) {
      udpipe::udpipe_download_model(language = model)
    }
    model_2 <- udpipe::udpipe_load_model(
      file = name2
    )
    annotated_data <- as.data.frame(
      udpipe::udpipe_annotate(model_2, x = data$new_col, doc_id = data[[id]])
    )
  }
  annotated_data <- annotated_data %>%
    dplyr::mutate(token = tolower(token)) %>%
    dplyr::mutate(lemma = tolower(lemma))
  if (use_weights) {
    weight_data <- svydesign$allprob
    colnames(weight_data) <- c("weight")
    weight_data['weight'] = 1/weight_data['weight']
    data2 <- data %>%
      dplyr::select(all_of(id))
    weight_data2 <- dplyr::bind_cols(data2, weight_data)
    annotated_data <- merge(x = annotated_data,
                            y = weight_data2,
                            by.x = 'doc_id',
                            by.y = id
    )
  }
  if (!is.null(add_cols)) {
    if (length(add_cols) == 1) {
      add_cols <- add_cols %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    }
    new_cols <- c(id, add_cols)
    add_data <- subset(data, select= new_cols)
    annotated_data <- merge(x = annotated_data,
                            y = add_data,
                            by.x = 'doc_id',
                            by.y = id
    )
  }
  annotated_data
}

#' Read In and format survey text responses from `svydesign` object
#'
#' Creates a dataframe in CoNLL-U format from a `svydesign` object including
#' text using the [udpipe] package and a language model plus
#' weights if these are included in the `svydesign` object and any columns added
#' through `add_cols`.Stopwords and punctuation are optionally removed if the
#' the `stopword_list` argument is not "none".
#'
#' `fst_prepare_svydesign()` produces a dataframe containing survey text
#'  responses in CoNLL-U format with stopwords optionally removed.
#' @param svydesign A `svydesign` object which contains an open-ended question.
#' @param question The column in the dataframe which contains the open-ended
#'  question.
#' @param id The column in the dataframe which contains the ids for the
#'  responses.
#' @param model A language model available for [udpipe]. `"ftb"`
#'  (default) or `"tdt"` are recognised as shorthand for "finnish-ftb" and
#'  "finnish-tdt". The full list is available in the [udpipe] documentation or
#'  via `fst_print_available_models()`.
#' @param stopword_list A valid stopword list, default is `"nltk"`, or
#'  `"none"`.
#' @param language two-letter ISO code for the language for the stopword list
#' @param use_weights Optional, whether to use weights within the `svydesign`
#' @param add_cols Optional, a column (or columns) from the dataframe which
#'  contain other information you'd like to retain (for instance, dimension
#'  columnns for splitting the data for comparison plots).
#' @param manual An optional boolean to indicate that a manual list will be
#'  provided, `stopword_list = "manual"` can also or instead be used.
#' @param manual_list A manual list of stopwords.
#'
#' @return A dataframe of text in CoNLL-U format.
#' @export
#'
#' @examples
#' \dontrun{
#' i <- "fsd_id"
#' svy_child <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' fst_prepare_svydesign(svy_child, question = "q7", id = i, use_weights = TRUE)
#'
#' svy_d <- survey::svydesign(id = ~1, weights = ~paino, data =dev_coop)
#' fst_prepare_svydesign(svy_d, question = "q11_2", id = i, add_cols = 'gender')
#'
#' fst_prepare_svydesign(svy_d, 'q11_2', i, 'finnish-ftb', 'nltk', 'fi')
#' unlink("finnish-ftb-ud-2.5-191206.udpipe")
#' unlink("finnish-tdt-ud-2.5-191206.udpipe")
#' }
fst_prepare_svydesign <- function(svydesign,
                                  question,
                                  id,
                                  model = "ftb",
                                  stopword_list = "nltk",
                                  language = 'fi',
                                  use_weights = TRUE,
                                  add_cols = NULL,
                                  manual = FALSE,
                                  manual_list = "") {
  an_data <- fst_format_svydesign(svydesign = svydesign,
                                  question = question,
                                  id = id,
                                  model = model,
                                  use_weights = use_weights,
                                  add_cols = add_cols)
  if (stopword_list != "none") {
    an_data <- fst_rm_stop_punct(data = an_data,
                                 stopword_list = stopword_list,
                                 language = language,
                                 manual = manual,
                                 manual_list = manual_list)
  }
  an_data
}
