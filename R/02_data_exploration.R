#' Make Simple Summary Table
#'
#' Creates a summary table for the input CoNLL-U data which provides the total
#' number of words, the number of unique words, and the number of unique lemmas.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#'
#' @return A dataframe with summary information on word counts for the data.
#' @export
#'
#' @examples
#' fst_summarise_short(fst_child)
#' fst_summarise_short(fst_dev_coop)
fst_summarise_short <- function(data) {
  no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
  count <- dplyr::n_distinct(data$doc_id) - no_resp_count
  data %>%
    dplyr::summarize(
      Respondents = count,
      "Total Words" = dplyr::n(),
      "Unique Words" = length(unique(token)),
      "Unique Lemmas" = length(unique(lemma))
    )
}

#' Make Summary Table
#'
#' Creates a summary table for the input CoNLL-U data which provides the
#' response count and proportion, total number of words, the number of unique
#' words, and the number of unique lemmas.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param desc A string describing responses in table, default is `"All
#'  responses"`.
#'
#' @return A dataframe with summary information for the data including response
#'  rate and word counts.
#' @export
#'
#' @examples
#' fst_summarise(fst_child)
#' fst_summarise(fst_dev_coop, "Q11_3")
fst_summarise <- function(data, desc = "All responses") {
  no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
  count <- dplyr::n_distinct(data$doc_id) - no_resp_count
  df <- data %>%
    dplyr::summarize(
      "Description" = desc,
      "Respondents" = count,
      "No Response" = no_resp_count,
      "Proportion" = round(count /
        dplyr::n_distinct(doc_id), 2),
      "Total Words" = dplyr::n(),
      "Unique Words" = length(unique(token)),
      "Unique Lemmas" = length(unique(lemma))
    )
  df
}

#' Make POS Summary Table
#'
#' Creates a summary table for the input CoNLL-U data which counts the number of
#' words of each part-of-speech tag within the data.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#'
#' @return A dataframe with a count and proportion of each UPOS tag in the data
#'  and the full name of the tag.
#' @export
#'
#' @examples
#' fst_pos(fst_child)
#' fst_pos(fst_dev_coop)
fst_pos <- function(data) {
  data <- data %>%
    dplyr::filter(!is.na(sentence)) %>%
    dplyr::filter(sentence != "na") %>%
    dplyr::filter(sentence != "NA")
  denom <- nrow(data)
  pos_table <- data %>%
    dplyr::count(upos, sort = TRUE) %>%
    dplyr::rename(UPOS = upos)
  pos_lookup <- data.frame(
    "UPOS" = c(
      "ADJ", "ADP", "ADV", "AUX", "CCONJ", "DET",
      "INTJ", "NOUN", "NUM", "PART", "PRON",
      "PROPN", "PUNCT", "SCONJ", "SYM", "VERB",
      "X"
    ),
    "UPOS_Name" = c(
      " adjective", " adposition",
      " adverb", " auxiliary",
      " coordinating conjunction",
      " determiner", " interjection",
      " noun", " numeral", " particle",
      " pronoun", " proper noun",
      " punctuation",
      " subordinating conjunction",
      " symbol", " verb", " other"
    )
  )
  df <- merge(x = pos_lookup, y = pos_table, by = "UPOS", all.x = TRUE)
  df %>%
    dplyr::mutate(n = tidyr::replace_na(n, 0)) %>%
    dplyr::rename(Count = n) %>%
    dplyr::mutate(Proportion = round(Count / denom, 3))
}

#' Make Length Summary Table
#'
#' Creates a table summarising  distribution of the length of responses.
#'
#' @param data dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param desc An optional string describing responses in table, default is
#'  `"All responses"`.
#' @param incl_sentences Whether to include sentence data in table, default is
#'  `TRUE`.
#'
#' @return Table summarising distribution of lengths of responses.
#' @export
#'
#' @examples
#' fst_length_summary(fst_child, incl_sentences = FALSE)
#' fst_length_summary(fst_dev_coop, desc = "Q11_3")
fst_length_summary <- function(data,
                               desc = "All responses",
                               incl_sentences = TRUE) {
  no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
  count <- dplyr::n_distinct(data$doc_id) - no_resp_count
  data <- dplyr::select(data, doc_id, sentence) %>%
    dplyr::mutate(length = stringr::str_count(sentence, "\\w+")) %>%
    dplyr::filter(!is.na(sentence)) %>%
    dplyr::filter(sentence != "na") %>%
    dplyr::filter(sentence != "NA")
  data <- data[!duplicated(data), ] %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      number_sentences = dplyr::n(),
      number_of_words = sum(length)
    )
  word_df <- data %>%
    dplyr::summarize(
      "Description" = paste0(desc, "- Words"),
      "Respondents" = count,
      "Mean" = round(mean(data$number_of_words), 3),
      "Minimum" = min(data$number_of_words),
      "Q1" = quantile(data$number_of_words, 0.25),
      "Median" = median(data$number_of_words),
      "Q3" = quantile(data$number_of_words, 0.75),
      "Maximum" = max(data$number_of_words)
    )
  if (incl_sentences == TRUE) {
    sentence_df <- data %>%
      dplyr::summarize(
        "Description" = paste0(desc, "- Sentences"),
        "Respondents" = count,
        "Mean" = round(mean(data$number_sentences), 3),
        "Minimum" = min(data$number_sentences),
        "Q1" = quantile(data$number_sentences, 0.25),
        "Median" = median(data$number_sentences),
        "Q3" = quantile(data$number_sentences, 0.75),
        "Maximum" = max(data$number_sentences)
      )
    word_df <- rbind(word_df, sentence_df)
  }
  word_df
}

#' Add `svydesign` weights to CoNLL-U data
#'
#' This function takes data in CoNLL-U format and a `svydesign` (from `survey`
#' package) object with weights in it and merges the weights, and any additional
#' columns into the formatted data.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param svydesign A `svydesign` object containing the raw data which produced
#'  the `data`
#' @param id ID column from raw data, must match the `docid` in formatted `data`
#' @param add_cols Optional, a column (or columns) from the dataframe which
#'  contain other information you'd need (for instance, covariate column for
#'  splitting the data for comparison plots).
#' @param add_weights Optional, a boolean for whether to add weights from
#'  svydesign object, default is `TRUE`.
#'
#' @return A dataframe of text in CoNLL-U format plus a `'weight'` column and
#'  optional other columns
#' @export
#'
#' @examples
#' svy_child <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' fst_use_svydesign(data = fst_child_2, svydesign = svy_child, id = 'fsd_id')
#'
#' svy_dev <- survey::svydesign(id = ~1, weights = ~paino, data = dev_coop)
#' fst_use_svydesign(data = fst_dev_coop_2, svydesign = svy_dev, id = 'fsd_id')
fst_use_svydesign <- function(data,
                              svydesign,
                              id,
                              add_cols = NULL,
                              add_weights = TRUE) {
  if (length(add_cols) == 1) {
    add_cols <- add_cols %>%
      stringr::str_extract_all(pattern = "\\w+") %>%
      unlist()
  }
  if (add_weights == TRUE) {
    weight_data <- svydesign$allprob
    colnames(weight_data) <- c("weight")
    weight_data['weight'] = 1/weight_data['weight']
  } else {
    weight_data <- NULL
  }
  data2 <- svydesign$variables %>%
    dplyr::select(all_of(c(id, add_cols)))
  weight_data2 <- dplyr::bind_cols(data2, weight_data)
  annotated_data <- merge(x = data,
                          y = weight_data2,
                          by.x = 'doc_id',
                          by.y = id
  )
}

#' Make Top Words Table
#'
#' Creates a table of the most frequently-occurring words (unigrams) within the
#' data. Optionally, weights can be provided either through a `weight` column in
#' the formatted data, or from a `svydesign` object with the raw (preformatted)
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param number The number of top words to return, default is `10`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses), `"number_resp"`
#'  (the number of r , or `NULL` (raw count returned, default, also used
#'  when weights are applied).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param use_svydesign_weights Option to weight words in the table using
#'  weights from  a `svydesign` containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A `svydesign` which contains the raw data and weights,
#'  required if `use_svydesign_weights = TRUE`.
#' @param use_column_weights Option to weight words in the table using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`
#'
#' @return A table of the most frequently occurring words in the data.
#' @export
#'
#' @examples
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' pf2 <- "NOUN, VERB, ADJ, ADV"
#' fst_freq_table(fst_child, number = 15, strict = FALSE, pos_filter = pf)
#' fst_freq_table(fst_child, number = 15, strict = FALSE, pos_filter = pf2)
#' fst_freq_table(fst_child, norm = 'number_words')
#' fst_freq_table(fst_child, use_column_weights = TRUE)
#' c2 <- fst_child_2
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' i <- 'fsd_id'
#' fst_freq_table(c2, use_svydesign_weights = TRUE, svydesign = s, id = i)
fst_freq_table <- function(data,
                           number = 10,
                           norm = NULL,
                           pos_filter = NULL,
                           strict = TRUE,
                           use_svydesign_weights = FALSE,
                           id = "",
                           svydesign = NULL,
                           use_column_weights = FALSE) {
  if (use_svydesign_weights == TRUE) {
    data <- fst_use_svydesign(data = data,
                              svydesign = svydesign,
                              id = id,
                              add_cols = NULL)
  }
  with_ties <- !strict
  if (strict == TRUE) {
    message("Note:\n Words with equal occurrence are presented in alphabetical order. \n By default, words are presented in order to the `number` cutoff word. \n This means that equally-occurring later-alphabetically words beyond the cutoff word will not be displayed.\n\n")
  } else {
    message("Note:\n Words with equal occurrence are presented in alphabetical order. \n With `strict` = FALSE, words occurring equally often as the `number` cutoff word will be displayed. \n\n")
  }

  if (is.null(norm)) {
    denom <- 1
  } else if (norm == "number_words") {
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  } else if (norm == "number_resp") {
    no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
    count <- dplyr::n_distinct(data$doc_id) - no_resp_count
    denom <- count
  } else {
    message("NOTE: A recognised normalisation method has not been provided. \n Function has defaulted to provide raw counts.")
    denom <- 1
  }
  if (!is.null(pos_filter)) {
    if (length(pos_filter) == 1) {
      pos_filter <- pos_filter %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    }
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na")
  if (use_svydesign_weights == TRUE) {
    data <- dplyr::count(data, lemma, sort = TRUE, wt = weight)
  } else if (use_column_weights == TRUE) {
    data <- dplyr::count(data, lemma, sort = TRUE, wt = weight)
  } else {
    data <- dplyr::count(data, lemma, sort = TRUE)
  }
  data %>%
    dplyr::mutate(n = round(n / denom, 3)) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
    dplyr::mutate(lemma = reorder(lemma, n)) %>%
    dplyr::rename(words = lemma, occurrence = n)
}

#' Make Top N-grams Table
#'
#' Creates a table of the most frequently-occurring n-grams within the
#' data. Optionally, weights can be provided either through a `weight` column
#' in the formatted data, or from a `svydesign` object with the raw
#' (preformatted) data.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses), `"number_resp"`
#'  (the number of responses), or `NULL` (raw count returned, default, also used
#'  when weights are applied).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param use_svydesign_weights Option to weight words in the table using
#'  weights from  a `svydesign` containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A `svydesign` which contains the raw data and weights,
#'  required if `use_svydesign_weights = TRUE`.
#' @param use_column_weights Option to weight words in the table using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`
#'
#' @return A table of the most frequently occurring n-grams in the data.
#' @export
#'
#' @examples
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' pf2 <- "NOUN, VERB, ADJ, ADV"
#' fst_ngrams_table(fst_child, norm = NULL)
#' fst_ngrams_table(fst_child, ngrams = 2, norm = "number_resp")
#' fst_ngrams_table(fst_child, ngrams = 2, pos_filter = pf)
#' fst_ngrams_table(fst_child, ngrams = 2, pos_filter = pf2)
#' c2 <- fst_child_2
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' i <- 'fsd_id'
#' fst_ngrams_table(c2, use_svydesign_weights = TRUE, svydesign = s, id = i)
#' fst_ngrams_table(fst_child, use_column_weights = TRUE, ngrams = 3)
fst_ngrams_table <- function(data,
                             number = 10,
                             ngrams = 1,
                             norm = NULL,
                             pos_filter = NULL,
                             strict = TRUE,
                             use_svydesign_weights = FALSE,
                             id = "",
                             svydesign = NULL,
                             use_column_weights = FALSE) {
  if (use_svydesign_weights == TRUE) {
    data <- fst_use_svydesign(data = data,
                              svydesign = svydesign,
                              id = id,
                              add_cols = NULL)
  }
  with_ties <- !strict
  if (strict == TRUE) {
    message("Note:\n N-grams with equal occurrence are presented in alphabetical order. \n By default, n-grams are presented in order to the `number` cutoff n-gram. \n This means that equally-occurring later-alphabetically n-grams beyond the cutoff n-gram will not be displayed. \n\n")
  } else {
    message("Note:\n N-grams with equal occurrence are presented in alphabetical order. \n With `strict` = FALSE, n-grams occurring equally often as the `number` cutoff n-gram will be displayed. \n\n")
  }
  if (is.null(norm)) {
    denom <- 1
  } else if (norm == "number_words") {
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  } else if (norm == "number_resp") {
    no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
    count <- dplyr::n_distinct(data$doc_id) - no_resp_count
    denom <- count
  } else if (norm == "use_weights") {
    denom <- 1
  } else {
    message("NOTE: A recognised normalisation method has not been provided. \n Function has defaulted to provide raw counts.")
    denom <- 1
  }
  if (!is.null(pos_filter)) {
    if (length(pos_filter) == 1) {
      pos_filter <- pos_filter %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    }
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na") %>%
    dplyr::mutate(words = udpipe::txt_nextgram(lemma, n = ngrams))
  if (use_svydesign_weights == TRUE) {
    data <- dplyr::count(data, words, sort = TRUE, wt = weight)
  } else if (use_column_weights == TRUE) {
    data <- dplyr::count(data, words, sort = TRUE, wt = weight)
  } else {
    data <- dplyr::count(data, words, sort = TRUE)
  }
  data %>%
    dplyr::mutate(n = round(n / denom, 3)) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
    dplyr::mutate(words = reorder(words, n)) %>%
    dplyr::filter(!is.na(words)) %>%
    dplyr::filter(words != "na") %>%
    dplyr::rename(occurrence = n)
}

#' Make Top N-grams Table 2
#'
#' Creates a table of the most frequently-occurring n-grams within the
#' data. Optionally, weights can be provided either through a `weight` column
#' in the formatted data, or from a `svydesign` object with the raw
#' (preformatted) data.
#' Equivalent to `fst_get_top_ngrams` but doesn't print message about ties.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param number The number of n-grams to return, default is `10`.
#' @param ngrams The type of n-grams to return, default is `1`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param use_svydesign_weights Option to weight words in the table using
#'  weights from a `svydesign` containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A `svydesign` which contains the raw data and weights,
#'  required if `use_svydesign_weights = TRUE`.
#' @param use_column_weights Option to weight words in the table using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`
#'
#' @return A table of the most frequently occurring n-grams in the data.
#' @export
#'
#' @examples
#' fst_ngrams_table2(fst_child, norm = NULL)
#' fst_ngrams_table2(fst_child, ngrams = 2, norm = "number_resp")
#' c <- fst_child_2
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' i <- 'fsd_id'
#' T <- TRUE
#' fst_ngrams_table2(c, 10, 2, use_svydesign_weights = T, svydesign = s, id = i)
fst_ngrams_table2 <- function(data,
                              number = 10,
                              ngrams = 1,
                              norm = NULL,
                              pos_filter = NULL,
                              strict = TRUE,
                              use_svydesign_weights = FALSE,
                              id = "",
                              svydesign = NULL,
                              use_column_weights = FALSE) {
  if (use_svydesign_weights == TRUE) {
    data <- fst_use_svydesign(data = data,
                              svydesign = svydesign,
                              id = id)
  }
  with_ties <- !strict
  if (strict == TRUE) {
  } else {
  }
  if (is.null(norm)) {
    denom <- 1
  } else if (norm == "number_words") {
    data %>%
      dplyr::filter(.data$dep_rel != "punct") %>%
      dplyr::filter(!is.na(lemma)) %>%
      dplyr::filter(lemma != "na")
    denom <- nrow(data)
  } else if (norm == "number_resp") {
    no_resp_count <- length(which(data$sentence %in% c("NA", "na")))
    count <- dplyr::n_distinct(data$doc_id) - no_resp_count
    denom <- count
  } else if (norm == "use_weights") {
    denom <- 1
  } else {
    message("NOTE: A recognised normalisation method has not been provided. \n Function has defaulted to provide raw counts.")
    denom <- 1
  }
  if (!is.null(pos_filter)) {
    if (length(pos_filter) == 1) {
      pos_filter <- pos_filter %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    }
    data <- dplyr::filter(data, .data$upos %in% pos_filter)
  }
  data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na") %>%
    dplyr::mutate(words = udpipe::txt_nextgram(lemma, n = ngrams))
  if (use_svydesign_weights == TRUE) {
    data <- dplyr::count(data, words, sort = TRUE, wt = weight)
  } else if (use_column_weights == TRUE) {
    data <- dplyr::count(data, words, sort = TRUE, wt = weight)
  } else {
    data <- dplyr::count(data, words, sort = TRUE)
  }
  data %>%
    dplyr::mutate(n = round(n / denom, 3)) %>%
    dplyr::slice_max(n, n = number, with_ties = with_ties) %>%
    dplyr::mutate(words = reorder(words, n)) %>%
    dplyr::filter(!is.na(words)) %>%
    dplyr::filter(words != "na") %>%
    dplyr::rename(occurrence = n)
}


#' Make Top Words plot
#'
#' Plots most common words.
#'
#' @param table Output of `fst_freq_table()` or `fst_ngrams_table()`.
#' @param number Optional number of n-grams for the title, default is `NULL`.
#' @param name An optional "name" for the plot to add to title, default is
#'  `NULL`.
#'
#' @return Plot of top words.
#' @export
#'
#' @examples
#' pf <- c("NOUN", "VERB", "ADJ", "ADV")
#' top_words <- fst_freq_table(fst_child, number = 15, pos_filter = pf)
#' fst_freq_plot(top_words, number = 15, name = "Bullying")
fst_freq_plot <- function(table, number = NULL, name = NULL) {
  table %>%
    ggplot2::ggplot(ggplot2::aes(occurrence, words)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::labs(
      y = NULL,
      title = paste(name, as.character(number), "Most Common Words")
    )
}



#' Make N-grams plot
#'
#' Plots frequency n-grams.
#'
#' @param table Output of `fst_get_top_words()` or `fst_get_top_ngrams()`.
#' @param number Optional number of n-grams for title, default is `NULL`.
#' @param ngrams The type of n-grams, default is `1`.
#' @param name An optional "name" for the plot to add to title, default is
#'  `NULL`.
#'
#' @return Plot of top n-grams.
#' @export
#'
#' @examples
#' top_bigrams <- fst_ngrams_table(fst_child, ngrams = 2, number = 15)
#' fst_ngrams_plot(top_bigrams, ngrams = 2, number = 15, name = "Children")
fst_ngrams_plot <- function(table, number = NULL, ngrams = 1, name = NULL) {
  if (ngrams == 1) {
    term <- "Words"
  } else if (ngrams == 2) {
    term <- "Bigrams"
  } else {
    term <- paste0(as.character(ngrams), "-grams")
  }
  table %>%
    ggplot2::ggplot(ggplot2::aes(occurrence, words)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = colours, guide = "none") +
    ggplot2::labs(
      y = NULL,
      title = paste(name, as.character(number), "Most Common", term)
    )
}


#' Find and Plot Top Words
#'
#' Creates a plot of the most frequently-occurring words (unigrams) within the
#' data. Optionally, weights can be provided either through a `weight` column
#' in the formatted data, or from a `svydesign` object with the raw
#' (preformatted) data.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param number The number of top words to return, default is `10`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param name An optional "name" for the plot to add to title, default is
#'  `NULL`.
#' @param use_svydesign_weights Option to weight words in the plot using
#'  weights from a `svydesign` containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A `svydesign` which contains the raw data and weights,
#'  required if `use_svydesign_weights = TRUE`.
#' @param use_column_weights Option to weight words in the plot using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`
#'
#' @return Plot of top words.
#' @export
#'
#' @examples
#' fst_freq(fst_child, number = 12, norm = 'number_resp',  name = "All")
#' fst_freq(fst_child, use_column_weights = TRUE)
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' i <- 'fsd_id'
#' fst_freq(fst_child_2, use_svydesign_weights = TRUE, svydesign = s, id = i)
fst_freq <- function(data,
                     number = 10,
                     norm = NULL,
                     pos_filter = NULL,
                     strict = TRUE,
                     name = NULL,
                     use_svydesign_weights = FALSE,
                     id = "",
                     svydesign = NULL,
                     use_column_weights = FALSE) {
  words <- fst_freq_table(
    data = data,
    number = number,
    norm = norm,
    pos_filter = pos_filter,
    strict = strict,
    use_svydesign_weights = use_svydesign_weights,
    id = id,
    svydesign = svydesign,
    use_column_weights = use_column_weights
  )
  fst_freq_plot(table = words, number = number, name = name)
}

#' Find and Plot Top N-grams
#'
#' Creates a plot of the most frequently-occurring n-grams within the
#' data. Optionally, weights can be provided either through a `weight` column
#' in the formatted data, or from a `svydesign` object with the raw
#' (preformatted) data.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param number The number of top words to return, default is `10`.
#' @param ngrams The type of n-grams, default is `1`.
#' @param norm The method for normalising the data. Valid settings are
#'  `"number_words"` (the number of words in the responses, default),
#'  `"number_resp"` (the number of responses), or `NULL` (raw count returned).
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param strict Whether to strictly cut-off at `number` (ties are
#'  alphabetically ordered), default is `TRUE`.
#' @param name An optional "name" for the plot to add to title, default is
#'  `NULL`.
#' @param use_svydesign_weights Option to weight words in the plot using
#'  weights from  a `svydesign` containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A `svydesign` which contains the raw data and weights,
#'  required if `use_svydesign_weights = TRUE`.
#' @param use_column_weights Option to weight words in the plot using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`
#'
#' @return Plot of top n-grams
#' @export
#'
#' @examples
#' fst_ngrams(fst_child, 12, ngrams = 2, strict = FALSE, name = "All")
#' c <- fst_child_2
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' i <- 'fsd_id'
#' T <- TRUE
#' fst_ngrams(c, ngrams = 3, use_svydesign_weights = T, svydesign = s, id = i)
fst_ngrams <- function(data,
                       number = 10,
                       ngrams = 1,
                       norm = NULL,
                       pos_filter = NULL,
                       strict = TRUE,
                       name = NULL,
                       use_svydesign_weights = FALSE,
                       id = "",
                       svydesign = NULL,
                       use_column_weights = FALSE) {
  ngram_list <- fst_ngrams_table(
    data = data,
    number = number,
    ngrams = ngrams,
    norm = norm,
    pos_filter = pos_filter,
    strict = strict,
    use_svydesign_weights = use_svydesign_weights,
    id = id,
    svydesign = svydesign,
    use_column_weights = use_column_weights
  )
  fst_ngrams_plot(
    table = ngram_list,
    number = number,
    ngrams = ngrams,
    name = name
  )
}

#' Make Wordcloud
#'
#' Creates a wordcloud from CoNLL-U data of frequently-occurring words.
#' Optionally, weights can be provided either through a `weight` column in the
#' formatted data, or from a `svydesign` object with the raw (preformatted)
#' data.
#'
#' @param data A dataframe of text in CoNLL-U format, with optional additional
#'  columns.
#' @param pos_filter List of UPOS tags for inclusion, default is `NULL` which
#'  means all word types included.
#' @param max The maximum number of words to display, default is `100`.
#' @param use_svydesign_weights Option to weight words in the wordcloud using
#'  weights from a `svydesign` containing the raw data, default is `FALSE`
#' @param id ID column from raw data, required if `use_svydesign_weights = TRUE`
#'  and must match the `docid` in formatted `data`.
#' @param svydesign A `svydesign` which contains the raw data and weights,
#'  required if `use_svydesign_weights = TRUE`.
#' @param use_column_weights Option to weight words in the wordcloud using
#'  weights from  formatted data which includes addition `weight` column,
#'  default is `FALSE`.
#'
#' @return A wordcloud from the data.
#' @export
#'
#' @examples
#' fst_wordcloud(fst_child)
#' fst_wordcloud(fst_child, pos_filter = c("NOUN", "VERB", "ADJ", "ADV"))
#' fst_wordcloud(fst_child, pos_filter = 'NOUN, VERB, ADJ')
#' fst_wordcloud(fst_child, use_column_weights = TRUE)
#' i <- 'fsd_id'
#' c <- fst_child_2
#' s <- survey::svydesign(id=~1, weights= ~paino, data = child)
#' fst_wordcloud(c, use_svydesign_weights = TRUE, id = i, svydesign = s)
fst_wordcloud <- function(data,
                          pos_filter = NULL,
                          max = 100,
                          use_svydesign_weights = FALSE,
                          id = "",
                          svydesign = NULL,
                          use_column_weights = FALSE) {
  if (use_svydesign_weights == TRUE) {
    data <- fst_use_svydesign(data = data, svydesign = svydesign, id = id)
  }
  if (!is.null(pos_filter)) {
    if (length(pos_filter) == 1) {
      pos_filter <- pos_filter %>%
        stringr::str_extract_all(pattern = "\\w+") %>%
        unlist()
    }
    data <- dplyr::filter(data, upos %in% pos_filter)
  }
  wordcloud_data <- data %>%
    dplyr::filter(.data$dep_rel != "punct") %>%
    dplyr::filter(!is.na(lemma)) %>%
    dplyr::filter(lemma != "na")
  if (use_svydesign_weights == TRUE) {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE, wt = weight)
  } else if (use_column_weights == TRUE) {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE, wt = weight)
  } else {
    wordcloud_data <- dplyr::count(wordcloud_data, lemma, sort = TRUE)
  }
  scale_x <- 2
  if (max <= 50) {
    scale_x <- 4
  } else if (max <= 100) {
    scale_x <- 3
  } else {
    scale_x <- 2
  }
  wordcloud::wordcloud(
    words = wordcloud_data$lemma,
    freq = wordcloud_data$n,
    max.words = max,
    random.order = FALSE,
    rot.per = 0.35,
    colors = RColorBrewer::brewer.pal(8, "Dark2")
  )
}
