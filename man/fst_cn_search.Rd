% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03_concept_network.R
\name{fst_cn_search}
\alias{fst_cn_search}
\title{Concept Network - Search textrank for concepts}
\usage{
fst_cn_search(data, concepts, pos_filter = NULL)
}
\arguments{
\item{data}{A dataframe of text in CoNLL-U format.}

\item{concepts}{String of terms to search for, separated by commas.}

\item{pos_filter}{List of UPOS tags for inclusion, default is `NULL` to
include all UPOS tags.}
}
\value{
Dataframe of n-grams containing searched terms.
}
\description{
This function takes a string of terms (separated by commas) or a single term
and, using `textrank_keywords()` from `textrank` package, filters data based
on `pos_filter` and finds words connected to search terms.
}
\examples{
con <- "kiusata, lyöminen, lyödä, potkia"
pf <- c("NOUN", "VERB", "ADJ", "ADV")
fst_cn_search(conllu_cb_bullying_iso, concepts = con, pos_filter = pf)
fst_cn_search(conllu_cb_bullying_iso, concepts = con)
}
