% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{fst_dev_coop}
\alias{fst_dev_coop}
\title{Young People's Views on Development Cooperation 2012 q11_3 response data in
CoNLL-U format with NTLK stopwords removed and background variables.}
\format{
## `fst_dev_coop`
A dataframe with 4192 rows and 19 columns:
\describe{
  \item{doc_id}{the identifier of the document}
  \item{paragraph_id}{the identifier of the paragraph}
  \item{sentence_id}{the identifier of the sentence}
  \item{sentence}{the text of the sentence for which this token is part of}
  \item{token_id}{Word index, integer starting at 1 for each new sentence; may be a range for multi-word tokens; may be a decimal number for empty nodes.}
  \item{token}{Word form or punctuation symbol.}
  \item{lemma}{Lemma or stem of word form.}
  \item{upos}{Universal part-of-speech tag.}
  \item{xpos}{Language-specific part-of-speech tag; underscore if not available.}
  \item{feats}{List of morphological features from the universal feature inventory or from a defined language-specific extension; underscore if not available.}
  \item{head_token_id}{Head of the current word, which is either a value of token_id or zero (0).}
  \item{dep_rel}{Universal dependency relation to the HEAD (root iff HEAD = 0) or a defined language-specific subtype of one.}
  \item{deps}{Enhanced dependency graph in the form of a list of head-deprel pairs.}
  \item{misc}{Any other annotation.}
  \item{weight}{Weight}
  \item{gender}{Gender}
  \item{year_of_birth}{Year of Birth}
  \item{region}{Region of Residence}
}
}
\source{
<https://urn.fi/urn:nbn:fi:fsd:T-FSD2821>
}
\usage{
fst_dev_coop
}
\description{
This data contains the responses to Development Cooperation q11_3 dataset
in CoNLL-U format with NLTK stopwords and punctuation removed plus weights
and background variables.
}
\keyword{datasets}
