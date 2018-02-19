# Created 2018-02-19 11_49_02 CEST
#

#' @import data.table
#' @import purrr
#' @import chR
NULL

makeNames <- function(dfm) chDT({
  chDT(dfm)
  colNames <- dfm %>% colnames
  setnames(dfm, colNames, make.names(colNames))
  dfm
})

norm2props <- function(dfm, props) chDT({
  chDT(dfm)
  chMaybe(chStrings, props)

  if (is.null(props)) dfm
  else {
    colNames <- colnames(dfm)
    # 1. All missing must be added with 0s
    props2Add <- setdiff(props, colNames)
    for (p in props2Add) dfm[, (p) := 0]

    # 2. All not in props must be removed
    props2rm <- setdiff(colNames, props)
    for (p in props2rm) set(dfm, j = p, value = NULL)

    setcolorder(dfm, props)
    dfm
  }
})

str2dfm <- function(s, props = NULL) {
  chStrings(s)

  s %>%
    quanteda::tokens(
      what = "word",
      remove_numbers = TRUE,
      remove_punct   = TRUE,
      remove_symbols = TRUE,
      remove_hyphens = TRUE)             %>%
    quanteda::tokens_tolower()           %>%
    quanteda::tokens_select(
      x = .,
      pattern   = quanteda::stopwords(),
      selection = "remove")              %>%
    quanteda::tokens_wordstem(
      x = ., language = "english")       %>%
    quanteda::dfm(tolower = FALSE)       %>%
    as.data.table                        %>%
    makeNames                            %>%
    norm2props(props)
}

smsHamSpamDT <- function(s, model, props) chDT({
  chStrings(s)
  dfm  <- str2dfm(s, props = props)
  pred <- predict(model, newdata = dfm) %>% as.data.table
  pred[, Label := ifelse(ham > spam, "ham", "spam")]
  pred
})

# preds1 <- smsSpam[, v1] %>% smsHamSpamDT(model, props)
