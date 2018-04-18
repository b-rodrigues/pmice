#' Impute missing data in parallel
#' @param
#' @export
#' @import mice furrr
#' @example
#' \dontrun{
#' print("TODO")
#' }
pmice <- function(df, m = 5, method = vector("character", length = ncol(df)),
  predictorMatrix = (1 - diag(1, ncol(df))), where = is.na(df),
  visitSequence = NULL, form = vector("character", length = ncol(df)),
  post = vector("character", length = ncol(df)),
  defaultMethod = c("pmm", "logreg", "polyreg", "polr"), maxit = 5, diagnostics = TRUE,
  printFlag = FALSE, seed = FALSE, imputationMethod = NULL, defaultImputationMethod = NULL,
  data.init = NULL)
    {
      imputations <- furrr::future_map(
                              rep(1, m),
                              ~mice::mice(data = df, m = ., maxit = maxit, printFlag = printFlag,
                                          method = method, predictorMatrix = predictorMatrix,
                                          where = where, visitSequence = visitSequence, form = form,
                                          post = post, defaultMethod = defaultMethod, diagnostics = diagnostics,
                                          imputationMethod = imputationMethod,
                                          defaultImputationMethod = defaultImputationMethod,
                                          data.init = data.init),
        future.seed = seed, future.packages = "mice")

    purrr::reduce(imputations, mice::ibind)
    }
