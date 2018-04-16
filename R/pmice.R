#' Impute missing data in parallel
#' @param
#' @export
#' @import mice furrr
#' @example
#' \dontrun{
#' print("TODO")
#' }
pmice <- function(data, m = 5, method = vector("character", length = ncol(data)),
                  predictorMatrix = (1 - diag(1, ncol(data))), where = is.na(data),
                  visitSequence = NULL, form = vector("character", length = ncol(data)),
                  post = vector("character", length = ncol(data)),
                  defaultMethod = c("pmm", "logreg", "polyreg", "polr"), maxit = 5, diagnostics = TRUE,
                  printFlag = FALSE, seed = NA, imputationMethod = NULL, defaultImputationMethod = NULL,
                  data.init = NULL, ...)
    {
    imputations <- furrr::future_map(rep(1, m), ~mice::mice(
        data, m = ., method, predictorMatrix, where, visitSequence, form, post,
        defaultMethod, maxit, diagnostics, printFlag, seed, imputationMethod,
        defaultImputationMethod, data.init, ...
        ))

    purrr::reduce(imputations, mice::ibind)

    }

