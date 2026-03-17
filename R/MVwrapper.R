#' Multivariate Imputation Error Benchmarking
#'
#' A multivariate wrapper around \code{\link{impute_errors}} that benchmarks
#' imputation methods across multiple time series simultaneously. Each column
#' of the input matrix or data frame is treated as an independent univariate
#' time series, and \code{\link{impute_errors}} is applied to each one
#' separately. Results are returned as a named list, one element per variable.
#'
#' @param multi_dataIn A numeric matrix or data frame where rows represent
#'   time points and columns represent variables (time series). Must have at
#'   least one column. Column names are used to label the output; if absent,
#'   columns are named \code{Series_1}, \code{Series_2}, etc.
#' @param smps Character string specifying the missing data mechanism.
#'   One of \code{"mcar"} (Missing Completely at Random) or \code{"mar"}
#'   (Missing at Random). Default is \code{"mcar"}.
#' @param methods Character vector of imputation method names to benchmark.
#'   Each name must be a function available in the current environment or
#'   specified via \code{methodPath}. Default is
#'   \code{c("na.approx", "na.interp", "na_interpolation", "na.locf", "na_mean")}.
#' @param methodPath Character string giving the file path to a script that
#'   defines custom imputation methods. Default is \code{NULL} (no custom
#'   methods loaded).
#' @param errorParameter Character string specifying the error metric to use.
#'   One of \code{"rmse"} (Root Mean Square Error) or \code{"mae"}
#'   (Mean Absolute Error). Default is \code{"rmse"}.
#' @param errorPath Character string giving the file path to a script that
#'   defines a custom error function. Default is \code{NULL}.
#' @param blck Numeric value specifying the block size for block missingness
#'   when \code{smps = "mar"}. Default is \code{50}.
#' @param blckper Logical. If \code{TRUE}, \code{blck} is interpreted as a
#'   percentage of the series length. If \code{FALSE}, it is interpreted as
#'   an absolute number of observations. Default is \code{TRUE}.
#' @param missPercentFrom Numeric. The starting missing data percentage for
#'   the benchmarking sequence. Default is \code{10}.
#' @param missPercentTo Numeric. The ending missing data percentage for the
#'   benchmarking sequence. Default is \code{90}.
#' @param interval Numeric. The step size between missing data percentages.
#'   Default is \code{10}.
#' @param repetition Integer. The number of repetitions for each missing data
#'   percentage and method combination. Higher values give more stable
#'   estimates. Default is \code{10}.
#' @param addl_arg A named list of additional arguments passed to the
#'   imputation methods. Default is \code{NULL}.
#'
#' @return A named list of class \code{c("mv_impute_errors", "list")} with one
#'   element per column of \code{multi_dataIn}. Each element is an
#'   \code{errprof} object as returned by \code{\link{impute_errors}},
#'   containing:
#'   \describe{
#'     \item{\code{Parameter}}{The error metric used (e.g., \code{"rmse"}).}
#'     \item{\code{MissingPercent}}{Numeric vector of missing data percentages
#'       evaluated.}
#'     \item{method name(s)}{Numeric vector of average errors at each missing
#'       percentage for each method.}
#'   }
#'   List elements are named using the column names of \code{multi_dataIn}.
#'
#' @details
#' This function is a convenience wrapper that extends \code{\link{impute_errors}}
#' to multivariate time series by applying it independently to each column.
#' Imputation methods do not share information across columns — each variable
#' is benchmarked in isolation. This approach preserves full compatibility with
#' all existing \code{impute_errors} arguments and method definitions.
#'
#' For datasets where cross-variable relationships are important (e.g., methods
#' such as MICE or Amelia that exploit inter-variable correlations), a deeper
#' multivariate integration would be required beyond this wrapper.
#'
#' @seealso \code{\link{impute_errors}}, \code{\link{plot_errors}}
#'
#' @examples
#' \donttest{
#' # Use the EuStockMarkets dataset (4 stock indices, 1860 observations)
#' data("EuStockMarkets")
#' stock_matrix <- as.matrix(EuStockMarkets)
#'
#' # Benchmark default imputation methods on all four indices
#' results <- mv_impute_errors(
#'   multi_dataIn    = stock_matrix,
#'   missPercentFrom = 10,
#'   missPercentTo   = 50,
#'   interval        = 10,
#'   repetition      = 5
#' )
#'
#' # Inspect results for the first variable (DAX)
#' print(results[["DAX"]])
#'
#' # Plot errors for each variable
#' for (nm in names(results)) {
#'   plot_errors(results[[nm]], plotType = "line")
#' }
#' }
#'
#' @export




mv_impute_errors<- function(multi_dataIn, smps = 'mcar', methods = c("na.approx", "na.interp", "na_interpolation", "na.locf", "na_mean"),  methodPath = NULL, errorParameter = 'rmse', errorPath = NULL, blck = 50, blckper = TRUE, missPercentFrom = 10, missPercentTo = 90, interval = 10, repetition = 10, addl_arg = NULL){


  # Input validation
  if (!is.matrix(multi_dataIn) && !is.data.frame(multi_dataIn)) {
    stop("multi_dataIn must be a matrix or data frame")
  }

  # Convert to matrix for uniform handling
  if (is.data.frame(multi_dataIn)) {
    multi_dataIn <- as.matrix(multi_dataIn)
  }

  n_series <- ncol(multi_dataIn)
  if (n_series < 1) {
    stop("multi_dataIn must have at least one column")
  }

  # Get or create column names
  cnames <- colnames(multi_dataIn)
  if (is.null(cnames)) {
    cnames <- paste0("Series_", seq_len(n_series))
  }

  # Loop over columns and apply impute_errors
  results <- lapply(seq_len(n_series), function(i) {
    series_ts <- stats::ts(multi_dataIn[, i])
    impute_errors(
      dataIn = series_ts,
      smps = smps,
      methods = methods,
      methodPath = methodPath,
      errorParameter = errorParameter,
      errorPath = errorPath,
      blck = blck,
      blckper = blckper,
      missPercentFrom = missPercentFrom,
      missPercentTo = missPercentTo,
      interval = interval,
      repetition = repetition,
      addl_arg = addl_arg
    )
  })


  # Name the list elements
  names(results) <- cnames

  # Assign a class for potential S3 methods (optional)
  class(results) <- c("mv_impute_errors", "list")

  return(results)
}



