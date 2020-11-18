#' Stratified RMST Object
#'
#' Defines the object class returned by \code{\link{StratRMST}}.
#'
#' @slot Stratified Stats per arm and stratum.
#' @slot Marginal Stats per arm, marginalized over strata. 
#' @slot Contrasts Contrasts of the marginal statistics.
#' @slot Weights Per-stratum weights.
#' @name stratRMST-class
#' @rdname stratRMST-class
#' @exportClass stratRMST

setClass(
  Class = "stratRMST",
  representation = representation(
   Stratified = "data.frame",
   Marginal = "data.frame",
   Contrasts = "data.frame",
   Weights = "data.frame"
  )
)

# -----------------------------------------------------------------------------
# Print Method
# -----------------------------------------------------------------------------

#' Print Method for Stratified RMST Object.
#'
#' Print method for objects of class \code{stratRMST}.
#'
#' @param x An object of class \code{stratRMST}.
#' @param ... Unused.
#' @export

print.stratRMST <- function (x, ...) {
  
  disp <- function(y) {
    if (is.numeric(y)) {
      out <- signif(y, digits = 3)
    } else {
      out <- y
    }
    return(out)
  }
  
  # Marginal.
  marg <- x@Marginal
  marg[, ] <- lapply(marg, disp)
  cat('Marginal Statistics:\n')
  show(marg)
  cat('\n\n')
  
  # Contrasts.
  contrast <- x@Contrasts
  contrast[, ] <- lapply(contrast, disp)
  cat('Contrasts:\n')
  show(contrast)
  cat('\n\n')

}

# -----------------------------------------------------------------------------
# Show Method
# -----------------------------------------------------------------------------

#' Show Method for Stratified RMST Object
#'
#' @param object An object of class \code{stratRMST}.
#' @rdname fit-method
#' @importFrom methods show

setMethod(
  f = "show",
  signature = c(object = "stratRMST"),
  definition = function (object) {print.stratRMST(x = object)}
)

