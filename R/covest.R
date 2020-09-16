
#' HAC estimator
#'
#' @param ids values
#' @param ...
#'
#' @return list of estimation result cov and name (for saving purpose)
#' @export
HAC <- function(ids,...)
{
  if(is.matrix(ids))
  {
    n <- nrow(ids)
  } else
  {
    n <- length(ids)
  }
  list(cov= n*sandwich::vcovHAC(lm(ids~1),...), name = "HAC")
}


#' iid variance estimator
#'
#' @param ids values
#' @param ...
#'
#' @return list of estimation result cov and name (for saving purpose)
#' @export
iid <- function(ids,...)
{
  list(cov= var(ids), name = "iid")
}


#' HC estimator
#'
#' @param ids values
#' @param ...
#'
#' @return list of estimation result cov and name (for saving purpose)
#' @export
HC <- function(ids)
{
  if(is.matrix(ids))
  {
    n <- nrow(ids)
  } else
  {
    n <- length(ids)
  }
  res <- n*sandwich::vcovHC(lm(ids~1))
  list(cov = res, name = "HC")
}



#' @export
error.var <- function()
{
  message("Coriance estimation failed")
  return(list(cov=NA, name=NA))
}
