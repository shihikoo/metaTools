#' CalculateAUC
#'
#' Calculate AUC
#'
#' @param y
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' x <- c(1,3,7,14)
#' y <- c(6.3,5.6,4.5,2.6)
#' ysd <- c(7,8,5,3)
#'
#' CalculateAUC(y,x)
#'
CalculateAUC <- function(y, x){
  dx <- c(diff(x), 0)
  dy <- c(diff(y), 0)
  areas <- y * dx + dy * dx/2
  auc <- sum(areas)
  return(sum(y * dx) + sum(dy * dx)/2)
}

#' CalculateAUCSD
#'
#' Calculate AUC SD
#'
#' @param y
#' @param x
#' @param ysd
#'
#' @return
#' @export
#'
#' @examples
#' x <- c(1,3,7,14)
#' y <- c(6.3,5.6,4.5,2.6)
#' ysd <- c(7,8,5,3)
#'
#' CalculateAUC(y,x,ysd)
#'
CalculateAUCSD <- function(y, x,ysd=NULL){
  dx <- c(diff(x), 0)
  dy <- c(diff(y), 0)
  areas <- y * dx + dy * dx/2
  areasSD <- sd(areas)

  if(!is.null(ysd)) sd <- sqrt(areasSD^2 + sum(((ysd*2 +c(diff(ysd),0))/2*dx)^2))

  return(sd)
}
