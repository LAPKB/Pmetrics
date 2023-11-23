#' @title Print NPC
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Print a Pmetrics NPC (Numerical Predictive Check) object, made by [plot.PM_sim].
#'
#' @method print PMnpc
#' @param x A PMnpc object made by [plot.PM_sim].
#' @param \dots Other parameters which are not necessary.
#' @return A printed object.
#' @author Michael Neely
#' @seealso [plot.PM_sim]
#' @export

print.PMnpc <- function(x, ...) {
  cat("\n")
  print(x$npc)
  cat(paste("\nProportion of observations between 5th and 95th simulated percentiles: ", round(attr(x$npc, "05-95"), 3), ", P=", round(attr(x$npc, "P-90"), 3), sep = ""))
}
