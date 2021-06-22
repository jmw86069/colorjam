
#' Rainbow categorical colors across multiple groups
#'
#' Rainbow categorical colors across multiple groups
#'
#' This function applies `colorjam::rainbowJam()` to multiple
#' vectors, such that the categorical colors are not duplicated,
#' and are assigned as well-spaced across the rainbow for
#' each group as possible.
#'
#' @family colorjam assignment
#'
#' @return `list` of categorical colors, length equal to the
#'    input `length(ns)`, with color vectors with lengths `ns`.
#'
#' @param ns `integer` vector of `n` values passed to
#'    `rainbowJam()`.
#' @param do_plot `logical` indicating whether to plot the result
#'    using `jamba::showColors()`.
#' @param ... additional arguments are passed to `rainbowJam()`.
#'
#' @examples
#' opar <- par("mfrow"=c(3, 1));
#' on.exit(par(opar));
#' ns <- c(A=8, B=3, C=4);
#' colorlist1 <- rainbowJamMulti(ns,
#'    do_plot=TRUE,
#'    main="rainbowJamMulti()");
#'
#' # basic assignment in order
#' colorset2 <- colorjam::rainbowJam(sum(ns));
#' colorlist2 <- split(colorset2, rep(seq_along(ns), ns))
#' jamba::showColors(colorlist2, main="sequential assignment")
#'
#' # re-assign the same colors
#' colorlist3 <- lapply(ns, colorjam::rainbowJam);
#' jamba::showColors(colorlist3, main="independent color assignment");
#' par(opar);
#'
#' @export
rainbowJamMulti <- function
(ns,
 do_plot=FALSE,
 ...)
{
   # generate a sorting offset
   noff <- (seq_along(ns) - 1) / 100;

   # create a sort order across each vector of ns
   nseqs <- lapply(seq_along(ns), function(n){
      nseq <- head(
         seq(from=0 + noff[n], to=360 + noff[n], length.out=ns[n] + 1),
         ns[n]);
      names(nseq) <- paste0("set", n, "_num", seq_len(ns[n]));
      nseq;
   });
   nseqs

   # obtain full set of rainbow categorical colors
   ncolors <- colorjam::rainbowJam(
      n=length(unlist(nseqs)),
      ...);
   names(ncolors) <- names(sort(unlist(nseqs)));

   nseqcolors <- lapply(nseqs, function(i){
      unname(ncolors[names(i)])
   });
   if (length(names(ns)) > 0) {
      names(nseqcolors) <- names(ns);
   }
   if (do_plot) {
      jamba::showColors(nseqcolors,
         ...);
   }
   return(nseqcolors);
}
