
.onLoad <- function(...) {
   # set global options
   op <- options();

   op.colorjam <- list(
      colorjam.preset="dichromat2",
      colorjam.step="default")

   # set options not already defined
   toset <- !(names(op.colorjam) %in% names(op))
   if (any(toset)) {
      options(op.colorjam[toset])
   }
   invisible()
}
