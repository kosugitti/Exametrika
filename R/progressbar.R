#' @title progress-bar function
#' @param pos position
#' @param len total length
#' @param msg show message
#' @description
#' A progress bar to display the calculation process.
#'
show.progress <- function(pos, len, msg) {
  prg <- round(pos / len * 100)
  done <- paste(rep("#", round(prg / 2.5)), collapse = "")
  remain <- paste(rep("-", 40 - round(prg / 2.5)), collapse = "")
  prg.bar <- paste("|", done, remain, "|  ", as.character(prg), "%. ", msg, sep = "")
  message("\r", prg.bar, appendLF = FALSE)
}
