# -----------------------------------------------------------------------------------------------
# cutter function
# -----------------------------------------------------------------------------------------------

cutter <- function(x, lower = 0, upper, by = 10, sep = "-", above.char = "+") {
  
  labs <- c(paste(seq(lower, upper - by, by = by),
                  seq(lower + by - 1, upper - 1, by = by),
                  sep = sep),
            paste(upper, above.char, sep = ""))
  
  as.character(cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
                   right = FALSE, labels = labs))
}
