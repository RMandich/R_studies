get_symbols <- function() {
  #Creates list with possible symbols and corresponding probabilities
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function(symbols) {
  
  #count the number of Diamonds and Cherries
  diamonds <- sum(symbols == "DD")
  cherries <- sum(symbols == "C")
  
  #Case Creation
  #slots receives the symbols different from DD
  slots <- symbols[symbols != "DD"] 
  #same receives TRUE if slots has only 1 unique value
  same <- length(unique(slots)) == 1
  #count the number of bars
  bars <- slots %in% c("B", "BB", "BBB")
  
  #Case Identification and prize attribution
  if (diamonds == 3){
    prize <- 100
  } else if (same) {
    payouts <- c("7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[slots[1]])
  } else if ( all(bars)) {
    prize <- 5
  } else if (cherries > 0) {
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
  }else{
    prize <- 0
  }
  
  #Double for each Diamond
    prize * 2 ^ diamonds
}

play <- function() {
  symbols <- get_symbols()
  structure(score(symbols), symbols = symbols, class = 'slots')
}


slot_display <- function(prize){
  
  # insert symbols attribute into a variable
  symbols <- attr(prize, "symbols")
  
  # collapse symbols into a single string
  symbols <- paste(symbols, collapse = " ")
  
  #Join symbols and prize together with a line break and $
  string <- paste(symbols, prize, sep = "\n$")
  
  #cat is similar to print, but without quotes
  cat(string)
}

print.slots <- function(x, ...) {
  slot_display(x)
}

class(one_play) <- "slots"
