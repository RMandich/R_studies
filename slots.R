get_symbols <- function() {
  #Creates a list with possible symbols with different probabilities
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(wheel, size = 3, replace = TRUE, 
         prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function(symbols) {
  #Case Creation
  same <- symbols[1] == symbols [2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  
  #Case Identification and prize attribution
  if (same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
  } else if ( all(bars)) {
    prize <- 5
  } else {
    cherries <- sum(symbols == 'C')
    prize <- c(0, 2, 5)[cherries + 1]
  }
  
  #Accounting for Diamonds
  diamonds <- sum(symbols == 'DD')
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
