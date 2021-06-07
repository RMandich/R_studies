setup <- function(deck) {
  DECK <- deck
  
  DEAL <- function() {
    card <- deck[1, ]
    assign("deck", deck[-1, ], envir = parent.env(environment()))
    card
  }
  
  SHUFFLE <- function(){
    random <- sample(1:52, size = 52)
    assign("deck", DECK[random, ], envir = parent.env(environment()))
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
deal <- cards$deal
shuffle <- cards$shuffle




deck3 <- deck

deck3$value [c(13, 26, 39, 52)] <- c(14,14,14,14)

deck4 <- deck
deck4$value <- 0
head(deck4)

deck4$value[deck4$suit == 'hearts'] <- 1
deck4

deck4$value[deck4$face == 'queen' & deck4$suit == 'spades'] <- 13
deck4


deck5 <- deck
facecard <- deck5$face %in% c('king', 'queen', 'jack')
deck5$value[facecard] <- 10
deck5 

deck5$value[deck5$face == 'ace'] <- NA
deck5
