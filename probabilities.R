#Calculating probabilities on the weighted dice

rolls <- expand.grid(die,die)
rolls$value <- rolls$Var1 + rolls$Var2

prob <- c("1" = 1/8, "2" = 1/8, "3" = 1/8,
          "4" = 1/8, "5" = 1/8, "6" = 3/8)

rolls$prob1 <- prob[rolls$Var1]
rolls$prob2 <- prob[rolls$Var2]
rolls$prob <- rolls$prob1 * rolls$prob2


##########

#calculating probabilities on the slot machine

#creates a vector with all the possible symbols
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")

#expands the symbols to all possible combinations of 3
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)

#lookup table for the slots probabilities
prob_slots <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, 
                "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52)

#creates the probabilites collumns for each symbol and combo
combos$prob1 <- prob_slots[combos$Var1]
combos$prob2 <- prob_slots[combos$Var2]
combos$prob3 <- prob_slots[combos$Var3]

combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

#calculating the prize for each combo

combos$prize <- NA

for (i in 1:nrow(combos)) {
  symbols <- c(combos[i,1], combos[i,2], combos[i,3])
  combos$prize[i] <- score(symbols)
}
