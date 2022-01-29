# this function creates the template board
createBoard <- function() {
  plot(c(0,10),c(0,10), col='white',
       xlab='', ylab ='', axes=F)
  segments(0,0,10,0)
  segments(0,0,0,10)
  segments(10,0,10,10)
  segments(0,10,10,10)
  points(0:10,rep(0,11), pch='|')
  points(0:10,rep(10,11), pch='|')
  points(rep(0,11),0:10, pch='-', cex=2)
  points(rep(10,11),0:10, pch='-', cex=2)
  title('Fertig-opoly')
}

numberOfRolls <- function(n) {
  sum(sapply(1:n,diceRoll)) %% 40
}

positionOnBoard <- function(n) {
  position <- c(0,0)
  if (floor(n / 10) == 0) {
    position[1] <- (n - 0.5) %% 10
  } else if (floor(n / 10) == 1) {
    position[1] <- 10
    position[2] <- (n - 0.5) %% 10
  } else if (floor(n / 10) == 2) {
    position[2] <- 10
    position[1] <- 10 - ((n - 0.5) %% 10)
  } else {
    position[2] <- 10 - ((n - 0.5) %% 10)
  }
  return(position)
}

# this function rolls the dice
diceRoll <- function(...){
  return(sum(sample.int(6,2, replace=T)))
}

# test that dice roll matches computed probabilities
testDice <- function(n) {
  plot(table(sapply(1:n, diceRoll))/n)
  points(2:12,
         c(1/36,1/18,1/12,1/9,5/36,1/6,5/36,1/9,1/12,1/18,1/36),
         col='red')
}

simulateGame <- function(n, piece='red') {
  x <- 0
  trackMoves <- rep(0, n)
  for (i in 1:n) {
    x <- (x+ diceRoll()) %% 40
    trackMoves[i] <- x
    points(positionOnBoard(x)[1],
           positionOnBoard(x)[2], pch=19, col=piece)
  }
  return(trackMoves)
}

computeProbBySpace <- function(spaces, nMoves=50) {
  moves <- table(cumsum(sapply(1:nMoves,diceRoll)) %% 40)
  return(sum(moves[names(moves) %in% spaces]))
}

createBoard()
simulateGame(20,piece='#FF000050')
simulateGame(20,piece='#00FF0050')
simulateGame(20,piece='#0000FF50')

# compute likelihood of landing on reds in 50 moves
nGames <- 1000
nMoves <- 50
probRed <- rep(0, nGames)
spaces <- c(21,23,24)

for (i in 1:nGames) {
  probRed[i] <- computeProbBySpace(spaces, nMoves)
}
plot(table(probRed)/nGames)

mean(probRed) / nMoves
