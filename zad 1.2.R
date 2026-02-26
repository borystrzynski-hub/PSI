przyznaj <- function() {
  rzut <- sample(1:6, size = 1)
  
  if (rzut == 6) {
    return("Super Bonus")
  } else if (rzut == 4 || rzut == 5) {
    return("nagroda standardowa")
  } else {
    return("Brak Nagrody")
  }
}
przyznaj()
przyznaj()
przyznaj()
przyznaj()
przyznaj()
przyznaj()
przyznaj()
przyznaj() #zgodne
