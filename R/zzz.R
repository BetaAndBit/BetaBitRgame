.onAttach <- function(...) {
  BB.start = "
 _____     _                    _    _____ _ _      _____
| __  |___| |_ ___    ___ ___ _| |  | __  |_| |_   |   __|___ _____ ___ ___
| __ -| -_|  _| .'|  | .'|   | . |  | __ -| |  _|  |  |  | .'|     | -_|_ -|
|_____|___|_| |__,|  |__,|_|_|___|  |_____|_|_|    |_____|__,|_|_|_|___|___|

Choose your game. Just type the name of the selected game in the console.
It's a function so do not forget about parentheses!

1. proton()
2. frequon()
3. regression()
"

     packageStartupMessage(BB.start)
}

dcode <- function(tex) {
  tmp1 <- c(LETTERS, letters)
  tmp2 <- setdiff(unique(unlist(strsplit(tex, split=""))), tmp1)
  let <- c(tmp1, tmp2)
  names(let) <- c(rev(tmp1), tmp2)
  sapply(strsplit(tex, split=""), function(x){
    paste(let[x], collapse="")
  })
}

.pouch <- new.env()
