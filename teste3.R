########################################
# Teste 3 - INF-0612          
# Nome(s): Atila de Moura Tavano Moretto
########################################


## 1 - Maximo Divisor Comum

gcd2 <- function(x, y) {
  if (y == 0) {
    return(x)
  } else {
    return(gcd2(y, x %% y))
  }
}


gcd <- function(...) {
  vec <- c(...)
  x <- 0
  for (i in vec) {
    x <- gcd2(x,i)
  }
  return(x)
}



## 2 - Moda da Idade da Turma

count <- function(vector, element) {
  count <- 0
  for (i in vector) {
    if (i == element) {
      count <- count + 1
    }
  }
  return(count)
}



mode <- function(vector) {
  res <- c()
  current_max <- 0
  for (i in vector) {
    if (!is.element(i,res)) {
      occur <- count(vector,i)
      if (current_max < occur) {
        res <- c()
        res[1] <- i
        current_max <- occur
      } else {
        if (current_max == occur) {
          res[length(res)+1] <- i
        }
      }
    }
  }
  return(res)
}


## 3 - Binario para Decimal


binToDec <- function(...) {
  listVectorBin <- list(...)
  res <- c()
  for (vectorBin in listVectorBin) {
    dec <- 0
    len <- length(vectorBin)
    for(i in len:1) {
        dec <- dec + vectorBin[i]*(2^(len - i))
    }
    res[length(res)+1] <- dec
  }
  return(res)
}



## 4 - Ocorrencia de palavras

wordCount <- function(word, text) {
  wordList <- strsplit(text,"[ |.|,|!|?]")[[1]]
  count <- 0
  for(w in wordList) {
    if (toupper(w) == toupper(word)) {
      count <- count + 1
    }
  }
  return(count)
}



