greedy <- function(file){
  table = read.table(file, header = FALSE)
  capacidad <- table[1,2]
  datos = data.frame(ganancia = table[-1,-1]$V2, peso = table[-1,-1]$V3)
  datos$tasa = datos$ganancia / datos$peso
  datos = datos[order(-datos$tasa), ]
  gananciaTotal <- 0
  
  for(i in 1:nrow(datos)){
    if(capacidad - datos[i, 2] > 0){
      capacidad <- capacidad - datos[i, 2]
      gananciaTotal <- gananciaTotal + datos[i, 1]    
    }
  }
  message(cat("Ganancia Total (", file, "): ", gananciaTotal))
  return(gananciaTotal)
}

greedy("tests/test_000_3e1.in")
greedy("tests/test_001_3e1.in")
greedy("tests/test_002_3e1.in")
greedy("tests/test_003_3e2.in")
greedy("tests/test_004_3e2.in")
greedy("tests/test_005_3e2.in")
greedy("tests/test_006_3e3.in")
greedy("tests/test_007_3e3.in")
greedy("tests/test_008_3e3.in")
greedy("tests/test_009_1e4.in")
greedy("tests/test_010_1e4.in")
greedy("tests/test_011_1e4.in")