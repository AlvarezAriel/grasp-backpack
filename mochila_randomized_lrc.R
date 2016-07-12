greedy_randomized <- function(file){
  table <- read.table(file, header = FALSE)
  capacidad <- table[1,2]
  datos = data.frame(ganancia = table[-1,-1]$V2, peso = table[-1,-1]$V3)
  datos$tasa = datos$ganancia / datos$peso
  datos = datos[order(-datos$tasa), ]
  tasa_total = sum(datos$tasa)
  min_tasa = min(datos$tasa)
  max_tasa = max(datos$tasa)
  alfa <- 0.6
  lrc_tope <- max_tasa - alfa*(max_tasa - min_tasa)
  lrc = datos[ datos$tasa >= lrc_tope,]
  lrc = datos[sample(x = 1:length(lrc$tasa), size = length(lrc$tasa), replace = F ), ] 
  
  gananciaTotal <- 0
  for(i in 1:nrow(lrc)){
    if(capacidad - lrc[i, 2] > 0){
      capacidad <- capacidad - lrc[i, 2]
      gananciaTotal <- gananciaTotal + lrc[i, 1]    
    }
  }
  
  if(capacidad > 0){
    for(i in (length(lrc$tasa) + 1):nrow(datos)){
      if(capacidad - datos[i, 2] > 0){
        capacidad <- capacidad - datos[i, 2]
        gananciaTotal <- gananciaTotal + datos[i, 1]    
      }
    }
  }

  return(gananciaTotal)
}

offset <- 30
run_greedy_randomized <- function(file, opt){
  return(c(apply(matrix(c(1:10)), 1, function(x){
    res <- greedy_randomized(file)
    return(round((res*100)/opt, digits = 3))
  })))
}

cases = matrix(c(
  run_greedy_randomized("tests/test_000_3e1.in", 378),
  run_greedy_randomized("tests/test_001_3e1.in", 696),
  run_greedy_randomized("tests/test_002_3e1.in", 2790),
  run_greedy_randomized("tests/test_003_3e2.in", 2499),
  run_greedy_randomized("tests/test_004_3e2.in", 7975),
  run_greedy_randomized("tests/test_005_3e2.in", 13166),
  run_greedy_randomized("tests/test_006_3e3.in", 38805),
  run_greedy_randomized("tests/test_007_3e3.in", 71925),
  run_greedy_randomized("tests/test_008_3e3.in", 268963),
  run_greedy_randomized("tests/test_009_1e4.in", 1796186),
  run_greedy_randomized("tests/test_010_1e4.in", 1055449),
  run_greedy_randomized("tests/test_011_1e4.in", 8363114)
  ), ncol = 12)

mean_rounded <- function(n) round(mean(n), digits = 3)

result_randomized = data.frame(
  min_value = apply(cases, 2, min ), 
  mean      = apply(cases, 2, mean_rounded),
  max_value = apply(cases, 2, max ),
  row.names = c(
    "00_3e1",
    "01_3e1",
    "02_3e1",
    "03_3e2",
    "04_3e2",
    "05_3e2",
    "06_3e3",
    "07_3e3",
    "08_3e3",
    "09_1e4",
    "10_1e4",
    "11_1e4")
)

par(mar=c(4.1, 4.1, 4.1, 4.1), xpd=TRUE)
greedy_plot <- barplot(
  t(data.matrix(result_randomized)), 
  names.arg = row.names(result_randomized), las=1, xlab = "Porcentaje al óptimo",
  horiz = TRUE,
  beside = TRUE
  )

text(y = greedy_plot, 
     x = result_randomized$max_value, 
     labels = result_randomized$mean,
     pos = 4, cex = 0.7, col = "black", offset = 1)
