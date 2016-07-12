greedy_randomized <- function(file){
  table <- read.table(file, header = FALSE)
  capacidad <- table[1,2]
  datos = data.frame(ganancia = table[-1,-1]$V2, peso = table[-1,-1]$V3)
  datos$tasa = datos$ganancia / datos$peso
  datos = datos[order(-datos$tasa), ]
  tasa_total = sum(datos$tasa)
  datos$dist = (datos$tasa / tasa_total)
  
  datos = datos[sample(x = 1:length(datos$dist), size = length(datos$dist), prob = datos$dist, replace = F ), ] 
  
  gananciaTotal <- 0
  for(i in 1:nrow(datos)){
    if(capacidad - datos[i, 2] > 0){
      capacidad <- capacidad - datos[i, 2]
      gananciaTotal <- gananciaTotal + datos[i, 1]    
    }
  }
  return(gananciaTotal)
}

run_greedy_randomized <- function(file, opt){
  return(c(apply(matrix(c(1:20)), 1, function(x){
    res <- greedy_randomized(file)
    return((round(res, digits = 3)*100)/opt)
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

result_randomized = data.frame(
  min_value = apply(cases, 2, min ), 
  mean      = apply(cases, 2, mean),
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

par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
greedy_plot <- barplot(
  t(data.matrix(result_randomized)), 
  names.arg = row.names(result_randomized), las=2, xlab = "Test", ylab = "Porcentaje al óptimo",
  beside = TRUE
)
#text(greedy_plot, 0, round(result_randomized, 1),cex=1,pos=3) 
