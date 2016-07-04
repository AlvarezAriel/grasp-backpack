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
  return(gananciaTotal)
}

greedy_result  = data.frame( 
  optimo = c(378,696,2790,2499,7975,13166,38805,71925,268963,1796186,1055449,8363114), 
  greedy = c(
    greedy("tests/test_000_3e1.in"),
    greedy("tests/test_001_3e1.in"),
    greedy("tests/test_002_3e1.in"),
    greedy("tests/test_003_3e2.in"),
    greedy("tests/test_004_3e2.in"),
    greedy("tests/test_005_3e2.in"),
    greedy("tests/test_006_3e3.in"),
    greedy("tests/test_007_3e3.in"),
    greedy("tests/test_008_3e3.in"),
    greedy("tests/test_009_1e4.in"),
    greedy("tests/test_010_1e4.in"),
    greedy("tests/test_011_1e4.in")),
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
greedy_result$percentage = round((greedy_result$greedy * 100)/greedy_result$optimo, digits = 2)
par(mar=c(5.1, 4.1, 4.1, 7.1), xpd=TRUE)
greedy_plot <- barplot(greedy_result$percentage, names.arg = row.names(greedy_result), las=2, xlab = "Test", ylab = "Porcentaje al óptimo", ylim = c(0,100))
text(x = greedy_plot, y = greedy_result$percentage, label = greedy_result$percentage, pos = 3, cex = 0.8, col = "red")
