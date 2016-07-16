
bounds <- function(datos, capacidad_original){
  capacidad_lower <- capacidad_original
  capacidad_upper <- capacidad_original
  lower <- 0
  upper <- 0
  for(i in 1:nrow(datos)){
    if(capacidad_upper - datos[i, 2] > 0){
      capacidad_upper <- capacidad_upper - datos[i, 2]
      upper <- upper + datos[i, 1]
    } else if(capacidad_upper > 0) {
      upper <- upper + (capacidad_upper*datos[i,1])/datos[i, 2]
      capacidad_upper <- 0
    }
    
    if(capacidad_lower - datos[i, 2] > 0){
      capacidad_lower <- capacidad_lower - datos[i, 2]
      lower <- lower + datos[i, 1]    
    }
  }
  return(c(lower,floor(upper)))
}

# Como ordeno por tasa, en cada nodo puedo aprovechar la tasa para saber si es factible que se llegue al menos al lower bound
# En cada nodo aplico la heurística primal: una heurística para conseguir una solución, dado que ya hay selecciones fijas.

branch <- function(file){
  global_lower <- 0
  global_upper <- 0
  iter <- 0
  max_weight <- 0
  best <- function(ganancia_acumulada, datos,capacidad){
    iter <<- iter + 1
    
    if(length(datos$ganancia) == 0){
      if(ganancia_acumulada > global_lower){
        global_lower <<- ganancia_acumulada 
      }
      return()
    }
    
    bound <- bounds(datos, capacidad)
    lower <- bound[1]
    upper <- bound[2]
    
    if(iter > 60000){
      message("Detenido: 60000 llamadas recursivas")
      return()
    }
    
    if(global_lower < lower + ganancia_acumulada){
      global_lower <<- lower + ganancia_acumulada
    }
    if(lower == upper || capacidad == 0){# || (capacidad < max_weight && is.na(which(datos$peso >= capacidad)[1]))){
      # prune
    } else {

      # take it
      if(datos[1,2] <= capacidad){
        best(ganancia_acumulada+datos[1,1], datos[-1,], capacidad - datos[1,2])
      }
      
      # leave it
      best(ganancia_acumulada, datos[-1,], capacidad)
      
    }
  }
  table      <- read.table(file, header = FALSE)
  datos      <- data.frame(ganancia = table[-1,-1]$V2, peso = table[-1,-1]$V3)
  datos$tasa <- datos$ganancia / datos$peso
  datos      <- datos[order(-datos$tasa), ]
  max_weight <- max(datos$peso)
  capacidad  <- table[1,2]
  bound      <- bounds(datos, capacidad)
  global_lower <- bound[1]
  greedy_value <- global_lower
  global_upper <- bound[2]
  best(0, datos, capacidad)
  return(list("best"=global_lower,"greedy"=greedy_value,"iterations"=iter))
}

report_to_console <- function(opt, file){
  ptm <- proc.time()
  result <- branch(file)
  took <- proc.time() - ptm
  message("|",file,"|",result$greedy,"|",result$iterations,"|",opt, "|", result$best, "|", took[3],"|")
}

run_branch_and_bound <- function(){
  message("|Test|Resultado Greedy|Iteraciones|Optimo|Obtenido|Tiempo|")
  message("|--|--|--|--|--|--|")

  report_to_console(1794, ("tests/test_012_2e1.in"))
  report_to_console(2291, ("tests/test_013_2e1.in"))
  report_to_console(4129, ("tests/test_014_2e1.in"))
  report_to_console(5370, ("tests/test_015_2e2.in"))
  report_to_console(7962, ("tests/test_016_2e2.in"))
  report_to_console(6898, ("tests/test_017_2e2.in"))
  report_to_console(7500, ("tests/test_018_1e3.in"))
  report_to_console(64240, ("tests/test_019_1e3.in"))
  report_to_console(693644, ("tests/test_020_1e3.in"))
  report_to_console(54945, ("tests/test_021_2e3.in"))
  report_to_console(236420, ("tests/test_022_2e3.in"))
  report_to_console(1419266, ("tests/test_023_2e3.in"))
}

run_branch_and_bound()

