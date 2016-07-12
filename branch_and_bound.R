bounds <- function(datos, capacidad_original){
  capacidad_lower <- capacidad_original
  capacidad_upper <- capacidad_original
  lower <- 0
  upper <- 0
  
  for(i in nrow(datos):1){
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
  return(c(lower,upper))
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
    
    if(iter %% 10000 == 0){
      message("Current Best: ", global_lower, " Upper: ", upper + ganancia_acumulada)
    }
    
    if(global_lower < lower + ganancia_acumulada){
      global_lower <<- lower + ganancia_acumulada
    }
    if(ganancia_acumulada + upper < global_lower || lower == upper || capacidad == 0){# || (capacidad < max_weight && is.na(which(datos$peso >= capacidad)[1]))){
      # prune
    } else {
      # leave it
      best(ganancia_acumulada, datos[-1,], capacidad)
      
      # take it
      if(datos[1,2] <= capacidad){
        best(ganancia_acumulada+datos[1,1], datos[-1,], capacidad - datos[1,2])
      }
    }
  }
  table      <- read.table(file, header = FALSE)
  datos      <- data.frame(ganancia = table[-1,-1]$V2, peso = table[-1,-1]$V3)
  datos$tasa <- datos$ganancia / datos$peso
  datos      <- datos[order(datos$tasa), ]
  max_weight <- max(datos$peso)
  capacidad  <- table[1,2]
  bound      <- bounds(datos, capacidad)
  global_lower <- bound[1]
  message("Greedy: ", global_lower)
  global_upper <- bound[2]
  best(0, datos, capacidad)
  message("Iterations: ", iter)

  return(global_lower)
}

report_to_console <- function(text, file){
  message("FILE: ", file)
  ptm <- proc.time()
  result <- branch(file)
  took <- proc.time() - ptm
  message(text,result)
  message("TIME: ", "user: ",took[1], "  system: ", took[2], "  elapsed: ", took[3])
  message("----------------------------------------------------------------------------------------")
}

run_branch_and_bound <- function(){
#  report_to_console("Expected: 1794  Actual: ", ("tests/test_012_2e1.in"))
#  report_to_console("Expected: 2291  Actual: ", ("tests/test_013_2e1.in"))
#  report_to_console("Expected: 4129  Actual: ", ("tests/test_014_2e1.in"))
#  report_to_console("Expected: 5370  Actual: ", ("tests/test_015_2e2.in"))
#  report_to_console("Expected: 7962  Actual: ", ("tests/test_016_2e2.in"))
#  report_to_console("Expected: 6898  Actual: ", ("tests/test_017_2e2.in"))
  report_to_console("Expected: 7500  Actual: ", ("tests/test_018_1e3.in"))
#  report_to_console("Expected: 64240  Actual: ", ("tests/test_019_1e3.in"))
#  report_to_console("Expected: 693644  Actual: ", ("tests/test_020_1e3.in"))
#  report_to_console("Expected: 54945  Actual: ", ("tests/test_021_2e3.in"))
#  report_to_console("Expected: 236420  Actual: ", ("tests/test_022_2e3.in"))
#  report_to_console("Expected: 1419266  Actual: ", ("tests/test_023_2e3.in"))
}

run_branch_and_bound()

