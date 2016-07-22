read_data <- function(file){
  table <- read.table(file, header = FALSE)
  capacidad_original <- table[1,2]
  datos = data.frame(ganancia = table[-1,-1]$V2, peso = table[-1,-1]$V3)
  datos$tasa = datos$ganancia / datos$peso
  return(list("input"=datos, "capacity"=capacidad_original))
}


greedy_position <- function(data, capacity){
  max_index <- 1
  ganancia  <- 0
  for(i in 1:nrow(data)){
    if(capacity - data[i, 2] > 0){
      capacity <- capacity - data[i, 2]
      ganancia  <- ganancia + data[i, 1]    
      max_index <- i
    }
  }
  return(list("max_index"=max_index, "earnings" = ganancia))
}

greedy_randomized_construction <- function(datos, original_capacity, factor, lrc_strategy){
  datos = datos[order(-datos$tasa), ]
  min_tasa = min(datos$tasa)
  max_tasa = max(datos$tasa)
  index <- greedy_position(datos, original_capacity)$max_index
  
  if(lrc_strategy == "factor"){
    lrc_tope <- ceiling(min(index + index*factor, nrow(datos)))
    lrc = head(datos,lrc_tope)
  } else if (lrc_strategy == "median"){
    median <- median(datos$tasa)
    lrc_tope <- median - median*factor
    lrc = datos[datos$tasa > lrc_tope,]
  } else if (lrc_strategy == "mean") {
    mean <- mean(datos$tasa)
    lrc_tope <- mean - mean*factor
    lrc = datos[datos$tasa > lrc_tope,]
  }
  
  
  lrc = datos[sample(x = 1:length(lrc$tasa), size = length(lrc$tasa), replace = F ), ] 
  seleccionados = data.frame(e=c(1:nrow(lrc)))
  seleccionados[1]<-FALSE
  
  mejor_ganancia <- 0
  
  gananciaTotal <- 0
  capacidad <- original_capacity
  for(i in 1:nrow(lrc)){
    if(capacidad - lrc[i, 2] > 0){
      seleccionados[i,1]<-TRUE 
      capacidad <- capacidad - lrc[i, 2]
      gananciaTotal <- gananciaTotal + lrc[i, 1]    
    }
  }
  
  if(capacidad > 0 &&  (length(lrc$tasa) + 1) <= nrow(datos) ){
    for(i in (length(lrc$tasa) + 1):nrow(datos)){
      if(capacidad - datos[i, 2] > 0){
        capacidad <- capacidad - datos[i, 2]
        gananciaTotal <- gananciaTotal + datos[i, 1]    
      }
    }
  }
  
  return(list("mejor_ganancia"=gananciaTotal, "seleccionados"=unlist(seleccionados), "lrc"=lrc, "data"=datos))
}


obtener_vecinos <- function(seleccionados, capacidad_original, neighbourhood_size){
  vecinos <- data.frame(index=1:neighbourhood_size)
  selected_index <- 1
  vecino_index <- 1
  while(neighbourhood_size - vecino_index > 0){
    if(seleccionados[selected_index]){
      # nuevo <- seleccionados
      # nuevo[selected_index] <- FALSE
      # vecinos$vecino[vecino_index] <- nuevo
      vecinos$fixed[vecino_index] <- selected_index
      vecino_index <- vecino_index + 1
    }
    selected_index <- selected_index + 1
  }
  return(vecinos)
}

mejor_vecino <- function(vecinos, previos, lrc, capacidad){
  mejor <- vecinos[1]
  mejor_ganancia <- 0
  for(v in 1:length(vecinos)){
    fixed <- vecinos[v,2]
    seleccionados <- previos
    seleccionados[fixed] <- FALSE
    cap <- capacidad - sum(lrc$peso[seleccionados])
    
    for(i in (fixed+1):length(lrc)){
      if(cap - lrc[i,1]> 0){
        cap <- cap - lrc[i,1]
        seleccionados[i] <- TRUE
        if(cap == 0) break
      }
    }
    nueva_ganancia <- sum(lrc$ganancia[seleccionados])
    if(mejor_ganancia < nueva_ganancia){
      mejor_ganancia <- nueva_ganancia
      mejor <- seleccionados
    }
  }
  return(list("ganancia"=mejor_ganancia, "seleccionados"=mejor))
}

mejor_vecino_sample <- function(vecinos, previos, lrc, capacidad){
  mejor <- vecinos[1]
  mejor_ganancia <- 0
  for(v in 1:length(vecinos)){
    fixed <- vecinos[v,2]
    seleccionados <- previos
    seleccionados[fixed] <- FALSE
    cap <- capacidad - sum(lrc$peso[seleccionados])
    tasa_total <- sum(lrc$tasa)
    prob <- lrc$tasa / tasa_total
    positions <- sample.int(length(lrc$tasa), length(lrc$tasa), replace = FALSE, prob)
    for(i in positions){
      if(i != fixed && !seleccionados[i]  && cap - lrc[i,1]> 0){
        cap <- cap - lrc[i,1]
        seleccionados[i] <- TRUE
        if(cap == 0) break
      }
    }
    nueva_ganancia <- sum(lrc$ganancia[seleccionados])
    if(mejor_ganancia < nueva_ganancia){
      mejor_ganancia <- nueva_ganancia
      mejor <- seleccionados
    }
  }
  return(list("ganancia"=mejor_ganancia, "seleccionados"=mejor))
}

local_search_sample_fixed <- function(solution, capacidad_original, neighbourhood_size){
  mejor_ganancia <- solution$mejor_ganancia

  vecinos <- obtener_vecinos(solution$seleccionados, capacidad_original, neighbourhood_size)
  mejor <- mejor_vecino_sample(vecinos, solution$seleccionados, solution$lrc, capacidad_original)

  while(mejor_ganancia < mejor$ganancia){
    mejor_ganancia <- mejor$ganancia
    vecinos <- obtener_vecinos(mejor$seleccionados, capacidad_original, neighbourhood_size)
    mejor <- mejor_vecino_sample(vecinos, mejor$seleccionados, solution$lrc, capacidad_original)
  }

  return(mejor_ganancia)
}


grasp <- function(file, max_iterations, lrc_factor, neighbourhood_size, lrc_strategy){
  ptm <- proc.time()
  data <- read_data(file)
  best_earning <- greedy_position(data$input, data$capacity)$earnings
  message("Greedy: ", best_earning)
  for(i in 1:max_iterations){
    solution     <- greedy_randomized_construction(data$input, data$capacity,lrc_factor,lrc_strategy)
    amount_selected <- length(solution$seleccionados[mapply(function(X) { X }, solution$seleccionados) == TRUE])
    earning      <- local_search_sample_fixed(solution, data$capacity, min(neighbourhood_size, amount_selected))
    best_earning <- max(best_earning, earning)
  }
  message(file," ~~> ", (proc.time() - ptm)[3])
  return(best_earning)
}

offset <- 30
percentage_to_optimum <- function(res,opt){
  return(min(round((res*100)/opt, digits = 3), 100))
}

run_greedy_randomized <- function(file, opt){
  
  return(c(
    percentage_to_optimum(grasp(file, max_iterations = 30, lrc_factor = 0.01, neighbourhood_size = 2, lrc_strategy = "factor"), opt),
    percentage_to_optimum(grasp(file, max_iterations = 30, lrc_factor = 0.01, neighbourhood_size = 5, lrc_strategy = "factor"), opt),
    percentage_to_optimum(grasp(file, max_iterations = 30, lrc_factor = 0.01, neighbourhood_size = 15, lrc_strategy = "factor"), opt)
    ))

}

tests = c(
  run_greedy_randomized("tests/test_012_2e1.in", 1794),
  run_greedy_randomized("tests/test_013_2e1.in", 2291),
  run_greedy_randomized("tests/test_014_2e1.in", 4129),
  run_greedy_randomized("tests/test_015_2e2.in", 5370),
  run_greedy_randomized("tests/test_016_2e2.in", 7962),
  run_greedy_randomized("tests/test_017_2e2.in", 6898),
  run_greedy_randomized("tests/test_018_1e3.in", 7500),
  run_greedy_randomized("tests/test_019_1e3.in", 64240),
  run_greedy_randomized("tests/test_020_1e3.in", 693644),
  run_greedy_randomized("tests/test_021_2e3.in", 54945),
  run_greedy_randomized("tests/test_022_2e3.in", 236420),
  run_greedy_randomized("tests/test_023_2e3.in", 1419266)
)

cases = matrix(tests, ncol = length(tests)/3, nrow = 3)

result_matrix = cases
range_tests <- 12:23

par(mar=c(9.2, 4.1, 1.1, 4.1), xpd=TRUE)
greedy_plot <- barplot(
  result_matrix, 
  names.arg = range_tests, las=1, xlab = "Porcentaje al óptimo",
  horiz = TRUE,
  beside = TRUE,
  col = terrain.colors(3)
)

legend(x = 0, y= -10, inset=.05, title="Neighbourhood Size",
       c("2", "5","10"), fill=terrain.colors(3), horiz=TRUE)

text(y = greedy_plot, 
     x = result_matrix, 
     labels = result_matrix,
     pos = 4, cex = 0.7, col = "black", offset = 1)
