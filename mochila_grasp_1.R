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

greedy_randomized_construction <- function(datos, original_capacity, factor){
  datos = datos[order(-datos$tasa), ]
  min_tasa = min(datos$tasa)
  max_tasa = max(datos$tasa)
  index <- greedy_position(datos, original_capacity)$max_index
  # lrc_tope <- max_tasa - factor*(max_tasa - min_tasa)
  lrc_tope <- ceiling(min(index + index*factor, nrow(datos)))
  # lrc_tope <- median(datos$tasa)
  lrc = head(datos,lrc_tope)
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
  
  return(list("mejor_ganancia"=gananciaTotal, "seleccionados"=seleccionados, "lrc"=lrc, "data"=datos))
}

local_search_sample<- function(solution, capacidad_original, neighbourhood_size){
  mejor_ganancia <- solution$mejor_ganancia
  lrc <- solution$lrc
  datos <- solution$data
  
  GAN <- 1; CAP <- 2
  
  for(x in 1:(min(nrow(lrc),neighbourhood_size))){
    ganancia <- 0
    capacity <- capacidad_original
    remaining <- lrc
    max_index <- x
    entry <- remaining[max_index,]
    remaining <- remaining[-max_index,]
    if(capacity - entry[2] > 0){
      capacity <- capacity - entry[2]
      ganancia  <- ganancia + entry[1]    
    }
    
    for(i in 1:nrow(lrc)){
      if(length(remaining$tasa) == 0) break
      tasa_total <- sum(remaining$tasa)
      prob <- remaining$tasa / tasa_total
      max_index <- sample( remaining , prob = prob, 1)
      entry <- remaining[max_index,]
      remaining <- remaining[-max_index,]
      
      if(capacity - entry[2] > 0){
        capacity <- capacity - entry[2]
        ganancia  <- ganancia + entry[1]    
      }
    } 
    
    if(capacity > 0 &&  (length(lrc$tasa) + 1) <= nrow(datos) ){
      for(i in (length(lrc$tasa) + 1):nrow(datos)){
        if(capacity - datos[i, 2] > 0){
          capacity <- capacity - datos[i, 2]
          ganancia <- ganancia + datos[i, 1]    
        }
      }
    }
    
    mejor_ganancia <- max(mejor_ganancia, ganancia[1,])
  }
  
  
  return(mejor_ganancia)
}

grasp <- function(file, max_iterations, lrc_factor, neighbourhood_size){
  data <- read_data(file)
  best_earning <- greedy_position(data$input, data$capacity)$earnings
  message("Greedy: ", best_earning)
  for(i in 1:max_iterations){
    solution     <- greedy_randomized_construction(data$input, data$capacity,lrc_factor)
    earning      <- local_search_sample(solution, data$capacity, neighbourhood_size)
    best_earning <- max(best_earning, earning)
  }
  return(best_earning)
}

offset <- 30
percentage_to_optimum <- function(res,opt){
  return(round((res*100)/opt, digits = 3))
}

run_greedy_randomized <- function(file, opt){
  
  return(c(
    percentage_to_optimum(grasp(file, max_iterations = 10, lrc_factor = 0.6, neighbourhood_size = 1), opt),
    percentage_to_optimum(grasp(file, max_iterations = 10, lrc_factor = 0.02, neighbourhood_size = 5), opt),
    percentage_to_optimum(grasp(file, max_iterations = 10, lrc_factor = 0.01, neighbourhood_size = 5), opt)
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

cases = matrix(tests, ncol = length(tests), nrow = 3)

result_matrix = cases

par(mar=c(4.1, 4.1, 4.1, 4.1), xpd=TRUE)
greedy_plot <- barplot(
  result_matrix, 
  names.arg = 23:12, las=1, xlab = "Porcentaje al óptimo",
  horiz = TRUE,
  beside = TRUE
)

text(y = greedy_plot, 
     x = result_matrix, 
     labels = result_matrix,
     pos = 4, cex = 0.7, col = "black", offset = 1)
