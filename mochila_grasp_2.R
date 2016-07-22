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
      max_index <- sample.int(length(remaining$tasa), 1, replace = FALSE, prob)
      # max_index <- sample( remaining , 1, replace = FALSE, prob = prob)
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

local_search <- function(solution, capacidad_original, neighbourhood_size){
  mejor_ganancia <- solution$mejor_ganancia
  seleccionados <- solution$seleccionados
  lrc <- solution$lrc
  datos <- solution$data
  
  GAN <- 1; CAP <- 2
  
  sel_aux <- data.frame(e=c(1:nrow(lrc)))
  sel_aux[1]<-FALSE
  u <- 1
  for(i in 1:neighbourhood_size){
    for(r in u:nrow(seleccionados)){
      if(seleccionados[r,1]){
        u <- r
        break 
      }
    }
    
    gananciaTotal <- lrc[u, GAN]
    capacidad <- capacidad_original - lrc[u,CAP]
    for(i in 1:nrow(lrc)){
      if(capacidad - lrc[i, CAP] > 0 && i != u){
        sel_aux[i,1]<-TRUE
        capacidad <- capacidad - lrc[i, CAP]
        gananciaTotal <- gananciaTotal + lrc[i, GAN]    
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
    
    if(gananciaTotal > mejor_ganancia){
      mejor_ganancia <- gananciaTotal
      seleccionados <- sel_aux
    }
    
    u <- u + 1
    if(u > nrow(seleccionados)){
      break
    }
  }
  
  return(mejor_ganancia)
}

grasp <- function(file, max_iterations, lrc_factor, neighbourhood_size, lrc_strategy){
  ptm <- proc.time()
  data <- read_data(file)
  best_earning <- greedy_position(data$input, data$capacity)$earnings
  message("Greedy: ", best_earning)
  for(i in 1:max_iterations){
    if(i %% 3 == 0){
      neighbourhood_size <- neighbourhood_size + 1
    }
    solution     <- greedy_randomized_construction(data$input, data$capacity,lrc_factor,lrc_strategy)
    earning      <- local_search(solution, data$capacity, neighbourhood_size)
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
    percentage_to_optimum(grasp(file, max_iterations = 30, lrc_factor = 0.01, neighbourhood_size = 1, lrc_strategy = "factor"), opt)
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

cases = matrix(tests, ncol = length(tests)/1, nrow = 1)

result_matrix = cases
range_tests <- 12:23

par(mar=c(9.2, 4.1, 1.1, 4.1), xpd=TRUE)
greedy_plot <- barplot(
  result_matrix, 
  names.arg = range_tests, las=1, xlab = "Porcentaje al óptimo",
  horiz = TRUE,
  beside = TRUE,
  col = terrain.colors(1)
)

legend(x = -5, y= -15, inset=.05, title="Neighbourhood Size",
       c("de 1 a 10", "de 3 a 13","de 8 a 18"), fill=terrain.colors(1), horiz=TRUE)

text(y = greedy_plot, 
     x = result_matrix, 
     labels = result_matrix,
     pos = 4, cex = 0.7, col = "black", offset = 1)



grasp = c(0.138999999999214
,0.148000000001048
,0.199000000000524
,1.15599999999904
,1.20799999999872
,1.1820000000007
,4.32500000000073)

branch = c(0.081000000  ,
0.0549999999 ,
0.128000000  ,
2.24200000   ,
27.7250000   ,
11.6619999   ,
57.0219999   )

backtracking = c(
  0.82300000000032,
  0.82300000000032,
  6.53299999999945,
  175.59,
  171.174,
  200,
  200)

cases = t(matrix(c(grasp,branch,backtracking), ncol = 3, nrow = 7))

result_matrix = cases
range_tests <- 12:18

par(mar=c(9.2, 4.1, 1.1, 4.1), xpd=TRUE)
greedy_plot <- barplot(
  result_matrix, 
  names.arg = range_tests, las=1, xlab = "Tiempo en segundos",
  horiz = TRUE,
  beside = TRUE,
  col = terrain.colors(3)
)

legend(x = -5, y= -5, inset=.05, title="Algoritmo",
       c("GRASP", "Branch & Bound","Backtracking"), fill=terrain.colors(3), horiz=TRUE)

text(y = greedy_plot, 
     x = result_matrix, 
     labels = result_matrix,
     pos = 4, cex = 0.7, col = "black", offset = 1)
