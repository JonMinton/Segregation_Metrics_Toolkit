# Segregation measures 

# inputs needed: 
# minority count vector p
# total count vector t

# MEASURES OF EVENNESS

Dissimilarity <- function(counts_minority, counts_total){
  
  if (length(counts_minority) != length(counts_total)){
    stop("Lengths of vectors are not equal")
  }
  
  if (any(counts_minority > counts_total)){
    stop("Minority counts greater than total counts")
  }

  proportion_minority <- counts_minority / counts_total
  
  
  overall_total <- sum(counts_total)
  minority_total <- sum(counts_minority)
  overall_proportion_minority <- minority_total / overall_total
  
  D <- sum(
    counts_total * {
      abs(proportion_minority - overall_proportion_minority)  
      } /(2 * overall_total * overall_proportion_minority * (1 - overall_proportion_minority)) 
    )
  
  return(D)
}


GiniCoefficient <- function(counts_minority, counts_total){
  if (length(counts_minority) != length(counts_total)){
    stop("Lengths of vectors are not equal")
  }
  
  if (any(counts_minority > counts_total)){
    stop("Minority counts greater than total counts")
  }
  
  proportion_minority <- counts_minority / counts_total
  
  overall_total <- sum(counts_total)
  minority_total <- sum(counts_minority)
  overall_proportion_minority <- minority_total / overall_total
  
  G <- 0
  n <- length(counts_minority)
  
  for (i in 1:n){
    for (j in 1:n){
      tmp <- counts_total[i] * counts_total[j]
      tmp <- tmp * abs(proportion_minority[i] - proportion_minority[j])
      tmp2 <- 2 * overall_total^2 * overall_proportion_minority * (1 - overall_proportion_minority)
      
      tmp <- tmp / tmp2
      G <- G + tmp
    }
  }
  
  return(G)
}


Entropy <- function(counts_minority, counts_total){
  
  if (length(counts_minority) != length(counts_total)){
    stop("Lengths of vectors are not equal")
  }
  
  if (any(counts_minority > counts_total)){
    stop("Minority counts greater than total counts")
  }
  
  
  proportion_minority <- counts_minority / counts_total
  
  overall_total <- sum(counts_total)
  minority_total <- sum(counts_minority)
  overall_proportion_minority <- minority_total / overall_total
  
  tmp1 <- overall_proportion_minority * log(1/overall_proportion_minority)
  tmp2 <- (1 - overall_proportion_minority) * log(1 / (1 - overall_proportion_minority))
  overall_entropy <- tmp1 + tmp2
  
  H <- 0
  n <- length(counts_minority)
  for (i in 1:n){
    tmp1 <- proportion_minority[i] * log(1/proportion_minority[i])
    if (is.nan(tmp1)) tmp1 <- 0 # catch issue if proportion_minority is 0
    tmp2 <- (1 - proportion_minority[i]) * log(1/(1 - proportion_minority[i]))
    if (is.nan(tmp2)) tmp2 <- 0 # catch issue if proportion_minority is 1
    area_entropy <- tmp1 + tmp2
    
    tmp3 <- counts_total[i] * (overall_entropy - area_entropy)
    tmp3 <- tmp3 / (overall_entropy * overall_total)
    
    H <- H + tmp3

  }

  return(H)
}

Atkinson <- function(counts_minority, counts_total, b= 0.5){
  if (length(counts_minority) != length(counts_total)){
    stop("Lengths of vectors are not equal")
  }
  
  if (any(counts_minority > counts_total)){
    stop("Minority counts greater than total counts")
  }
  
  proportion_minority <- counts_minority / counts_total
  
  overall_total <- sum(counts_total)
  minority_total <- sum(counts_minority)
  overall_proportion_minority <- minority_total / overall_total
  
  
  
  
  A2 <- 0
  n <- length(counts_minority)
  
  for (i in 1:n){
    tmp1 <- (1 - proportion_minority[i])^(1-b)
    tmp2 <- proportion_minority[i]^b
    tmp3 <- counts_total[i]/(overall_proportion_minority*overall_total)
    tmp4 <- (tmp1*tmp2*tmp3)
    A2 <- A2 + tmp4
  }
  
  A2 <- abs(A2)^(1/(1-b))
  A <- 1 - (overall_proportion_minority/(1 - overall_proportion_minority))*A2
  
  return(A)
}


# MEASURES OF EXPOSURE


x_Pstar_y <- function(counts_x, counts_y=NULL, counts_total){
  
  if(is.null(counts_y)) counts_y <- counts_x 
  # if y is not specified then this becomes x_Pstar_x
  
  if (
    (length(counts_x) != length(counts_y)) || (
      length(counts_y) != length(counts_total)
      )
    ){
    stop("Lengths of vectors are not equal")
  }
  if (any(counts_x > counts_total) || any(counts_y > counts_total)){
    stop("Group counts greater than total counts")
  }
  
  total_x <- sum(counts_x)
  
  output <- sum((counts_x/total_x) * (counts_y/counts_total))
  
  return(output)
}

V <- function(counts_x, counts_total){
  if (length(counts_x) != length(counts_total)){
    stop("Lengths of vectors are not equal")
  }
  pstar <- x_Pstar_y(counts_x=counts_x, counts_total=counts_total)
  
  overall_proportion_minority <- sum(counts_x) / sum(counts_total)
  
  output <- (pstar - overall_proportion_minority)/(1 - overall_proportion_minority)
  
  return(output)
}


# CONCENTRATION

Del <- function(counts_x, land_areas){
  if (length(counts_x) != length(land_areas)){
    stop("Lengths of vectors are not equal")
  }
  
  total_counts_x <- sum(counts_x)
  total_land_area <- sum(land_areas)
  
  output <- 0.5 * sum(counts_x/total_counts_x - land_areas/total_land_area)
  return(output)
}

Aco <- function(counts_x, counts_total, land_areas){
  
  if(
    (length(counts_x) != length(counts_total)) || {
      (length(counts_total) != length(land_areas))
    })
     {
    stop("Lengths of vectors are not equal")
  }
  
  total_x <- sum(counts_x)
  overall_total <- sum(counts_total)
  
  # sort in ascending order by land areas
  counts_x <- counts_x[order(land_areas)]
  counts_total <- counts_total[order(land_areas)]
  land_areas <- land_areas[order(land_areas)]
  
  # need to caclulate n1, the rank of the tract where the cumulative 
  # total population of areal units equals/exceeds the total minority population
  # of the city
  
  n <- length(counts_total)
  n1 <- which(cumsum(counts_total)> total_x)[1]
  # also need to calculate n2, the rank of the tract where the cumulative
  # total population, in reverse order, equals/exceeds the total minority population
  # or equivalently, the rank where the cumulative population exceeds the 
  # total non-minority population
  
  n2 <- n - (which(cumsum(counts_total) > (overall_total - total_x))[1]) + 1
  
  T1 <- sum(counts_total[1:n1])
  T2 <- sum(counts_total[n2:n])
  
  num1 <- sum(counts_x*land_areas/total_x) 
  num2 <- sum(counts_x[1:n1]*land_areas[1:n1]/T1)
  num <- num1 - num2
  
  denom1 <- sum(counts_total[n2:n]*land_areas[n2:n]/T2)
  denom2 <- sum(counts_total[1:n1]*land_areas[1:n1]/T1)
  denom <- denom1 - denom2
  
  output <- 1 - num / denom
  
  return(output)
}


RCO <- function(counts_x, counts_y, counts_total, land_areas){
  
  if(
    (length(counts_x) != length(counts_total)) || {
      (length(counts_total) != length(land_areas)) || {
        (length(counts_x) != length(counts_y))
      }
    })
  {
    stop("Lengths of vectors are not equal")
  }
  
  total_x <- sum(counts_x)
  total_y <- sum(counts_y)
  
  if (total_x > total_y){
    stop("Minority group is majority group")
  }
  
  if (total_x + total_y > sum(counts_total)){
    stop("total of two specified groups exceeds overall total")
  }
  
  overall_total <- sum(counts_total)
 
  # sort in ascending order by land areas
  counts_x <- counts_x[order(land_areas)]
  counts_y <- counts_y[order(land_areas)]
  counts_total <- counts_total[order(land_areas)]
  land_areas <- land_areas[order(land_areas)]
  
  # need to caclulate n1, the rank of the tract where the cumulative 
  # total population of areal units equals/exceeds the total minority population
  # of the city
  
  n <- length(counts_total)
  n1 <- which(cumsum(counts_total)> total_x)[1]
  # also need to calculate n2, the rank of the tract where the cumulative
  # total population, in reverse order, equals/exceeds the total minority population
  # or equivalently, the rank where the cumulative population exceeds the 
  # total non-minority population
  
  n2 <- n - (which(cumsum(counts_total) > (overall_total - total_x))[1]) + 1
  
  T1 <- sum(counts_total[1:n1])
  T2 <- sum(counts_total[n2:n])
  
  num <- sum(counts_x*land_areas/total_x) / sum(counts_y*land_areas/total_y)
  num <- num - 1
  
  denom1 <- sum(counts_total[1:n1]*land_areas[1:n1]/T1)
  denom2 <- sum(counts_total[n2:n]*land_areas[n2:n]/T2) - 1
  denom <- denom1/denom2
  
  output <- num / denom
  
  return(output)
}

PCC <- function(counts_centre, counts_total){
  if (sum(counts_centre) > sum(counts_total)){
    stop("the total population must be larger than the number living in the centre")
  }
  
  output <- sum(counts_centre) / sum(counts_total) 
  return(output)
}

# RELATIVE CENTRALISATION INDEX - 
# NOT COMPUTABLE GIVEN DEFINITION BECAUSE IT USES X_0
# WHICH IS UNDEFINED

# ABSOLUTE CENTRALISATION INDEX = 
# NOT COMPUTABLE GIVEN DEFINITION BECAUSE IT USES X_0
# WHICH IS UNDEFINED

# CLUSTERING

# Will assume a n1 by n2 matrices, showing showing minority cell-count
# i.e. that areal units will be 'digitised' into squares

# Binary contiguity
#   - only makes sense if looking at non-border areas

ACL <- function(minority_matrix_counts, total_matrix_counts){
  
  if(!is.matrix(minority_matrix_counts)){
    stop("A matrix has not been passed")
  }
  if (!all(dim(minority_matrix_counts)==(dim(total_matrix_counts)))){
    stop("are not correct")
  }
  if (any(minority_matrix_counts < 0)){
    stop("negative cell counts passed")
  }
  
  n1 <- dim(minority_matrix_counts)[1]
  n2 <- dim(minority_matrix_counts)[2]
  m <- minority_matrix_counts
  M <- sum(m)
  t <- total_matrix_counts
  Ttl <- sum(t)
  
  num1 <- 0
  num2 <- 0
  for (i in 2:(n1-1)){
    for (j in 2:(n2-1)){
      num1 <- num1 + (m[i,j]/M) * {
        m[i-1,j-1] + m[i-1,j  ] + m[i-1,j+1] +
        m[i  ,j-1]              + m[i  ,j+1] +
        m[i+1,j-1] + m[i+1,j  ] + m[i+1,j+1]        
      }
      
      num2 <- num2 + (m[i,j]/M) * {
        t[i-1,j-1] + t[i-1,j  ] + t[i-1,j+1] +
        t[i  ,j-1]              + t[i  ,j+1] +
        t[i+1,j-1] + t[i+1,j  ] + t[i+1,j+1]                
      }
    }
  }
  num3 <- (M / (n1*n2)) * 8 * (n1-2)*(n2-2)
  
  output <- (num1 - num3)/(num2 - num3)
  
  return(output)
}


MakeDf <- function(inmat){
  nrow <- dim(inmat)[1]
  ncol <- dim(inmat)[2]
  
  df <- expand.grid(x1=as.vector(inmat), x2=as.vector(inmat), d=NA)
  
  attach(df)
  for (i in 1:dim(df)[1]){
    x1.x <- ((x1[i]-1) %%  nrow)   + 1
    x1.y <- ((x1[i]-1) %/% nrow) + 1
    
    x2.x <- ((x2[i]-1) %% nrow ) + 1
    x2.y <- ((x2[i]-1) %/% nrow) + 1
    
    df$d[i] <- ((x2.y - x1.y)^2 + (x2.x - x1.x)^2)^0.5
  }
  detach(df)

  return(df)
}
# x : minority
# y : majority

SP <- function(x_matrix_counts, y_matrix_counts, total_matrix_counts){
  
  if(!is.matrix(x_matrix_counts)){
    stop("A matrix has not been passed")
  }
  if(!is.matrix(y_matrix_counts)){
    stop("A matrix has not been passed")
  }
  if(!is.matrix(total_matrix_counts)){
    stop("A matrix has not been passed")
  }
  
  
  
  if (!all(dim(x_matrix_counts)==(dim(total_matrix_counts)))){
    stop("are not correct")
  }
  
  if (any(x_matrix_counts < 0)){
    stop("negative cell counts passed")
  }
  
  
  n1 <- dim(x_matrix_counts)[1]
  n2 <- dim(x_matrix_counts)[2]
  x <- x_matrix_counts
  X <- sum(x)
  y <- y_matrix_counts
  Y <- sum(y)
  t <- total_matrix_counts
  Ttl <- sum(t)
  
  
  DF.x <- MakeDf(X)
  DF.y <- MakeDF(Y)
  
  DF.t <- MakeDF(t)

  # FOR EACH AREA
  # COMPARE WITH EACH OTHER AREA

  
  attach(DF.x)
  Pxx <- sum(exp(-d) * x1 * x2/X)
  detach(DF.x)
  
  attach(DF.y)
  Pyy <- sum(exp(-d) * x1 * x2 / X)
  detach(DF.y)
  
  attach(DF.t) 
  Ptt <- sum(exp(-d) * x1 * x2 / X)
  detach(DF.t)
  
  output <- (X*Pxx + Y*Pyy) / (Ttl * Ptt)
  
  return(output)
}


RCL <- function(x_matrix_counts, y_matrix_counts){
  
  if(!is.matrix(x_matrix_counts)){
    stop("A matrix has not been passed")
  }
  if(!is.matrix(y_matrix_counts)){
    stop("A matrix has not been passed")
  }
  
  
  
  if (!all(dim(x_matrix_counts)==(dim(y_matrix_counts)))){
    stop("are not correct")
  }
  
  if (any(x_matrix_counts < 0)){
    stop("negative cell counts passed")
  }
  
  
  n1 <- dim(x_matrix_counts)[1]
  n2 <- dim(x_matrix_counts)[2]
  x <- x_matrix_counts
  X <- sum(x)
  y <- y_matrix_counts
  Y <- sum(y)
  
  
  DF.x <- MakeDf(X)
  DF.y <- MakeDF(Y)
  
  
  attach(DF.x)
  Pxx <- sum(exp(-d) * x1 * x2/X)
  detach(DF.x)
  
  attach(DF.y)
  Pyy <- sum(exp(-d) * x1 * x2 / X)
  detach(DF.y)
  
 
  output <- Pxx/Pyy - 1
  
  return(output)
}
  

