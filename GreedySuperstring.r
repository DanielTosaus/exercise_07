
find_overlap <- function(A, B){
  
  # Find the first letter of B in A (in none there is no overlap)
  # From that letter in index ind advance comparing A[ind+1] and B[j+1]
  # If they match till the end of A we have a overlap of length(A)-ind
  
  ind <- which(A==B[1])
  if(length(ind)==0){
    
    return(0)
  }
  
  for(i in ind){
    aux <- i
    for(j in (2:length(B))){
      aux <- aux + 1
      
      # If we reach the end of A we already have an overlap value
      if(aux > length(A)){
        
        return((length(A)-i)+1)
      }
      else if (A[aux] != B[j]){
            # If the end of A and begging of B dont match we take the next
            # value in ind (possible start of overlap)
            break
            
      }
    }
  }
  
  return(0)
}

A <- c('C', 'G', 'T', 'G', 'G', 'C')
B <- c('G', 'C', 'T', 'A')
overlap <- find_overlap(A,B)

OverlapMatrix <- function(S){
  if(length(S) < 2){
    return(0)
  }
  LS <- length(S)
  M <- matrix(nrow=LS , ncol= LS)
  for(i in (1:LS)){
    for(j in (1:LS)){
      if(i!=j){
        
        M[i,j] <- find_overlap(S[[i]],S[[j]])
        
      }else{
        M[i,j] <- 0
      }
    }
  }
  return(M)
}

S <- list(c('C','A','T','G','C'), c('C','T','A','A','G','T'),c('G','C','T','A'),c('T','T','C','A'),c('A','T','G','C','A','T','C'))
M <- OverlapMatrix(S)


GreedySuperstring <- function(S){
  
  
  while(length(S)>1){
    
    M <- OverlapMatrix(S)
    max_overlap <- max(M)[1]
    ij_max <- which(M == max(M)[1], arr.ind = TRUE)
    #print(M)
    A <- S[[ij_max[1]]]
    B <- S[[ij_max[2]]]
    # Merge
    AB <- c(A, B[(max_overlap+1):length(B)])
    
    #Eliminate A and B from S
    S <- S[-c(ij_max[1], ij_max[2])]
    # Add merged string to S
    S <- c(list(AB),S)
    
  }
  print(paste("GreedySuperstring: ", paste(S[[1]], collapse = " ")))
  return(S[[1]])
  
}

GSS <- GreedySuperstring(S)
