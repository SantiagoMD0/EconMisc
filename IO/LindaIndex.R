#####
##
#
##
#
##
rm(list = ls())

##' @name Linda_Index.fun
##' @description This is a function that computes the 
##' Index System Linda (1976)..  
##' @param df It's the data.frame with two variables, at least.
##' (1) The firm identifier.
##' (2) The variable used to compute the market-shares.
##' @return A Three elements list:
##' (1) Linda_data: Table with the following variables:
##'     n_star = n*'s label.
##'     Linda_Index = Linda Index for each n*
##'     PL = Perfect Equilibrium - n*
##'     n_star_numer = n*
##'     Linda_s = Arithmetic Mean of Linda_Index (based on n*_m).
##' (2) n_star_min: This contains two elements: the minimum Linda Index
##' and the minimum n* that minimises the Linda Linda Index.
##' (3) n_star_max: This contains two elements: the maximum Linda Index
##' and the minimum n* that maximises the Linda Linda Index.
##' 

df <- data.frame(id = paste0("id",1:20),
                 volume = runif(n = 20,100,1000) )

# var.ms = "volume"

Linda_Index.fun <- function(df, var.ms = "volume"){
  
  if( nrow(df) == 0) stop("Provide a valid input data")
  
  df <-  df[order(df[[var.ms]], decreasing = T),]
  
  N <- nrow(df)
  
  L.l <- list()
  
  for(i in 2:N){
    
    subdf <- df[1:i,]
    
    subdf$ms <- subdf[[var.ms]] / sum(subdf[[var.ms]])
    
    subdf$Ai <- cumsum(subdf$ms)
    
    subdf$difA <- max(subdf$Ai) - subdf$Ai
    
    subdf$A <- subdf$Ai / subdf$difA
    
    subdf$i <- 1:i
    
    subdf$difni <- (max(subdf$i) - subdf$i) / subdf$i
    
    subdf$EOi_n <- (subdf$A * subdf$difni)/max(subdf$i)
    
    subdf <- subdf[1:(i-1),]
    
    L.l[[(i-1)]] <- sum(subdf$EOi_n) / max(subdf$i)
  }
  
  names(L.l) <- paste0("n=",2:N)
  
  L.df <- data.frame(
    
    cbind(paste0("n = ", 2:N),
          do.call("rbind",L.l))
  )
  
  colnames(L.df) <- 
    c("n_star","Linda_Index")
  
  L.df$PL <- 1/(2:N)
  
  L.df$Linda_Index <- as.numeric(L.df$Linda_Index)
  
  tmp.pos.min <- which.min(L.df$Linda_Index)
  
  n_star_m <- L.df[tmp.pos.min,]$n_star
  
  L_n_star_m <- L.df[tmp.pos.min,]$Linda_Index
  
  tmp.pos.max <- which.max(L.df[1:tmp.pos.min,]$Linda_Index)
  
  if(!(tmp.pos.max %in% 1:tmp.pos.min)) stop("Order data")
  
  n_star_h <- L.df[tmp.pos.max,]$n_star
  
  L_n_star_h <- L.df[tmp.pos.max,]$Linda_Index
  
  L.df$n_star_number <- as.numeric(
    gsub(pattern = "n = ",replacement = "", L.df$n_star)
  )
  
  L_s <- sum( L.df[ 1:tmp.pos.min, ]$Linda_Index ) / 
    ( L.df[ tmp.pos.min, ]$n_star_number - 1)
  
  L.df$Linda_s <- L_s
  
  rownames(L.df) <- NULL
  
  out <- list(
    
    Linda_data = L.df,
    
    n_star_min = c("n_star_m_label" = n_star_m,
                   "n_star_m" = as.numeric(
                     gsub(pattern = "n = ",replacement = "", n_star_m)),
                   "L_n_star_m" = L_n_star_m),
    
    n_star_max = c("n_star_h_label" = n_star_h,
                   "n_star_h" = as.numeric(
                     gsub(pattern = "n = ",replacement = "", n_star_h)),
                   "L_n_star_h" = L_n_star_h),
    
    Linda_s = L_s
  )
}


# Testing

out <- Linda_Index.fun(df, var.ms = "volume")















