# # https://es.wikipedia.org/wiki/Producto_escalar

x <- c(2,1)
y <- c(1,2)

# Idea:
plot(x, y, xlim = c(-2,2), ylim = c(-2,2))
abline(h=0, lty=3)
abline(v=0, lty=3)
arrows(0,0,2,1, col="blue")
arrows(0,0,1,2, col="red")


# Producto escalar

(producto_scalar <- 2*1 + 1*2)

(producto_scalar <- x %*% y)


# Producto  escalar  o producto punto: x·y = ||x|| ||y|| cos Φ

#  Norma de un vector de un vector que va de c(0,0) c(2,1)
norma_vector_x <- 

norma_vector_y <- 

cos_theta <- 

theta <- 

  
  
  
  
  
  

# Aplicación --------------------------------------------------------------


sentence_m <- "Mason really loves food"
sentence_h <- "Hannah loves food too"
sentence_w <- "The whale is food"


sentence_m = c("Mason" = 1, "really" = 1, "loves" = 1, "food" = 1, "too" = 0, "Hannah"= 0, "The" = 0, "whale" = 0,"is"= 0) 
sentence_h = c("Mason" = 0, "really" = 0, "loves" = 1, "food" = 1, "too" = 1, "Hannah"= 1, "The" = 0, "whale" = 0,"is"= 0) 
sentence_w = c("Mason" = 0, "really" = 0, "loves" = 0, "food" = 1, "too" = 0, "Hannah"= 0, "The" = 1, "whale" = 1,"is"= 1) 


