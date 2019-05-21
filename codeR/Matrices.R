# Matrices
mnist <- read_mnist()
class(mnist$train$images)
x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]
length(x[,1])
dim(x)
dim(as.matrix(x[,3]))
my_vector <- 1:15
my_mat <- matrix(my_vector,3,5)
my_mat

my_mat <- matrix(my_vector,3,5,byrow = TRUE)
my_mat
grid <- matrix(x[3,] , 28, 28)
image(1:28, 1:28, grid)
image(1:28, 1:28, grid[,28:1])
sums <- rowSums(x)
avg <- rowMeans(x)

install.packages("matrixStats")
library(matrixStats)
sds <- colSds(x)
new_x <- x[,colSds(x) > 60]
dim(new_x)

class(x[,1,drop = FALSE])
dim(x[,1,drop = FALSE])

qplot(as.vector(x), bins = 30, color = I("black"))
new_x <- x
new_x[new_x < 50] <- 0

bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
grid <- matrix(bin_x[4,] , 28, 28)
image(1:28,1:28, grid[,28:1] )

#Scale each row 
(x - rowMeans(x))/rowSds(x)
#Scale each colum
t(t(x) - colMeans(x))

x_mean_0 <- sweep(x,2,colMeans(x))
x_standardized <- sweep(x_mean_0,2,colSds(x), FUN = "/")  

