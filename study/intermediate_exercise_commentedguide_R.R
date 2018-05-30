# Look at structure of mtcars
str(mtcars)

# Store mtcars in a new dataframe called cars. 
cars <- mtcars

# Calculate each car's score in a new column named score 
cars$score = cars$mpg*cars$hp / cars$wt


# Store the scores in a vector s
s = c(cars$score)

# Create a new vector named performance equal to the length of the vector s

l = length(s)
performance <- vector(length = 32)
# http://stackoverflow.com/questions/22104774/how-to-initialize-a-vector-with-fixed-length-in-r


# If score < mean(score), performance is 'average', else performance is 'good'
for(i in 1:32)
{
if(s[i] < mean(cars$score))
{
  performance[i] <- "average"
 
}
else
{
  performance[i] <- "good"
}
}



# Add the performance vector as a column to cars

cars$performance = performance


# Convert the performance variable to factor type
as.factor(cars$performance)

# Summarise the cars df
summary(cars)

frequency(cars$performance)

data.frame(sapply(cars, factor))
summary(cars)

length(which(cars$performance == "good"))


x <- c("I", "eat", "apple")


x <- c("I eat apple")

x<-c("2", "3", "4")
y<-c(3, 4, 5)

matrix(1:6, 2, 3)

matrix(1:6, 2, ,FALSE)

x <- 8
f <- function() {
  y <- x+2
  return(c(x, y))
}
f()

f <- function() {
  a <- 8
  b <- a+2
  return(c(a, b))
}
b

function_math <- function(x, y){
  z <- x + y
  p <- x * y
  q <- z / p
  return(c(z, p, q))
}
alpha <- function_math(2, 3)
alpha[2]


A <- c(5,4,1,3,8,1)
sum(which(A == 1))

sum(which(A = 1))
length(which(A == 1))

mtcars[2:3, ]

mtcars[c(2,3), ]

mtcars[seq(2,3), ]

mtcars[seq(2,3,1), ]

