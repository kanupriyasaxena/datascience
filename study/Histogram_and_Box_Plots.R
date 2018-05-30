
##-------------Histogram and Box Plots Using Base Package-------------


# Plot histogram of Sepal.Width using hist(); 

hist(iris$Sepal.Width)


# type ?hist in R console and read up more about histograms

?hist


# Plot a histogram for Petal width and compare it with the plot above

hist(iris$Petal.Width)


# Make a boxplot of Petal.Length; read up on boxplots also. 


boxplot(iris$Petal.Length)


str(InsectSprays)

View(InsectSprays)

boxplot(InsectSprays$count~InsectSprays$spray)

?boxplot

plot(iris$Species,iris$Sepal.Length)
