###---------Ingredients of ggplot Package--------------


## Load the ggplot2 package using install.packages("ggplot2")


install.packages("ggplot2")

library(ggplot2)

# After installation completes,type library(ggplot2). 
# This loads the package into your R environment and makes it ready for use. 
# You can type search() to see which packages are ready for use in your environment, 
# ggplot2 should appear there now

ggplot(data=mtcars,aes(x=mtcars$wt,y=mtcars$mpg,col=factor(mtcars$cyl)))+geom_point()
ggplot(data=mtcars,aes(x=mtcars$wt,y=mtcars$mpg,col=mtcars$cyl))+geom_point()

ggplot(data=mtcars,aes(x=factor(mtcars$wt),y=mtcars$mpg, size=factor(mtcars$disp),col=factor(mtcars$hp)))+geom_point()

?ggplot
