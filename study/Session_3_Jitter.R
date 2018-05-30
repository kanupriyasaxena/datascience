##Jitter 



# Plot the cyl on the x-axis and wt on the y-axis
# Note that points on 3 vertical lines are not easy to read

ggplot(mtcars,aes(x=cyl,y=wt))+geom_point()




# Use geom_jitter() instead of geom_point(); Jitter is a type of point plot used to avoid overplotting,
# especially for categorical variables like cyl. Jitter can be written as a position in the geom_point layer,
# which means the code below is same as geom_point(position = "jitter") 
jitter_posn<-position_jitter(0.5)
ggplot(mtcars,aes(x=cyl,y=wt))+geom_jitter(position=jitter_posn)


sunflowerplot(iris$Petal.Length, iris$Petal.Width, col = iris$Species,size = 0.6)

ggplot(iris,aes(x=iris$Petal.Length,y=iris$Petal.Width))+geom_point()

?sunflowerplot
# Note that jitter plot scatters the points a little too much




# Let's avoid the random scatter made by jitter using a width argument in jitter
# You can define a position object and put it inside 
# Define a position object using position_jitter(): 



# Use jitter_posn object inside the geom_point in geom_point() or geom_jitter
# Both commands below are exactly the same







