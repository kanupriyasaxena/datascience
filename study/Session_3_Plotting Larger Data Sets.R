##------------Plotting Larger Data Sets----------
installed.packages()
library(ggplot2)

# See structure of diamonds data frame
str(diamonds)



# Plot carat on x and price on y axis; use geom_point
# This is a plot with over 50,000 data points
  ggplot(data=diamonds,aes(x=carat,y=price))+geom_point()



# Add geom_smooth() after the geom_point() to fit a smooth line, just use a + sign
# Note that + sign simply adds a geometric layer geom_smooth over geom_point
# You can add as many geometric layers as you wish!  

  ggplot(data=diamonds,aes(x=carat,y=price))+geom_point()+geom_smooth()



# Show only the smooth line: Remove the geom_point layer


  ggplot(data=diamonds,aes(x=carat,y=price))+geom_smooth()

# Map col to clarity in the aes() of ggplot
# Diamonds come in different clarity levels, each having its own pricing methodology
# Can you see which clarity level is the cheapest? 

  ggplot(data=diamonds,aes(x=carat,y=price,col=clarity))+geom_smooth()


# Keep the color settings as they are (col mapped to clarity) 
# Plot using geom_point instead of geom_smooth and use alpha = 0.4 inside geom_point
# Alpha is a measure of transparency and is useful to plot large data sets neatly 
# (somewhat neatly)

  ggplot(data=diamonds,aes(x=carat,y=price,col=clarity))+geom_point(alpha=0.4)

# Try changing alpha from 0.9 to 0.1 (it's a fraction)


  ggplot(data=diamonds,aes(x=carat,y=price,col=clarity))+geom_point(alpha=0.1)

## Visual as Objects:
  dia_plot <- ggplot(diamonds, aes(x = carat, y = price))



# Expand dia_plot by adding geom_point() with alpha set to 0.2
dia_plot <- dia_plot + geom_point(alpha = 0.2)



dia.object <- ggplot(diamonds, aes(x = clarity, y = carat, col = price))

dia.object+geom_point()

dia.object+ stat_sum()
