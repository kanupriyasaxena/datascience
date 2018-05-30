## -------------ggplot - Histogram and More on Bar Charts---------------

# Let's learn some more bars. Histograms are also types of bars, we just make them using geom_histogram.



# BARS: Bars are made using geom_bar, which you would have plotted in the previous assignment
# Bars take position arguments like stack, dodge, fill etc. which changes the appearance of bars drastically 
# We'll use the three most common position arguments: "stack", "fill" and "dodge"




# Draw a bar plot of cyl, filled according to am
# geom_bar has no argument, whcih makes it same as geom_bar(position = "stack")
ggplot(mtcars,aes(x=mtcars$cyl,fill=factor(am)))+geom_bar()





# Change the position argument to stack. It will be exactly same as previous plot 
ggplot(mtcars,aes(x=mtcars$cyl,fill=factor(am)))+geom_bar(position="stack")




# Same mistake of forgetting factor again? Note that fill is not mapped to am; add factor around am






# Change the position argument to "fill" - It covers the entire length of y axis 

ggplot(mtcars,aes(x=mtcars$cyl,fill=factor(am)))+geom_bar(position="fill")





# Change the position argument to "dodge"
# Dodge makes the bars appear beside each other, instead "stacking" over one another
# This makes it easy to compare y values of both colors (am variable) 

p<-ggplot(mtcars,aes(x=mtcars$cyl,fill=factor(am)))+geom_bar(position="dodge")
p+theme_economist()





##  -----------------Frequency Polygons---------------------------

# Frequency polygons are a variation of histograms which make the plot continuous
# Frequency Polygon also plots count on y axis (no of observations)
# Histograms use Bars, frequency polygons use Lines

ggplot(mtcars, aes(x=mpg, fill=factor(cyl))) + geom_histogram(binwidth =1)


ggplot(mtcars, aes(x=mpg, col=cyl)) + geom_freqpoly(binwidth =1)


# Make a basic histogram, add coloring defined by col = cyl and mpg on x-axis... 
# Oh wait, factor(cyl).




ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth =2)
myBlue<-"blue"

ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth =2,fill=myBlue)

# Change position to identity, keep using binwidth = 1






# Make a frequency polygon using freqpoly: Change geom to freqpoly (position is identity by default) 








