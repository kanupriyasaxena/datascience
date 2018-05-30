
##-------------Bar Charts in ggplot--------------------

# setting the ggplot object
cyl.am<-ggplot(mtcars,aes(x=factor(cyl)))
cyl.am+geom_bar(col="red")
# The base layer is available : cyl.am
# Add geom (position = "stack" by default)

cyl.am<-cyl.am+aes(fill=factor(am))
cyl.am+geom_bar()

# Fill - show proportion
cyl.am+geom_bar(position = "fill")

ggplot(mtcars,aes(x=factor(cyl),fill=factor(am)))+geom_bar(alpha=0.4, position = position_dodge(0.2))