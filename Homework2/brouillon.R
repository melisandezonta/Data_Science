library(ggplot2)
data(midwest)
library(GGally)

# Handling datas ------------------------------------------------

names(midwest)
summary(midwest)

# Relationship description between states and percentage of employments --------------------


ILLINOIS = subset(midwest, midwest$state == 'IL')
OHIO = subset(midwest, midwest$state == 'OH')
MICHIGAN = subset(midwest, midwest$state == 'MI')
INDIANA = subset(midwest, midwest$state == 'IN')
WISCONSIN = subset(midwest, midwest$state == 'WI')


perctotprof_IL = ILLINOIS$percprof * ILLINOIS$popadults/(sum(ILLINOIS$popadults))
perctotprof_OH = OHIO$percprof * OHIO$popadults/(sum(OHIO$popadults))
perctotprof_MI = MICHIGAN$percprof * MICHIGAN$popadults/(sum(MICHIGAN$popadults))
perctotprof_IN = INDIANA$percprof * INDIANA$popadults/(sum(INDIANA$popadults))
perctotprof_WI = WISCONSIN$percprof * WISCONSIN$popadults/(sum(WISCONSIN$popadults))

perctotprof_IL_1 = sum(ILLINOIS$percprof * ILLINOIS$popadults)/(sum(ILLINOIS$popadults))
perctotprof_OH_1 = sum(OHIO$percprof * OHIO$popadults)/(sum(OHIO$popadults))
perctotprof_MI_1 = sum(MICHIGAN$percprof * MICHIGAN$popadults)/(sum(MICHIGAN$popadults))
perctotprof_IN_1 = sum(INDIANA$percprof * INDIANA$popadults)/(sum(INDIANA$popadults))
perctotprof_WI_1 = sum(WISCONSIN$percprof * WISCONSIN$popadults)/(sum(WISCONSIN$popadults))

a = c('IL','OH','MI','IN','WI')
arr = c(perctotprof_IL_1,perctotprof_OH_1,perctotprof_MI_1,perctotprof_IN_1,perctotprof_WI_1)
dataframe = data.frame(states = a,percprof.per.state = arr)

qplot(x = states,
      y = percprof.per.state,
      data = dataframe,
      main = "Percprof vs State",)
ggsave('plot_question1.png',width = 5,height = 5,path = "/Users/jacquelineroudes/Documents/GTL_courses/Data_Visual_Analytics/Assignments/Homework2")



pertotprof_IL.median = median(perctotprof_IL)
pertotprof_OH.median = median(perctotprof_OH)
pertotprof_MI.median = median(perctotprof_MI)
pertotprof_IN.median = median(perctotprof_IN)
pertotprof_WI.median = median(perctotprof_WI)


list1 = rep(c('IL'),times = dim(ILLINOIS)[1])
d1 = data.frame(x=list1,y=c(ILLINOIS$percprof,ILLINOIS$popadults))


list2 = rep(c('OH'),times = dim(OHIO)[1])
d2 = data.frame(x=list2,y=c(OHIO$percprof,OHIO$popadults))


list3 = rep(c('MI'),times = dim(MICHIGAN)[1])
d3 = data.frame(x=list3,y=c(MICHIGAN$percprof,MICHIGAN$popadults))


list4 = rep(c('IN'),times =dim(INDIANA)[1])
d4 = data.frame(x=list4,y=c(INDIANA$percprof,INDIANA$popadults))


list5 = rep(c('WI'),times = dim(WISCONSIN)[1])
d5 = data.frame(x=list5,y=c(WISCONSIN$percprof,WISCONSIN$popadults))

data <- rbind(d1,d2,d3,d4,d5)

p <- ggplot(midwest, aes(reorder(state, -percprof, mean), percprof))
p1 <- p + geom_point(position = position_jitter(width = 0.1),aes(size=popadults), alpha = 0.3)
p1 + geom_boxplot(outlier.colour = NA, color = 'red') +
  coord_flip() +
  scale_x_discrete("state")

# Relationship description between states, percentage of people with a high school diploma and the percentage of college educated population--------------------

perchsd_IL = log10(ILLINOIS$perchsd * ILLINOIS$popadults/(sum(ILLINOIS$popadults)))
perchsd_OH = log10(OHIO$perchsd * OHIO$popadults/(sum(OHIO$popadults)))
perchsd_MI = log10(MICHIGAN$perchsd * MICHIGAN$popadults/(sum(MICHIGAN$popadults)))
perchsd_IN = log10(INDIANA$perchsd * INDIANA$popadults/(sum(INDIANA$popadults)))
perchsd_WI = log10(WISCONSIN$perchsd * WISCONSIN$popadults/(sum(WISCONSIN$popadults)))

percollege_IL = log10(ILLINOIS$percollege * ILLINOIS$popadults/(sum(ILLINOIS$popadults)))
percollege_OH = log10(OHIO$percollege * OHIO$popadults/(sum(OHIO$popadults)))
percollege_MI = log10(MICHIGAN$percollege * MICHIGAN$popadults/(sum(MICHIGAN$popadults)))
percollege_IN = log10(INDIANA$percollege * INDIANA$popadults/(sum(INDIANA$popadults)))
percollege_WI = log10(WISCONSIN$percollege * WISCONSIN$popadults/(sum(WISCONSIN$popadults)))

data2 = data.frame(midwest$state,c(perchsd_IL,perchsd_OH,perchsd_MI,perchsd_IN,perchsd_WI),
                   c(percollege_IL,perchsd_OH,perchsd_MI,perchsd_IN,perchsd_WI))
ggpairs(data2, 1:3, lower = list(combo = wrap("facethist", binwidth = 0.5)))


# Random Scatterplot-----------------------------------------------------------------

pdf = c()
ps = c()
png = c()
jpg = c()
N = 100000
for (i in seq(1,N, by = 500)){
  X = runif(i, min = 0, max = 1)
  Y = runif(i, min = 0, max = 1)
  qplot(x = X,
        y = Y,
        main = "X vs Y")
  ggsave("last_plot.ps",width = 5,height = 5)
  s_ps = file.info('last_plot.ps')
  ps = c(ps,s_ps[1])
  ggsave("last_plot.png",width = 5,height = 5)
  s_png = file.info('last_plot.png')
  png = c(png,s_png[1])
  ggsave("last_plot.pdf",width = 5,height = 5)
  s_pdf = file.info('last_plot.pdf')
  pdf = c(pdf,s_pdf[1])
  ggsave("last_plot.jpeg",width = 5,height = 5)
  s_jpeg = file.info('last_plot.jpeg')
  jpg = c(jpg,s_jpeg[1])
}

# Plot -------------------------------

x_grid = seq(1,N, by = 500)
R = stack(list('format = ps' = ps,
               'format = png' = png,
               'format = pdf' = pdf,
               'format = jpeg' = jpg))
names(R) = c("y", "format");
R$x = x_grid
qplot(x,
      y,
      color = format,
      lty = format,
      geom = "line",
      data = R,
      main = "File size for different formats",
      xlab = "N",
      ylab = "Size in bytes")


# Relationship inside Diamonds dataset-----------------------

data("diamonds")

ggplot(diamonds ,aes(x = color)) +
  geom_bar() +
  ggtitle(label = "Bar chart of color in diamonds dataset")
ggsave("bar_chart_color.png",width = 5,height = 5,path = 
         "/Users/jacquelineroudes/Documents/GTL_courses/Data_Visual_Analytics/Assignments/Homework2")

ggplot(diamonds ,aes(x = carat)) +
  geom_histogram(binwidth = .2) +
  ggtitle(label = "Histogram of carat in diamonds dataset")
ggsave("histogram_carat.png",width = 5,height = 5,path = 
         "/Users/jacquelineroudes/Documents/GTL_courses/Data_Visual_Analytics/Assignments/Homework2")

ggplot(diamonds, aes(x = price, y = ..density..)) +
  geom_histogram(binwidth = 1000) +
  geom_density(size = .5, color = "red")+
  ggtitle(label = "Histogram of price in diamonds dataset")
ggsave("histogram_price.png",width = 5,height = 5,path = 
         "/Users/jacquelineroudes/Documents/GTL_courses/Data_Visual_Analytics/Assignments/Homework2")

# Pairplot ----------------

data = data.frame(diamonds$color,diamonds$price,diamonds$carat)
ggpairs(data)
ggsave("pairplot.png",width = 30,height = 30,path = 
         "/Users/jacquelineroudes/Documents/GTL_courses/Data_Visual_Analytics/Assignments/Homework2")

qplot(x = carat,
      y = price,
      data = diamonds,
      color = color,
      main = "Price by Carat")

ggsave("pricebycarat.png",width = 5,height = 5,path = 
         "/Users/jacquelineroudes/Documents/GTL_courses/Data_Visual_Analytics/Assignments/Homework2")



