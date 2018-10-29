##GT account name : mzr3
# -------------------------------------------------------------
library(ggplot2)
data(midwest)
library(GGally)

# Handling datas ------------------------------------------------

names(midwest)
summary(midwest)

# Question 1 : Relationship description between states and percentage of employments --------------------

# Creation of subset for each state
ILLINOIS = subset(midwest, midwest$state == 'IL')
OHIO = subset(midwest, midwest$state == 'OH')
MICHIGAN = subset(midwest, midwest$state == 'MI')
INDIANA = subset(midwest, midwest$state == 'IN')
WISCONSIN = subset(midwest, midwest$state == 'WI')

#Compute the formula allowing to normalize according to state
perctotprof_IL_1 = sum(ILLINOIS$percprof * ILLINOIS$popadults)/(sum(ILLINOIS$popadults))
perctotprof_OH_1 = sum(OHIO$percprof * OHIO$popadults)/(sum(OHIO$popadults))
perctotprof_MI_1 = sum(MICHIGAN$percprof * MICHIGAN$popadults)/(sum(MICHIGAN$popadults))
perctotprof_IN_1 = sum(INDIANA$percprof * INDIANA$popadults)/(sum(INDIANA$popadults))
perctotprof_WI_1 = sum(WISCONSIN$percprof * WISCONSIN$popadults)/(sum(WISCONSIN$popadults))

#Plotting the result
a = c('IL','OH','MI','IN','WI')
arr = c(perctotprof_IL_1,perctotprof_OH_1,perctotprof_MI_1,perctotprof_IN_1,perctotprof_WI_1)
dataframe = data.frame(states = a,percprof.per.state = arr)

qplot(x = states,
      y = percprof.per.state,
      data = dataframe,
      main = "Percprof vs State")
ggsave('plot_question1.png',width = 5,height = 5)

# Display a boxplot and a scatter plot
p <- ggplot(midwest, aes(reorder(state, -percprof, mean), percprof))
p1 <- p + geom_point(position = position_jitter(width = 0.1),aes(size=popadults), alpha = 0.3)
p1 + geom_boxplot(outlier.colour = NA, color = 'red') +
    coord_flip() +
    scale_x_discrete("state")

#Question 2 :  Relationship description between states, percentage of people with a high school diploma and the percentage of college educated population--------------------
#Display a pair wise plot with GGpais function from GGaly library
data2 = data.frame(midwest$state,
                   c(ILLINOIS$perchsd,OHIO$perchsd,MICHIGAN$perchsd,
                                   INDIANA$perchsd,WISCONSIN$perchsd),
                   c(ILLINOIS$percollege,OHIO$percollege,MICHIGAN$percollege,
                     INDIANA$percollege,WISCONSIN$percollege))
ggpairs(data2)
ggsave('pairplot_question2.png',width = 20,height = 20)

# Question 4 : Random Scatterplot-----------------------------------------------------------------

# Save the size of each file after the genration of the image with i random points
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

# Question 4 :  Plot the results -------------------------------

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


# Question 5 : Relationship inside Diamonds dataset-----------------------

data("diamonds")
# Bar Chart for color
ggplot(diamonds ,aes(x = color)) +
  geom_bar() +
  ggtitle(label = "Bar chart of color in diamonds dataset")
ggsave("bar_chart_color.png",width = 5,height = 5)
# Histogram for carat
ggplot(diamonds ,aes(x = carat)) +
  geom_histogram(binwidth = .2) +
  ggtitle(label = "Histogram of carat in diamonds dataset")
ggsave("histogram_carat.png",width = 5,height = 5)
# Smoothed Histogram for price
ggplot(diamonds, aes(x = price, y = ..density..)) +
  geom_histogram(binwidth = 1000) +
  geom_density(size = .5, color = "red")+
  ggtitle(label = "Histogram of price in diamonds dataset")
ggsave("histogram_price.png",width = 5,height = 5)

# Question 5 : Pairplot ----------------
# Display the three way relationship between the 3 variables
data = data.frame(diamonds$color,diamonds$price,diamonds$carat)
ggpairs(data)
ggsave("pairplot.png",width = 30,height = 30)

qplot(x = carat,
      y = price,
      data = diamonds,
      color = color,
      main = "Price by Carat")

ggsave("pricebycarat.png",width = 5,height = 5)

  
