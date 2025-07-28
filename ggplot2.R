my_df <- read.csv("Christmas Tree.csv")#reading csv
my_df <- subset(my_df, select = -c(1))#deleating empty column

my_df$Number.of.trees.sold.Million <- my_df$Number.of.trees.sold/1000000
my_df$Sales.Million <- my_df$Sales/1000000
#adding new variables to df for better visualization 

str(my_df)#summary of dataset

realtree <- subset(my_df, subset = (my_df$Type.of.tree == "Real tree"))
faketree <- subset(my_df, subset = (my_df$Type.of.tree == "Fake tree"))
#splitting dataframe into 2 dataframes


library(ggplot2) #downloading library

?qplot

qplot(x = Number.of.trees.sold.Million, y = Year, data = my_df) #auto plotting 

qplot(x = Number.of.trees.sold.Million, y = Sales.Million, data = my_df,
      color = Type.of.tree, size = Year) #auto plotting

v <- my_df$Number.of.trees.sold.Million

qplot(v)
#we can build a quick plot for a vector

my_qplot <- qplot(x = Number.of.trees.sold.Million, y = Sales.Million, 
                  data = my_df,
                  color = Type.of.tree, size = Year)
my_qplot
#we can save the graphic into a variable

str(my_qplot)#information of 'my_qplot' variable, as we can see all settings
#required for building a plot are settled.

?ggplot()
ggplot(my_df) #a blank has been created, the canvas 
#is at stage 2 'geometric objects'
ggplot(my_df, aes(x = Number.of.trees.sold.Million)) 
#an aesthetics has been created, a preparation
ggplot(my_df, aes(x = Number.of.trees.sold.Million))+
  geom_histogram()+
  facet_wrap(~Type.of.tree)
#unlike qplot we more clearly attribute all stages
#of creating a graph

#aes mapping
ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price))+
  geom_point()+
  labs(title = "No colour mapping") #no color mapping

ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Type.of.tree))+
  geom_point()+
  labs(title = "With colour mapping") #color mapping

ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Year,
           shape = Type.of.tree))+
  geom_point()+
  scale_colour_gradient(low = "deeppink", high = "cyan1") +
  labs(title = "By color mapping year, by shape type of tree")

ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Year,
           shape = Type.of.tree,
           size = I(5)))+
  geom_point()+
  scale_colour_gradient(low = "deeppink", high = "cyan1") +
  labs(title = "With aes we can change size")

ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Year,
           shape = Type.of.tree))+
  geom_point(aes(size = I(5)))+
  geom_line()+
  scale_colour_gradient(low = "deeppink", high = "cyan1") +
  labs(title = '"Size" settled by aes just for points')

ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Year,
           shape = Type.of.tree,
           size = I(5)))+
  geom_point()+
  geom_line()+
  scale_colour_gradient(low = "deeppink", high = "cyan1") +
  labs(title = '"Size" settled by aes for whole plot')




ggplot(my_df, aes(x = Number.of.trees.sold.Million, y = Average.Tree.Price))+
  geom_point()+
  geom_smooth()
#built a graph with points and smoothing on one graph

ggplot(my_df, aes(x = Average.Tree.Price, y = Sales.Million, col = Type.of.tree))+
  geom_line(size = 1)+
  geom_point(color = 'black')
#built a line graph with black points


ggplot(subset(my_df, my_df$Year == '2016' | my_df$Year == '2015' | my_df$Year == '2014'),
       aes(x =  Average.Tree.Price,
                  y = Number.of.trees.sold.Million,
                  col = Type.of.tree))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = mean(my_df$Average.Tree.Price))
#built a line graph with points and vline for 3 years

#changing color with scale
ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Type.of.tree))+
  geom_point()+
  scale_colour_manual(
    values = c("deeppink",
               "cyan1")) +
  labs(title = "Colors settled manually by scales") 
#colors are not settled by default, but manually

ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Type.of.tree))+
  geom_point()+
  theme_minimal()+
  labs(title = "Theme is minimal")
#setting theme to change background

ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Year,
           shape = Type.of.tree))+
  geom_point(aes(size = I(5)))+
  geom_line()+
  scale_colour_gradient(low = "deeppink", high = "cyan1") +
  theme_minimal()+
  labs(title = 'Theme is minimal')
#setting theme to change background

theme_set(theme_minimal()) #setting theme to all 
#future plots

ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Year,
           shape = Type.of.tree))+
  geom_point(aes(size = I(5)))+
  geom_line()+
  scale_colour_gradient(low = "deeppink", high = "cyan1") +
  theme(axis.text = element_text(colour = "pink"),
        axis.title.x = element_text(colour = "pink") ,
        axis.title.y = element_text(colour = "pink"),
        legend.title = element_text(colour = "pink"),
        legend.text = element_text(colour = "pink"),
        plot.title = element_text(colour = "pink"))+
  xlab("Number of trees sold (million)")+
  ylab("Average tree price")+
  labs(title = 'With theme changhed color of text')

#geoms

ggplot(data = my_df, 
       aes(x = Average.Tree.Price))+
  geom_histogram()+
  xlab("Average tree price")+
  labs(title = "Histogram") #Histogram

ggplot(data = my_df,
       aes(x = Average.Tree.Price,
           y = Number.of.trees.sold.Million))+
  geom_point()+
  xlab("Average tree price")+
  ylab("number of trees sold (million)")+
  labs(title = "Scatterplot")

ggplot(data = my_df,
       aes(x = Type.of.tree,
           y = Sales.Million))+
  geom_boxplot()+
  labs(title = "Boxplot")+
  xlab("type of tree")+
  ylab("sales (million)")+
  labs(title = "Scatterplot")

#stat
ggplot(data = my_df, 
       aes(x = Average.Tree.Price))+
  stat_density(aes(alpha = 0.7))+
  xlab("Average tree price")+
  labs(title = "Density")

?stat_density

#plotly
library(plotly)

bb <- ggplot(my_df, 
       aes(x = Number.of.trees.sold.Million, 
           y = Average.Tree.Price,
           col = Year,
           shape = Type.of.tree))+
  geom_point(aes(size = I(5)))+
  geom_line()+
  scale_colour_gradient(low = "deeppink", high = "cyan1") +
  theme(axis.text = element_text(colour = "pink"),
        axis.title.x = element_text(colour = "pink") ,
        axis.title.y = element_text(colour = "pink"),
        legend.title = element_text(colour = "pink"),
        legend.text = element_text(colour = "pink"),
        plot.title = element_text(colour = "pink"))+
  xlab("Number of trees sold (million)")+
  ylab("Average tree price")

bb
?ggplotly

ggplotly(bb)

