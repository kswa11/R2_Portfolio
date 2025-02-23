# R2_Portfolio
This is my second repository for github. This is for my bioinformatics R2 class that I learned from Dr. Vandenbrink winter 2025.

# GGPlot
## Barplots

Now lets take a look at some ggplot2 barplots

We'll start with making a dataframe based on the tooth data

```{r}

df <- data.frame(dose = c("D0.5", "D1", "D2"),
                 len = c(4.2, 10, 29.5))

df

```

And now lets make a second dataframe

```{r}

df2 <- data.frame(supp=rep(c("VC", "OJ"), each = 3),
                  dose = rep(c("DO.5", "D1", "D2"), 2),
                  len = c(6.8, 15, 33, 4.2, 10, 29.5))
                  
df2

```

Lets load up ggplot2

```{r}

library(ggplot2)
```

Lets set our parameters for ggplot
```{r}
theme_set(
  theme_classic() +
    theme(legend.position = "top")
)
```

Lets start with some basic barplots using the tooth data

```{r}

f <- ggplot(df, aes(x =  dose, y = len))

f + geom_col()


```

Now lets change the fill, and add labels to the top

```{r}

f + geom_col(fill = "darkblue") + 
  geom_text(aes(label = len), vjust = -0.3)

```

Now lets add the labels inside the bars

```{r}

f + geom_col(fill = "darkblue") +
  geom_text(aes(label = len), vjust = 1.6, color = "white")

```

Now lets change the barplot colors by group

```{r}

f + geom_col(aes(color = dose), fill = "white") +
  scale_color_manual(values = c("blue", "gold", "red"))

```

This is kinda hard to see, so lets change the fill.

```{r}

f + geom_col(aes(fill = dose)) +
  scale_fill_manual(values = c("blue", "gold", "red"))

```

Ok how do we do this with multiple groups

```{r}

ggplot(df2, aes(x = dose, y = len)) +
  geom_col(aes(color = supp, fill = supp), position = position_stack()) +
  scale_color_manual(values = c("blue", "gold")) +
  scale_fill_manual(values = c("blue", "gold"))

```

```{r}

p <- ggplot(df2, aes(x = dose, y = len)) + 
  geom_col(aes(color = supp, fill = supp), position = position_dodge(0.8), width = 0.7) +
  scale_color_manual(values = c("blue", "gold")) +
  scale_fill_manual(values = c("blue", "gold"))
```

```{r}

p 

```

Now lets add those labels to the dodged barplots 

```{r}

p + geom_text(
  aes(label = len, group = supp),
  position = position_dodge(0.8), 
  vjust = -0.3, size = 3.5
) 

```

Now what if we want to add labels to our stacked barplots? For this we need dplyr

```{r}
library(dplyr)

df2 <- df2 %>%
  group_by(dose) %>%
  arrange(dose, desc(supp)) %>%
  mutate(lab_ypos = cumsum(len) - 0.5 * len)



```

```{r}
df2

```

Now lets recreate our stacked graphs

```{r}

ggplot(df2, aes(x = dose, y = len)) + 
  geom_col(aes(fill = supp), width = 0.7) +
  geom_text(aes(y = lab_ypos, label = len, group = supp), color = "white") +
  scale_color_manual(values = c("blue", "gold")) +
  scale_fill_manual(values = c("blue", "gold"))

```

## Boxplots

Lets look at some boxplots

```{r}

data("ToothGrowth")

```

Lets change the dose to a factor, and look at the top of the dataframe

```{r}

ToothGrowth$dose <- as.factor(ToothGrowth$dose)

head(ToothGrowth, 4)

```
Lets load ggplot

```{r}

library(ggplot2)

```

Lets set the theme for our plots to classic 

```{r}

theme_set(
  theme_bw() +
    theme(legend.position = "top")
) 

```

Lets start with a very basic boxplot with dose vs length

```{r}

tg <- ggplot(ToothGrowth, aes(x = dose, y = len))
tg + geom_boxplot()

```

Now lets look at a boxplot with points for the mean

```{r}
tg + geom_boxplot(notch = TRUE, fill = "lightgrey") + 
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 2.5, color = "indianred")

```

We can also change the scale number of variables included, and their order


```{r}

tg + geom_boxplot() +
  scale_x_discrete(limits = c("0.5", "2"))

```

```{r}

tg + geom_boxplot() + 
  scale_x_discrete(limits = c("2", "1", "0.5"))

```

Lets put our x axis in descending order

```{r}

tg +geom_boxplot() +
  scale_x_discrete(limits = c("2", "1", "0.5"))

```

We can also change boxplot colors by groups

```{r}

tg + geom_boxplot(aes(color = dose)) + 
  scale_color_manual(values = c("indianred", "blue", "green2"))

```


What if we want to display our data subset by oj vs vitamin c?

```{r}

tg2 <- tg + geom_boxplot(aes(fill = supp), position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#999999", "#E69F00"))

tg2

```

We can also arrange this as two plots with facet_wrap

```{r}

tg2 + facet_wrap(~supp)

```

## Histograms

```{r}
set.seed(1234)

wdata = data.frame(
  sex = factor(rep(c("F", "M"), each = 200)), 
  weight = c(rnorm(200, 56), rnorm(200, 58))
)

head(wdata, 4)

```

Now lets load dplyr

```{r}
library(dplyr)

mu <- wdata %>%
  group_by(sex) %>%
  summarise(grp.mean = mean(weight))

```

Now lets load the plotting package 

```{r}
library(ggplot2)

theme_set(
  theme_classic() +
    theme(legend.position = "bottom")
)
```

Now lets create a ggplot object

```{r}
a <- ggplot(wdata, aes(x = weight))

a + geom_histogram(bins = 30, color = "black", fill = "grey") +
  geom_vline(aes(xintercept = mean(weight)), 
             linetype = "dashed", size = 0.6)
```

Now lets change the color by group 

```{r}

a + geom_histogram(aes(color = sex), fill = "white", position = "identity") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

```

```{r}

a + geom_histogram(aes(color = sex, fill = sex), position = "identity") + 
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("indianred", "lightblue1"))

```

What if we want to combine density plots and histograms?

```{r}
a + geom_histogram(aes(y = stat(density)), 
                   color = "black", fill = "white") +
  geom_density(alpha = 0.2, fill = "#FF6666")

```

```{r}

a + geom_histogram(aes(y = stat(density), color = sex), 
                   fill = "white", position = "identity") +
  geom_density(aes(color = sex), size = 1) +
  scale_color_manual(values = c("indianred", "lightblue1"))

```

## Dotplots 

First lets load the required packages

```{r}

library(ggplot2)

```

Lets set our theme

```{r}

theme_set(
  theme_dark() +
    theme(legend.position = "top")
)

```

First lets initiate a ggplot object called TG

```{r}

data ("ToothGrowth")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

tg <- ggplot(ToothGrowth, aes(x=dose, y = len))

```

Lets create a dotplot with a summary statistic

```{r}

tg + geom_dotplot(binaxis = "y", stackdir = "center", fill = "lightgray") +
  stat_summary(fun = mean, fun.args = list(mult=1))

```

```{r}

tg + geom_boxplot(width = 0.5) + 
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "white")

```
```{r}

tg + geom_violin(trim = FALSE) + 
  geom_dotplot(binaxis = "y", stackdir = "center", fill = "#999999") +
  stat_summary(fun = mean, fun.args = list(mult=1))

```

```{r}

tg + geom_boxplot(width = 0.5) + 
  geom_dotplot(aes(fill = supp), binaxis = 'y', stackdir = "center") + 
  scale_fill_manual(values = c("indianred", "lightblue1"))

```
```{r}

tg + geom_boxplot(aes(color = supp), width = 0.5, position = position_dodge(0.8)) + 
  geom_dotplot(aes(fill = supp, color = supp), binaxis = 'y', stackdir = 'center',
               dotsize = 0.8, position = position_dodge(0.8)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))

```

## Lineplots
Now lets change it up and look at some line plots

We'll start by making a custom dataframe kinda like the tooth dataset. This way we can see the lines and stuff that we're modifying

```{r}

df <- data.frame(dose = c("D0.5", "D1", "D2"),
                 len = c(4.2, 10, 29.5))

```

Now lets create a second dataframe for plotting by groups

```{r}

df2 <- data.frame(supp = rep(c("VC", "OJ"), each = 3),
                  dose = rep(c("D0.5", "D1", "D2"), 2),
                  len = c(6.8, 15, 33, 4.2, 10, 29.5))

df2

```
Now lets again load ggplot2 and set a theme

```{r}

library(ggplot2)

theme_set(
  theme_gray() +
    theme(legend.position = "right")
)

```

Now lets do some basic time plots. First we will build a function to display all the different line types

```{r}

generateRLineTypes <- function (){
  oldPar <- par()
  par(font = 2, mar = c(0,0,0,0))
  plot(1, pch="", ylim = c(0,6), xlim=c(0,0.7), axes = FALSE, xlab = "", ylab = "")
  for(i in 0:6) lines(c(0.3, 0.7), c(i,i), lty = i, lwd = 3)
  text(rep(0.1,6), 0:6, labels = c("0. 'Blank'", "1.'solid'", "2.'dashed'", "3.'dotted'",
                                  "4.'dotdash'", "5.'longdash'", "6.'twodash'"))
  par(mar=oldPar$mar, font=oldPar$font)
}

generateRLineTypes()

```
Now lets build a basic line plot

```{r}

p <- ggplot(data = df, aes(x = dose, y = len, group = 1))

p + geom_line() + geom_point()
```

Now lets modify the line type and color

```{r, line type}

p + geom_line(linetype = "dashed", color = "steelblue") + 
  geom_point(color = "steelblue")

```

Now lets try a step graph, which indicates a threshold type progression

```{r}

p + geom_step() + geom_point()

```

Now lets move on to making multiple groups. First we'll create our ggplot object 

```{r, multiple groups}

p <- ggplot(df2, aes(x=dose, y=len, group = supp))

```

Now lets change line types and point shapes by group

```{r, change line types by group}

p + geom_line(aes(linetype = supp, color = supp)) + 
  geom_point(aes(shape = supp, color = supp)) +
  scale_color_manual(values = c("red", "blue"))

```
Now lets look at line plots with a numeric x axis 

```{r}

df3 <- data.frame(supp = rep(c("VC", "OJ"), each = 3),
                  dose = rep(c("0.5", "1", "2"), 2),
                  len= c(6.8, 15, 33, 4.2, 10, 29.5))
df3

```
Now lets plot where both axises are treated as continuous labels

```{r}

df3$dose <- as.numeric(as.vector(df3$dose))
ggplot(data = df3, aes(x=dose, y = len, group = supp, color = supp)) +
  geom_line() + geom_point()

```
Now lets look at a line graph with having the x axis as dates. We'll use the built in economics time series for this example.

```{r}
head(economics)

```
```{r}

ggplot(data = economics, aes(x = date, y = pop)) +
  geom_line()

```
Now lets subset the data

```{r}

ss <- subset(economics, date > as.Date("2006-1-1"))
ggplot(data = ss, aes(x = date, y = pop)) + geom_line()

```

We can also change the line size, for instance by another variable like unemployment 

```{r}

ggplot(data = economics, aes(x=date, y = pop)) +
  geom_line(aes(size = unemploy/pop))

```

We can also plot multiple time-series data

```{r}

ggplot(economics, aes(x = date)) +
  geom_line(aes(y=psavert), color = "darkred") +
  geom_line(aes(y = uempmed), color = "steelblue", linetype = "twodash")

```

Lastly, lets make this into an area plot

```{r}

ggplot(economics, aes(x=date)) + 
  geom_area(aes(y = psavert), fill = "#999999",
            color = "#999999", alpha = 0.5) +
  geom_area(aes(y= uempmed), fill = "#E69F00",
            color = "#E69F00", alpha = 0.5)
  
```

## Ridge Plots
First lets load the required packages

```{r}
library(ggplot2)
library(ggridges)

#BiocManager::install{"ggridges")

```

Now lets load some sample data

```{r}
?airquality
```

```{r}

air <- ggplot(airquality) + aes(Temp, Month, group = Month) + geom_density_ridges()

air
```

Now lets add some pazzaz to our graph

```{r}

library (viridis)

ggplot(airquality) + aes(Temp, Month, group = Month, fill = ..x..) +
  geom_density_ridges_gradient() + 
  scale_fill_viridis(option = "C", name = "Temp")

```

Last thing we will do is create a facet plot for all our data. 

```{r}
library(tidyr)

airquality %>%
  gather(key = "Measurement", value = "value", Ozone, Solar.R, Wind, Temp) %>%
  ggplot() + aes(value, Month, group = Month) +
  geom_density_ridges() +
  facet_wrap(~ Measurement, scales = "free")
  
```

## Density Plots
A density plot is a nice alternative to a histogram

```{r}

set.seed(1234)

wdata = data.frame(
  sex = factor(rep(c("F", "M"), each = 200)),
  weight = c(rnorm(200, 55), rnorm(200, 58))
)

```


```{r}

library(dplyr)
mu <- wdata %>%
  group_by(sex) %>%
summarise(grp.mean = mean(weight))

```

Now lets load the graphing packages 

```{r}
library(ggplot2)
theme_set(
  theme_classic() +
    theme(legend.position = "right")
)
```

Now lets do a basic plot function. First we will create a ggplot object 

```{r}

d <- ggplot(wdata, aes(x = weight))

```

Now lets do a basic density plot

```{r}
d + geom_density() +
  geom_vline(aes(xintercept = mean(weight)), linetype = "dashed")

```

Now lets change the y axis to count instead of density 

```{r}
d + geom_density(aes(y = stat(count)), fill = "lightgray") +
  geom_vline(aes(xintercept = mean(weight)), linetype = "dashed")

```

```{r}

d + geom_density(aes(color = sex)) + 
  scale_color_manual(values = c("darkgray", "gold"))

```

Lastly, lets fill the density plots 

```{r}

d + geom_density(aes(fill = sex), alpha = 0.4) +
  geom_vline(aes(xintercept = grp.mean, color = sex), data = mu, linetype = "dashed") +
  scale_color_manual(values = c("grey", "gold")) +
  scale_fill_manual(values = c("grey", "gold"))

```

# Plotly

## Line Plots
First lets load our required package 

```{r}

library(plotly)

```

Letss start with a scatter plot of the orange dataset

```{r}
Orange <- as.data.frame(Orange)

plot_ly(data = Orange, x = ~age, y = ~circumference)

```
Now lets add some more info

```{r}
plot_ly(data = Orange, x = ~age, y = ~circumference,
        color = ~Tree, size = ~age,
        text = ~paste("Tree ID:", Tree, "<br>Age:", age, "Circ:", circumference)
)

```
Now lets create a random distribution and add it to our dataframe

```{r}
trace_1 <- rnorm(35, mean = 120, sd = 10)
new_data <- data.frame(Orange, trace_1)

```

We'll use the random numbers as lines on the graph

```{r}
plot_ly(data = new_data, x = ~age, y = ~circumference, color = ~Tree, size = ~age, 
        text = ~paste("Tree ID:", Tree, "<br>Age:", age, "<br>Circ:", circumference)) %>%
  add_trace(y = ~trace_1, mode = 'lines') %>% 
  add_trace(y = ~circumference, mode = 'markers')


```
Now lets create a graph with the option of showing as a scatter or line, and add labels.

```{r}

plot_ly(data = Orange, x = ~age, y = ~circumference,
        color = ~Tree, size = ~circumference,
        text = ~paste("Tree ID:", Tree, "<br>Age:", age, "Circ:", circumference)) %>%
  add_trace(y = ~circumference, mode = 'markers') %>%
  layout(
    title = "Plot with switchable trace", 
    updatemenus = list(
      list(
        type = "dropdown",
        y = 0.8,
        buttons = list(
          list(method = "restyle",
               args = list("mode", "markers"),
               label = "Marker"
          ),
          list(method = "restyle",
               args = list("mode", "lines"),
               labels = "Lines"
          )
        )
      )
    )
  )
```

## Plotly 3D
First lets load our required packages 

```{r}

library(plotly)

```

Now lets create a random 3D matrix

```{r}

d <- data.frame(
  x <- seq(1,10, by = 0.5),
  y <- seq(1,10, by = 0.5)
)

z <- matrix(rnorm(length(d$x) * length(d$y)), nrow = length(d$x), ncol = length(d$y))

```

Now lets plot our 3D data

```{r}

plot_ly(d, x=~x, y = ~y, z = ~z) %>%
  add_surface()

```

Lets add some more aspects to it, such as at topogrophy

```{r}

plot_ly(d, x = ~x, y = ~y, z = ~z) %>%
  add_surface(
    contours = list(
      z = list(
        show = TRUE,
        usecolormap = TRUE,
        highlightcolor = "FF0000",
        project = list(z = TRUE)
      )
    )
  )
```

Now lets lookk at a 3D scatter plot

```{r}

plot_ly(longley, x = ~GNP, y = ~Population, z = ~Employed, marker = list(color = ~GNP)) %>%
  add_markers()

```

# Other Graphing Techniques

## Error Bars

```{r}
library(ggplot2)
library(dplyr)
library(plotrix)

theme_set(
  theme_classic() +
    theme(legend.position = 'top')
)
```


Lets again use the tooth data for this exercise 

```{r}

df <- ToothGrowth
df$dose <- as.factor(df$dose)

```

Now lets use dplyr for manipulation purposes

```{r}

df.summary <- df %>%
  group_by(dose) %>%
  summarise(
    sd = sd(len, na.rm = TRUE),
    len = mean(len),
    stderr = std.error(len, na.rm = TRUE)
  )

df.summary 
```

Lets now look at some key functions
* geom_crossbar() for hollow bars with middle indicated by a horizontal line
* geom_errorbar() for error bars 
* geom_errorbarh() for horizontal error bars
* geom_linerange() for drawing an interval represented by a vertical line 
* geom_pointrange() for creating an interval represented by a vertical line; with a point in the middle 

lets start by creating a ggplot object

```{r}
tg <- ggplot(
  df.summary,
  aes(x = dose, y = len, ymin = len - sd, ymax = len + sd)
)
```

Now lets look at the most basic error bars 

```{r}
tg + geom_pointrange()

tg + geom_errorbar(width = 0.2) +
  geom_point(size = 1.5)

```
Now lets create horizontal error bars by manipulating our graph
  
```{r}

ggplot(df.summary, aes(x= len, y=dose, xmin = len-sd, xmax = len+sd)) +
  geom_point() +
  geom_errorbarh(height = 0.2)

```
This just gives you an idea of error bars on the horizontal axis

Now lets look at adding jitter points (actual measurements) to our data.

```{r}

ggplot(df, aes(dose, len)) +
  geom_jitter(position = position_jitter(0.2), color = "darkgray") + 
  geom_pointrange(aes(ymin= len-sd, ymax = len+sd), data = df.summary)

```

Now lets try error bars on a violin plot

```{r}

ggplot(df, aes(dose, len)) +
  geom_violin(color = "darkgray", trim = FALSE) + 
  geom_pointrange(aes(ymin = len - sd, ymax = len+sd), data = df.summary)

```

Now how about with a line graph?

```{r}

ggplot(df.summary, aes(dose, len)) +
  geom_line(aes(group = 1)) + # always specify this when you have 1 line
  geom_errorbar(aes(ymin = len-stderr, ymax = len+stderr), width = 0.2) +
  geom_point(size = 2)

```
Now lets make a bar graph with halve error bars

```{r}

ggplot(df.summary, aes(dose, len)) + 
  geom_col(fill = "lightgrey", color = "black") + 
  geom_errorbar(aes(ymin = len, ymax = len+stderr), width = 0.2)

```
You can see that by not specifying wmin = len-stderr, we have in essence cut our error bar in half.

How about we add jitter points to line plots? We need to use the original dataframe for the jitter plot, and the summary df for the geom layers.

```{r}

ggplot(df, aes(dose, len)) + 
  geom_jitter(position = position_jitter(0.2), color = "darkgray") + 
  geom_line(aes(group = 1), data = df.summary) +
  geom_errorbar(
    aes(ymin = len - stderr, ymax = len + stderr),
    data = df.summary, width = 0.2) +
  geom_point(data = df.summary, size = 0.2)

```
What about adding jitterpoints to a barplot?

```{r}

ggplot(df, aes(dose, len)) + 
  geom_col(data = df.summary, fill = NA, color = "black") +
  geom_jitter(position = position_jitter(0.2), color = "blue") + 
  geom_errorbar(aes(ymin = len - stderr, ymax = len + stderr),
    data = df.summary, width = 0.2) 

```
What if we wanted to have our error bars per group? (OJ vs VC)

```{r}

df.summary2 <- df %>% 
  group_by(dose, supp) %>%
  summarise(
    sd = sd(len), 
    stderr = std.error(len),
    len = mean(len)
  ) 
df.summary2 

```
Now you can we have mean and error for each dose and supp 

```{r}

ggplot(df.summary2, aes(dose, len)) + 
  geom_pointrange(
    aes(ymin = len-stderr, ymax = len+stderr, color = supp),
    position = position_dodge(0.3)) + 
  scale_color_manual(values = c("indianred", "lightblue"))

```

How about line plots with multiple error bars? 

```{r} 

ggplot(df.summary2, aes(dose, len)) +
  geom_line(aes(linetype = supp, group = supp)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = len-stderr, ymax = len+stderr, group = supp), width = 0.2)

```
And the same with a bar plot

```{r}

ggplot(df.summary2, aes(dose, len)) +
  geom_col(aes(fill = supp), position = position_dodge(0.8), width = 0.7)+
   geom_errorbar(
     aes(ymin = len-sd, ymax = len+sd, group = supp),
     width = 0.2, position = position_dodge(0.8)) +
   scale_fill_manual(values = c("indianred", "lightblue"))

```
Now lets add some jitterpoints

```{r}

ggplot(df, aes(dose, len, color = supp)) +
  geom_jitter(position = position_dodge(0.2)) +
  geom_line(aes(group = supp), data = df.summary2) +
  geom_point() +
  geom_errorbar(aes(ymin = len - stderr, ymax = len +stderr, group = supp), data = df.summary2, width = 0.2)

```

```{r}

ggplot(df, aes(dose, len, color = supp)) +
  geom_col(data = df.summary2, position = position_dodge(0.8), width = 0.7, fill = "white") +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8)) +
  geom_errorbar(
    aes(ymin = len - stderr, ymax = len+stderr), data = df.summary2,
    width = 0.2,  position = position_dodge (0.8)) + 
  scale_color_manual(values = c("indianred", "lightblue")) + 
  theme(legend.position = "top")

```

## ECDF Plots
Now let do an empirical cumulative distribution function. This reports any given number percentile of individuals that are above or below that threshold. 

```{r}

set.seed(1234)

wdata = data.frame(
  sex = factor(rep(c("F", "M"), each = 200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))

```

Now lets look at our dataframe

```{r}

head(wdata, 5)

```


Now lets load our plotting package 

```{r}
library(ggplot2)

theme_set(
  theme_classic() +
    theme(legend.position = "bottom")
)

```

Now lets create our ECDF Plot

```{r}
ggplot(wdata, aes(x=weight)) +
  stat_ecdf(aes(color = sex, linetype = sex), 
            geom = "step", size = 1.5) + 
  scale_color_manual(values = c("#00AFBB", "#E7B900")) +
  labs(y = "weight")

```

## qq Plots
Now lets take a look at qq plots. These are used to determine if the given data follows a normal distribution.

```{r}

#install.packages("ggpubr")

set.seed(1234)

```


Now lets randomly generate some data

```{r}

wdata = data.frame(
  sex = factor(rep(c("F", "M"), each = 200)),
  weight = c(rnorm(200, 55), rnorm(200, 58))
)

```

Lets set our theme for the graphing with ggplot

```{r}
library(ggplot2)

theme_set(
  theme_classic() +
    theme(legend.position = "top")
)
```

Create a qq plot of the weight 

```{r}

ggplot(wdata, aes(sample=weight)) + 
  stat_qq(aes(color = sex)) +
  scale_color_manual(values = c("#0073C2FF", "#FC4E07")) +
  labs(y = "weight")

```

```{r}

#install.packages(ggpubr)
library(ggpubr)

ggqqplot(wdata, x = "weight", 
         color = "sex",
         palettes = c("#0073C2FF", "#FC4E07"),
         ggtheme = theme_pubclean())

```

Now what would a non-normal distribution look like

```{r}

#intsall.packages(mnonr)

library(mnonr)

data2 <- mnonr::mnonr(n = 1000, p = 2, ms = 3, mk = 61, Sigma = matrix(c(1,0.5, 0.5, 1), 2, 2), initial = NULL)

data2 <- as.data.frame(data2)

```

```{r}

ggplot(data2, aes(sample=V1)) +
  stat_qq()

```

```{r}

ggqqplot(data2, x = "V1",
         palette = "#0073C2FF",
         ggtheme = theme_pubclean())

```

## Facet Plots
Lets look at how to put multiple plots together into a single figure

```{r}

library(ggpubr)
library(ggplot2)

theme_set(
  theme_bw() +
    theme(legend.position = "top")
  
)
```

First lets create a nice boxplot

lets load the data
```{r}

df <- ToothGrowth
df$dose <- as.factor(df$dose)
```

and create the plot object 

```{r}
p <- ggplot(df, aes(x= dose, y = len)) +
  geom_boxplot(aes(fill = supp), position = position_dodge(0.9)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

p

```
Now lets look at the gvgplot facit function 

```{r}

p + facet_grid(rows = vars(supp))

```
Now lets do a facet with multiple variables 

```{r}

p + facet_grid(rows = vars(dose), cols = vars(supp))

p 
```
Now lets look at the facet_wrap function. This allows facets to be placed side-by-side

```{r}

p + facet_wrap(vars(dose), ncol = 2)
```
Now how do we combine multiple plots using ggarrange()

Lets start by making some basic plots. First we will define a color palette and data

```{r}

my3cols <- c("#e7B800", "#2E9FDF", "#FC4E07")
ToothGrowth$dose <- as.factor(ToothGrowth$dose)

```

Now lets make some basic plots

```{r}

p <- ggplot(ToothGrowth, aes(x = dose, y = len))
bxp <- p + geom_boxplot(aes(color = dose)) + 
  scale_color_manual(values = my3cols)

```

Now lets make some basic plots 

```{r}

p <- ggplot(ToothGrowth, aes(x = dose, y = len))
bxp <- p + geom_boxplot(aes(color = dose)) + 
  scale_color_manual(values = my3cols)

```

Ok now lets do a dotplot

```{r}

dp <- p + geom_dotplot(aes(color = dose, fill = dose),
                       binaxis = 'y', stackdir = 'center') +
  scale_color_manual(values = my3cols) +
  scale_fill_manual(values = my3cols)

```

Now lastly lets create a lineplot

```{r}

lp <- ggplot(economics, aes(x=date, y=psavert)) +
  geom_line(color = "indianred")

```

Now we can make the figure 

```{r}

figure <- ggarrange(bxp, dp, lp, labels = c("A", "B", "C"), ncol = 2, nrow = 2)

figure

```
This looks great, but we can make it look even better

```{r}

figure2 <- ggarrange(
  lp,
  ggarrange(bxp, dp, ncol=2, nlabs = c ("B", "C")),
  nrow = 2,
  labels = "A")
  
figure2


```
Ok this looks really good, but you'll notice that there are two legends that are the same.

```{r}

ggarrange(
  bxp, dp, labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom")
```
Lastly, we should export the plot 

```{r}
ggexport(figure2, filename = "facetfigure.pdf")

```

We can also export multiple plots to a pdf

```{r}

ggexport(bxp, dp, lp, filename = "multi.pdf")
```

Lastly, we can export to pdf with multiple pages and multiple columns

```{r}

ggexport(bxp, dp, lp, bxp, filename = "test2.pdf", nrow = 2, ncol = 1)

```
## Heatmaps
Lets get started with heatmaps

```{r}

#install.packages(heatmap3)

```

```{r}
library(heatmap3)

```

Now lets get our data.

```{r}

data <- ldeaths

data2 <- do.call(cbind, split(data, cycle(data)))
dimnames(data2) <- dimnames(.preformat.ts(data))

```

Now lets generate a heat map

```{r}
heatmap(data2)
```

```{r}

heatmap(data2, Rowv = NA, Colv = NA)

```

Now lets play with the colors

```{r}
rc <- rainbow(nrow(data2), start = 0, end = 0.3)
cc <- rainbow(ncol(data2), start = 0, end = 0.3)

```

Now lets apply our color selections 

```{r}

heatmap(data2, ColSideColors = cc)

```

```{r}
library(RColorBrewer)
heatmap(data2, ColSideColors = cc, 
        col = colorRampPalette(brewer.pal(8,"PiYG"))(25))
```

Theres more that we can customize

```{r}

library(gplots)

heatmap.2(data2, ColSideColors = cc, 
        col = colorRampPalette(brewer.pal(8, "PiYG"))(25))
```
# Outlier Detection
## Missing Values
Missing Values:
If you encounter a unusual value in your dataset, and simply want to move on to the rest of your analysis, you have two options:

Drop the entire row with the strange values:

```{r}
library(dplyr)
library(ggplot2)

diamonds <- diamonds

diamonds2 <- diamonds %>%
  filter(between(y, 3, 20))

```

In this instance, y is the width of the diamond, so anything under 3 mm or above 20 is excluded

I don't recommend this option, just because there is one bad measurement doesn't mean they are all bad 
Instead, I recommend replacing the unusual values with missing values

```{r}

diamonds3 <- diamonds %>%
  mutate(y = ifelse(y < 3 | y > 20, NA, y))

```

Like R, ggplot2 subscribes to the idea that missing values shouldn't pass silently into the night.

```{r}

ggplot(data = diamonds3, mapping = aes(x = x, y = y)) +
  geom_point()

```

If you want to supress that warning you can use na.rm = TRUE

```{r}

ggplot(data = diamonds3, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

```

Other times you want to understand what makes observations with missing values of different to the observation with recorded values. For example in the NYCflights13 dataset, missing values in the dep_time variable indicate that the flight was cancelled. So you might want to compare thr schedule departure times for cancelled and non-cancelled times.


```{r}
library(nycflights13)
library(dplyr)
library(ggplot2)

nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100, 
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(color = cancelled), binwidth = 1/4)

```
## Covariation

```{r}

library(ggplot2)

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut), bindwidth = 500)

```
Its hard to see the difference in distribution because the counts differ so much.

```{r}

ggplot(diamonds) + 
  geom_bar(mapping = aes(x = cut))

```
to make the comparison easier, we need to swap the display on the y-axis. Instead of displaying count, we'll display density, which is the count standardized so that the area under the curve is one.

```{r}
ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  geom_freqpoly(mapping = aes(color = cut), bindwith = 500)

```

It appears that fair diamonds (the lowest cut quality) have the highest average price. But maybe thats bacause frequency polygons are a little hard to interpret.

Another alternative is the boxplot. A boxplot is a type of visual shorthand for a distribution of values. 

```{r}

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()

```

We see much less information about the distribution, but the boxplots are much more compact, so we can more easily compare them. It supports the counterintuitive finding that better quality diamonds are cheaper on average!

lets look at some car data

```{r}

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

```
```{r}

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

```
If you have long variable names, you can switch the axis and flip it 90 degrees.

```{r}

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()

```
To visualize the correlation between to continuous variables, we can use a scatter plot.

```{r}

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
```
Scatterplots become less useful as the size of your dataset grows, because we get overplot. We can fix this using the alpha aesthetic

```{r}

ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price), alpha = 1/100)

```
# Exploratory Data Analysis
## EDA (part 1 - 5; Outliers included at bottom)
First lets load a required library 

```{r}
library(RCurl)
library(dplyr)

```


Now lets get our data
```{r}
site <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/colleges/colleges.csv"

College_Data <- read.csv(site)
```

First lets use the str function, this shows the structure of the object

```{r}

str(College_Data)
```

What if we want to arrange our dataset alphabetically by college?

```{r}

alphabetical <- College_Data %>%
  arrange(College_Data$college)

```

The glimpse package is another way to preview data

```{r}
glimpse(College_Data)

```
We can also subset with select()

```{r}
College_Cases <- select(College_Data, college, cases)
```

We can also filter or subset with the filter function

```{r}

Louisiana_Cases <- filter(College_Data, state =="Louisiana")

```

Lets filter our a smaller amount of states

```{r}

South_Cases <- filter(College_Data, state =="Louisiana" | state =="Texas" | state =="Arkansas" | state == "Mississippi")

```

Lets look at some time series data

First we'll load the required libraries 
```{r}
library(lubridate)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

```

Now lets load some data

```{r}

state_site <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

State_Data <- read.csv(state_site)

```

Lets create group_by object using the state column

```{r}

state_cases <- group_by(State_Data, state)

class(state_cases)

```
How many measurements were made by state? This gives us an idea of when states started reporting

```{r}

Days_since_first_reported <- tally(state_cases)

```

Lets visualize some data

First lets start off with some definitions

Data - obvious - the stuff we want to visualize

Layer - made of gemetric elements and requisite statistical information. Include geometric objects which represents the plot

Scales - used to map values in the data space that is used for creation of values (color, size, shape, etc)

Coordinate system - describes how the data coordinates are mapped together in relation to the plan on the graphic

Faceting - how to break up data into subsets to display multiple types or groups of data

Theme - controls the finer points of the display, such as font size and background color

```{r}

options(repr.plot.width = 6, rep.plot.height = 6)

class(College_Data)

head(College_Data)

summary(College_Data)

```
Now lets take a look at a different dataset

```{r}

iris <- as.data.frame(iris)

class(iris)

head(iris)

summary(iris)

```
Lets start by creating a scatter plot of the College Data 

```{r}

ggplot(data = College_Data, aes(x = cases, y = cases_2021)) +
  geom_point() +
  theme_minimal()

```

Now lets do the iris data

```{r}

ggplot(data = iris, aes(x = Sepal.Width, y = Sepal.Length)) +
  geom_point() +
  theme_minimal()

```

Lets color coordinate our college data

```{r}

ggplot(data = College_Data, aes(x = cases, y = cases_2021, color = state)) +
  geom_point() +
  theme_minimal()

```

Lets color coordinate the iris data

```{r}

ggplot(data = iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) +
  geom_point()
  theme_minimal()
  
```
Lets run a simple histogram of our Louisiana Case Data 

```{r}
hist(Louisiana_Cases$cases, freq =NULL, density = NULL, breaks = 10, xlab = "Total Cases", ylab = "Frequency", main = "Total College Covid-19 Infections (Louisiana)")

```
Lets run a simple histogram for the Iris data

```{r}

hist(iris$Sepal.Width, freq = NULL, density = NULL, breaks = 10, xlab = "Sepal width", 
     ylab = "Frequency", main = "Iris Sepal width")

```

```{r}

histogram_college  <- ggplot(data = Louisiana_Cases, aes(x = cases))

histogram_college + geom_histogram(bindwidth = 100, color = "black", aes(fill = county)) + xlab("cases") + ylab("Frequency") + ggtitle("Histogram of Covid 19 cases in Louisiana")

```
Lets create a ggplot for the IRIS data

```{r}

histogram_iris <- ggplot(data = iris, aes(x = Sepal.Width))

histogram_iris + geom_histogram(bindwidth = 0.2, color = "black", aes(fill = Species)) + xlab("Sepal Width") + ylab("Frequency") + ggtitle("Histogram of Iris Sepal Width by Species")

```
Maybe a density plot makes more sense for our college data

```{r}

ggplot(South_Cases) +
  geom_density(aes(x = cases, fill = state), alpha = 0.25)

```


Lets do it with the iris data 

```{r}

ggplot(iris) +
  geom_density(aes(x = Sepal.Width, fill = Species), alpha = 0.25)

```


```{r}

ggplot(data = iris, aes(x = Species, y = Sepal.Length, color = Species)) +
  geom_violin() +
  theme_classic() +
  theme(legend.position = "none")

```

Now lets try the south data

```{r}

ggplot(data = South_Cases, aes(x = state, y = cases, color = state)) +
  geom_violin() +
  theme_gray() +
  theme(legend.position = "none")

```

Now lets take a look at risidual plots. This graph displays the residuals on the vertical axis, and the independent variable on the horizontal. In the event that the points in a residual plot are dispersed in a random manner around the horizontal axis, it is appropriate to use a linear regression. If they are not randomly disperced, a non linear model is more appropriate.


Lets start with the iris data

```{r}

ggplot(lm(Sepal.Length ~ Sepal.Width, data = iris)) + 
  geom_point(aes(x = .fitted, y = .resid))

```

Now look at the southern states cases

```{r}

ggplot(lm(cases ~ cases_2021, data = South_Cases)) + 
  geom_point(aes(x = .fitted, y = .resid))

```

A linear model is not a good call for the state cases

Now lets do some correlations




```{r}


obesity <- read.csv("~/Desktop/classroom/myfiles/Obesity_insurance.csv")

```

```{r}

library(tidyr)
library(dplyr)
library(plyr)

```

Lets look at the structure of the dataset

```{r}

str(obesity)

```

Lets look at the column classes

```{r}
class(obesity)
```
And get a summary of distribution of the variables

```{r}

summary(obesity)

```
Now lets look at the distribution for insurance charges

```{r}

hist(obesity$charges)

```
We can also get an idea of the distribution using a boxplot

```{r}

boxplot(obesity$charges)

```
```{r}

boxplot(obesity$bmi)

```
Now lets look at correlations. The cor() command is used to determine correlations between two vectors, all of the columns of a data frame, or two data frames. The cov() command, on the otherhand examines the covariance. The cor.test() command carries out a test as to the significance of the correlation

```{r}

cor(obesity$charges, obesity$bmi)

```
This test uses a spearman Rho correlation, or you can use Kendall's tau by specifying it 

```{r}

cor(obesity$charges, obesity$bmi, method = 'kendall')

```
This correlation measurers strength of a correlation between -1 and 1.

Now lets look at the Tietjen=Moore test. This is used for univariate datasets. The algorithm depicts the detection of the outliers in a univariate dataset.

```{r}
TietjenMoore <- function(dataSeries, k)
{
 n = length(dataSeries)
 # Comput the absolute residuals
 r = abs(dataSeries - mean(dataSeries))
 ## Sort data according to a size of residual
 df = data.frame(dataSeries,r)
 dfs = df[order(df$r),]
 ## Create a subset of the data without the largest values.
 klarge = c((n-k+1):n)
 subdataSeries = dfs$dataSeries[-klarge]
 ## Compute the sums of squares.
 ksub = (subdataSeries - mean(subdataSeries))**2
 all = (df$dataSeries - mean(df$dataSeries))**2
 # Compute the test statistic.
 sum(ksub)/sum(all)
}
 
```

This function helps to compute the absolute residuals and sorts data according to the size of the residuals.
Later, we will focus on the computation of sum of squares.

```{r}

FindOutliersTietjenMooreTest <- function(dataSeries, k, alpha = 0.5){
  ek <- TietjenMoore(dataSeries, k)
  # Compute critical values based on simulation.
  test = c(1:10000)
  for (i in 1:1000){
    dataSeriesdataSeries = rnorm(length(dataSeries))
    test[i] = TietjenMoore(dataSeriesdataSeries, k)}
  Talpha = quantile(test, alpha)
  list(T = ek, Talpha = Talpha)
  
}

```

This function helps us to compute the critical values based on simulation data. Now lets demomstrate these functions with sample data and the obesity dataset for evaluating this algorithm.

The critical region for the Tietjen-Moore test is determined by stimulation.
The simulation is performed by generating a standard normal random sample of size
n and computing the Tietjen Moore test statistic. Typically, 10,000 randome samples are use. The values of the Tietjen-Moore statistic obtained from the data is compared to this reference distribution. The values of the test statistic is between zero and one. If there are no outliers in the data, the test statistic is close to 1. If there are outliers the test statistic will be closer to zero. Thus, the test is always a lower, one-tailed test regarless of which test statistic issued, Lk or Ek.

First we will look at charges

```{r}

boxplot(obesity$charges)

FindOutliersTietjenMooreTest(obesity$charges, 50)

```

Lets check out bmi

```{r}

boxplot(obesity$bmi)

FindOutliersTietjenMooreTest(obesity$bmi, 2)

```
Probability Plots

```{r}

library(ggplot2)
library(tigerstats)

```

We will use the probability plot function and their output dnorm: density function of the normal distribution.
Using the density, it is possible to determine the probability of events. 
Or for examples, you may wonder "what is the likelihood that a person has an BMI of exactly __? 
In this case, you would need to retrieve the density of the BMI distribution at values 140.
The BMI distribution can be modeled with a mean of 100 and a standard deviation of 15.
The corresponding density is:

```{r}

bmi.mean <- mean(obesity$bmi)
bmi.sd <- sd(obesity$bmi)

```

Lets create a plot of our normal distribution

```{r}

bmi.dist <- dnorm(obesity$bmi, mean = bmi.mean, sd = bmi.sd)
bmi.df <- data.frame("bmi" = obesity$bmi, "Density" = bmi.dist)

ggplot(bmi.df, aes(x = bmi, y = Density)) +
  geom_point()

```

This gives us the probability of every single point occurring

Now lets use the pnorm function for more info

```{r}

bmi.dist <- pnorm(obesity$bmi, mean = bmi.mean, sd = bmi.sd)
bmi.df <- data.frame("bmi" = obesity$bmi, "Density" = bmi.dist)

ggplot(bmi.df, aes(x=bmi, y = Density)) +
  geom_point()

```
What if we want to find the probability of the bmi being greater than 40 in our distribution?

```{r}
pp_greater <- function(x) {
  paste(round(100 * pnorm(x, mean = 30.66339, sd = 6.09818, lower.tail = FALSE), 2), "%")
}

pp_greater(40)

pnormGC(40, region = "above", mean = 30.66339, sd = 6.09818, graph = TRUE)

```
What about the probability that a bmi is less than 40 in our population?

```{r}

pp_less <- function(x) {
  paste(round(100 *(1-pnorm(x, mean = 30.66339, sd = 6.09818, lower.tail = FALSE)),2), "%")
}

pp_less(40)

```
What if we want to find the area in between?

```{r}

pnormGC(c(20, 40), region = "between", mean = 30.66339, sd = 6.09818, graph = TRUE)

```
What if we want to know the quantiles? Lets use the pnorm function. We need to assume a normal distribution for this.

What bmi represents the lowest 1% of the population?

```{r}

qnorm(0.01, mean = 30.66339, sd = 6.09818, lower.tail = TRUE)

```

What if you want a random sampling of values within your distribution? 

```{r}
subset <- rnorm(50, mean = 30.66339, sd = 6.09818)

hist(subset)

```

```{r}

subset2 <- rnorm(5000, mean = 30.66339, sd = 6.09818)

hist(subset2)

```

Shapiro-wilk Test 

So now we know how to generate a normal distribution, how do we tell if our samples came from a normal distribution? 

```{r}

?shapiro.test(obesity$charges[1:5])

```

You can see here, width a small sample size, we would reject then null hypothesis that the sample came from a normal distribution. We can increase the power of the test by increasing the sample size.

```{r}

shapiro.test(obesity$charges[1:1000])

```
Now lets check out age 

```{r}

shapiro.test(obesity$age[1:1000])

```
And lastly bmi

```{r}

shapiro.test(obesity$bmi[1:1000])

```
Time series data

First lets load our packages


```{r}

library(readr)
library(readxl)

Air_data <- read_xlsx("AirQualityUCI.xlsx")

```
Date - date of measurement 
time - time of measurement
CO(GT) - average hourly CO2
PT08, s1(CO) - tin oxide hourly average sensor response
NMHC - average hourly non-metallic hydrocarbon concentration
C6HC = average benzene concentration 
PT08.S3(NMHC) - titania average hourly sensor response
NOx - average hourly NOx concentration
NO2 - average hourly NO2 concentration
T - temper
RH - relative humidity
AH - absolute humidity

```{r}

str(Air_data)

```
```{r}
library(tidyr)
library(dplyr)
library(lubridate)
library(hms)
library(ggplot2)

```

Lets get rid of the data in the time column

```{r}

Air_data$Time <- as_hms(Air_data$Time)

glimpse(Air_data)

```

```{r}
plot(Air_data$AH, Air_data$RH, main = "Humidity Analysis", xlab = "Absolute Humidity", ylab = "Relative Humidity")

```

Notice we have an outlier in our data 

```{r}

t.test(Air_data$RH, Air_data$AH)

```










What if we want to know what our outliers are?

First we need to load the required libraries

```{r}

library(outliers)
library(ggplot2)

```

And reload the dataset because we removed outliers 

```{r}

Air_data <- read_xlsx("AirQualityUCI.xlsx")

```


Lets create a function using the grubb test to identify all outliers. The grubb test identifies outliers in a univariate dataset that is presumed to come from a normal distribution.

```{r}

grubbs.flag <-function(x) {
  # lets create a variable called outliers and save nothing in it, we'll add to the variable 
  # as we identify them
  outliers <- NULL
  # We'll create a variable called test to identify which univariate we are testing 
  test <- x
  # Now using the outliers package, use grubbs.test to find outliers in our variable 
  grubbs.result <- grubbs.test(test)
  # Lets get the p-values of all tested variables
  pv <- grubbs.result$p.value
  # Now lets search through our p-values for ones that are outside of 0.5
  while(pv <  0.05) {
    # anything with a pvalues greater than p = 0.05, we add to our empty outliers vector
    outliers <- c(outliers, as.numeric(strsplit(grubbs.result$alternative, " ")[[1]][3]))
    # Now we want to remove those outliers from our test variable
    test <- x[!x %in% outliers]
    # and run the grubbs test agaiin without the outliers
    grubbs.result <- grubbs.test(test)
    # and save the new p values
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x, Outliers = (x %in% outliers)))
}

```


```{r}

identified_outliers <- grubbs.flag(Air_data$AH)

```

Now we can create a histogram showing where the outliers were

```{r}

ggplot(grubbs.flag(Air_data$AH), aes(x=Air_data$AH, color = Outliers, fill = Outliers)) +
  geom_histogram(bindwidth = diff(range(Air_data$AH))/30) + 
  theme_bw()

```

# Text Mining 
## Text Mining (part 1 - 2)
First we'll look at the unnest_token function

Lets start by looking at an Emily Dickenson passage

```{r}

text <- c("Because I could not stop from Death = ",
          "He kindly stopped for me - ",
          "The Carriage held but just Ourselves - ",
          "and Immortality")

text

```

This is a typical character vector that we might want to analyze. In order to turn it into a tidytext dataset, we first need to put it into a dataframe.

```{r}

library(dplyr)

text_df <- tibble(line = 1:4, text = text)

text_df

```
Reminder: A tibble is a modern class sofa data frame within R. Its available in the dplyr and tibble packages, that has a convient print method, will not convert strongs to factors, and does not use row names. Tibbles are great for use with tidy tools.

Next we will use the 'unest_tokens' function.

First we have the output column name that will be created as the text is unnested into it

```{r}

library(tidytext)

text_df %>% 
  unnest_tokens(word, text)

```
Lets use the janeaustenr package to analyze some Jane Austen texts. There are 6 books in this package.


```{r}

library(janeaustenr)
library(dplyr)
library(stringr)


original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(), 
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup()

original_books

```



To work with this as a tidy dataset, we need to restructure it in the one-token-per-row format, which as we saw earlier is done with the unnest_tokens() function

```{r}

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)

tidy_books

```

This function uses  the tokenizers package to separate each line of text in the original dataframe into tokens.

The default tokenizing is for words, but other options including characters, n-grams, sentences, lines, or paragraphs can be used.

Now that the data is in a one-word-per-row format, we can manipulate it with tools like dplyr.

Often in text analysis, we will want to remove stop words. Stop words are words that are NOT USEFUL for an analysis. 
These include words like the, of, to, and, and so forth.

We can remove stop words (kept in the tidytext dataset 'stop_words') with an anti_join().

```{r}
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

```

The stop words dataset in the tidytext package contains stop words from three lexicons, we can use them all together, as we have three or filter() to only use one set of stop words if thats more appropriate for your analysis.

```{r}

tidy_books %>%
  count(word, sort = TRUE)

```

Because we've been using tidy tools, our word counts are stored in a tidy data frame. This allows us to pipe this directly into ggplot2. For example, we can create a visualization of the most common words.

```{r}
library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n> 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() + 
  labs(y = NULL, x = "word count")

```

The gutenbergr package

This package provides access to the public domain works from the gutenberg project (www.gutenberg.org). This package includes tools for both downloading books and a complete dataset of project gutenberg metadata that can be used to find works of interest. We will mostly use the function gutenberg_download().

Word frequencies 

Lets look at some biology texts, starting with Darwin 

The Voyage at the Beagle - 944
On the origin of species by thee means of natural selection - 1228
The expression of emotions in man and animals - 1227
The descent of man, and selection in relation to sex - 2300

We can access these works using the gutenberg_download() and the Project Gutenberg ID numbers

```{r}

library(gutenbergr)

darwin <- gutenberg_download(c(944, 1227, 1228, 2300), mirror = "https://mirror.csclub.uwaterloo.ca/gutenberg/")

```

Lets break these into tokens

```{r}

tidy_darwin <- darwin %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

```

Lets check out what the most common darwin words are.

```{r}

tidy_darwin %>% 
  count(word, sort = TRUE)

```

Now lets get some work from Thomas Hunt Morgan, who is credited with dicovering chromosomes. 

Regeneration - 57198
The genetic and operative evidence relating to secondary sexual characteristics - 57460
Evolution and Adaptation - 63540

```{r}

morgan <- gutenberg_download(c( 57198, 57460, 63540), mirror = "https://mirror.csclub.uwaterloo.ca/gutenberg/" )

```

Lets tokenize THM

```{r}

tidy_morgan <- morgan %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

```

What are THM's most common words?

```{r}

tidy_morgan %>%
  count(word, sort = TRUE)

```

Lastly lets look at Thomas Henry Huxley

Evidence as to mans place in nature - 2931 
On the reception of the Origin of Species - 2089
Evolution and Ethics, and other essays - 2940
Science and Culture, and other essays = 52344

```{r}
huxley <- gutenberg_download(c(2931, 2089, 2940, 52344), mirror = "https://mirror.csclub.uwaterloo.ca/gutenberg/")

```


```{r}

tidy_huxley <- huxley %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

```


```{r}

tidy_huxley %>%
  count(word, sort = TRUE)

```

Now, lets calculate the frequency for each word for the works of Darwin, Morgan, and Huxley, by binding the frames together.

```{r}

library(tidyr)

frequency <- bind_rows(mutate(tidy_morgan, author = "Thomas Hunt Morgan"),
                       mutate(tidy_darwin, author = "Charles Darwin"),
                       mutate(tidy_huxley, author = "Thomas Henry Huxley")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/ sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = author, values_from = proportion) %>%
  pivot_longer('Thomas Hunt Morgan': 'Charles Darwin', names_to = "author", values_to = "proportion")

frequency

```

Now we need to change the table so that each author has its own row

```{r}

frequency2 <- pivot_wider(frequency, names_from = author, values_from = proportion)

frequency2

```

Now lets plot

```{r}

library(scales)
library(ggplot2) # Make sure to load ggplot2

ggplot(frequency2, aes(x = `Charles Darwin`, y = `Thomas Hunt Morgan`, 
                       color = abs(`Charles Darwin` - `Thomas Hunt Morgan`))) + 
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0,0.001), 
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position = "none") +
  labs(y = "Thomas Hunt Morgan", x = "Charles Darwin")

```
  
                       
```{r}

ggplot(frequency2, aes(x = `Charles Darwin`, y = `Thomas Henry Huxley`), color = abs(`Charles Darwin` - `Thomas Henry Huxley`)) + 
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0,0.001), 
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position = "none") +
  labs(y = "Thomas Henry Huxley", x = "Charles Darwin")

```

```{r}

ggplot(frequency2, aes(x = `Thomas Hunt Morgan`, y = `Thomas Henry Huxley`), color = abs(`Thomas Hunt Morgan` - `Thomas Henry Huxley`)) + 
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0,0.001), 
                       low = "darkslategray4", high = "gray75") +
  theme(legend.position = "none") +
  labs(y = "Thomas Henry Huxley", x = "Thomas Hunt Morgan")

```

## Sentiment Analysis (part 1 - 3)
The Sentiments datasets:

There are a variety of methods and dictionaries that exist for evaluating the opinion or emotion of the text.

AFFIN
bing
nrc

bing categorizes word in a binary fashion into positive or negative
nrc categorizes into positive, negative, anger, anticipation, disgust, fear, joy, sadness, suprise and trust.
AFFIN assigns a score between -5 and 5, with negative indicating negative sentiment, and 5 positive.

The function get_setiments() allows us to get the specific sentiments lexicon with the measures for each one.

```{r}
install.packages("textdata")
```


```{r}
library(tidytext)
library(textdata)

afinn <- get_sentiments("afinn")

afinn
```
Lets look at bing

```{r}

bing <- get_sentiments("bing")

bing

```
And lastly nrc

```{r}

nrc <- get_sentiments("nrc")

nrc

```
These libraries were created either using crowdourcing or cloud computing/ai like Amazon Mechanical Turk, or by labor of one of the authors, and then validated with crowdsourcing.

Lets look at the words with a joy score from NRC


```{r}
library(gutenbergr)
library(dplyr)
library(stringr)

darwin <- gutenberg_download(c(944, 1227, 1228, 2300), mirror = "https://mirror.csclub.uwaterloo.ca/gutenberg/")

tidy_books <- darwin %>%
  group_by(gutenberg_id) %>%
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

tidy_books
```
Lets add the book name instead if GID

```{r}
colnames(tidy_books)[1] <- "book"

tidy_books$book[tidy_books$book == 944] <- "The Voyage of the Beagle"
tidy_books$book[tidy_books$book == 1227] <- "The Expression of the Emotions in Man and Animals"
tidy_books$book[tidy_books$book == 1228] <- "On the Origin of Species By Means of Natural Selection"
tidy_books$book[tidy_books$book == 2300] <- "The Descent of Man, and Selection in Relation to Sex"

tidy_books

```
Now that we have a tidy format with one word per row, we are ready for sentiment analysis. First lets use NRC.

```{r}
nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

tidy_books %>% 
  filter(book == "The Voyage of the Beagle") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

```

We can also examine how sentiment changes throughout a work.

```{r}

library(tidyr)

Charles_Darwin_sentiment <- tidy_books %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

```
Now lets plot it

```{r}

library(ggplot2)

ggplot(Charles_Darwin_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

```

Lets compare the three sentiment dictions

There are several options for sentiment lexicons, you might want some more info on which is appropriate for your purpose. Here we will use all three of our dictionariesand examine how the sentiment changes across the arc of TVOTB.

```{r}
library(tidyr)

voyage <- tidy_books %>%
  filter(book == "The Voyage of the Beagle")

voyage

```
Lets again use interger division ('%/%') to define larger sections of the text that span multiple lines, and we can use the same pattern with 'count()', 'pivot_wider()', and 'mutate', to find the net sentiment in each of these sections of text. 

```{r}

affin <- voyage %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  voyage %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  voyage %>% 
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))
              ) %>%
      mutate(method = "NRC")) %>%
    count(method, index = linenumber %/% 80, sentiment) %>%
    pivot_wider(names_from = sentiment, 
                values_from = n,
                values_fill = 0) %>%
    mutate(sentiment = positive - negative)
    
```
We can now estimate the net sentiment (positive - negative) in each chunk of thenovel text for each lexion (dictionary). 

Lets bind them all together and visualize with ggplot

```{r}

bind_rows(affin, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
  
```

Lets look at the counts based on each dictionary

```{r}
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

```
```{r}

get_sentiments("bing") %>% 
  count(sentiment)

```

```{r}

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

```

This can be shown visually, and we can pipe straight into ggplot2
 
```{r}
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scale = "free_y") +
  labs(x = "Contribution to Sentiment", y = NULL)

```
Lets spot an anomoly in the dataset.

```{r}
custom_stop_words <- bind_rows(tibble(word = c("wild", "dark", "great", "like"), lexicon = c("custom")), stop_words)

custom_stop_words

```
Word Clouds!

We can see that tidy text mining and sentiment analysis works well with ggplot2, but having our data in tidy formate leads to other nice graphing techniques

Lets use the wordcloud package!!

```{r}
library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
  
```
Lets also look at comparison.cloud(), which may require turning the dataframe into a matrix.

We can change to matrix using the acast() function.

```{r}
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"), max.words = 100)

```
Looking at units beyond words

Lots of useful work can be done by tokenizing not the word level, but sometimes its nice to look at different units of text. For example, we can look beyond just unigrams.

Ex I am not having a good day.

```{r}

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter !=0) %>%
  slice_max(ratio, n = 1) %>%
  ungroup()

```

## N-grams (part 1 - 3)
So far we've only looked at single words, but many interesting (more accurate) analyses are based on the relationship between words

Lets look at some methods of tidytext for calculating and visualizing word relationships.


```{r}
library(dplyr)
library(tidytext)

darwin_books <- gutenberg_download(c(944, 1227, 1228, 2300), mirror = "https://mirror.csclub.uwaterloo.ca/gutenberg/")

colnames(darwin_books)[1] <- "book"

darwin_books$book[darwin_books$book == 944] <- "The Voyage of the Beagle"
darwin_books$book[darwin_books$book == 1227] <- "The Expression of the Emotions in Man and Animals"
darwin_books$book[darwin_books$book == 1228] <- "On the Origin of Species By Means of Natural Selection"
darwin_books$book[darwin_books$book == 2300] <- "The Descent of Man, and Selection in Relation to Sex"

darwin_bigrams <- darwin_books %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

darwin_bigrams

```
This data is still in tidytext format, and isn't structured as one-token-per-row. Each token is a bigram.

Counting and filtering n-gram

```{r}
darwin_bigrams %>%
  count(bigram, sort = TRUE)

```
Most of the common bigrams are stop-words. This can be good time to use tidyr's separate command which splits a column into multiple based on a delimiter. This will let us make a column for word one and word two. 

```{r}
library(tidyr)

bigrams_separated <- darwin_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered

```

New bigram counts

```{r}
bigram_counts <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts

```
We may also be interested in trigrams, which are three word combos

```{r}

trigrams <- darwin_books %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

trigrams

```

Lets analyze some bigrams

```{r}

bigrams_filtered %>%
  filter(word2 == "selection") %>%
  count(book, word1, sort = TRUE)

```
Lets again look at tf-idf across bigrams across Darwin works.

```{r}

bigram_tf_idf <- bigram_counts %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

```
```{r}

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(tf_idf, bigram, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf of bigrams", y = NULL)

```

Using bigrams to provide context in sentiment analysis

```{r}

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

```
By doing sentiment analysis on bigrams, we can examine how often sentiment-associated words are preceded by a modifier like "not" or other negating words.


```{r}

AFINN <- get_sentiments("afinn")

AFINN

```
We can examine the most frequent words that were preceded by "not", and associate with sentiments.

```{r}

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

```
Lets visualize 

```{r}

library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0 )) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number or occurences", y = "words preceded by \"not\"")

```
```{r}

negation_words <- c("not", "no", "never", "non", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

negated_words

```
Lets visualize the negation words

```{r}

negated_words %>%
  mutate(contribution = n * value,
         word2= reorder(paste(word2, word1, sep = "_"), contribution)) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 12, with_ties = FALSE) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("_.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurences") +
  coord_flip()

```

Visualize a network of bigrams with ggraph

```{r}

library(igraph)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

```
```{r}

library(ggraph)
set.seed(1234)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

```

We can also add directionality to this network

```{r}

set.seed(1234)

a <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

ggraph(bigram_graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a, end_cap = circle(0.7, 'inches')) + 
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label=name), vjust = 1, hjust = 1) +
  theme_void()

```

## Word Frequencies
A central question in text mining is how to quantify what a document is about. We can do this but looking at words that make up the document, and measuring term frequency.

There are a lot of words that may not be important, these are the stop words.

One way to remedy this is to look at inverse document frequency words, which decreases the weight for commonly used words and increases the weight for words that are not used very much.

Term frequency in Darwins works

```{r}

library(dplyr)
library(tidytext)

book_words <- gutenberg_download(c(944, 1227, 1228, 2300), mirror = "https://mirror.csclub.uwaterloo.ca/gutenberg/")

colnames(book_words)[1] <- "book"

book_words$book[book_words$book == 944] <- "The Voyage of the Beagle"
book_words$book[book_words$book == 1227] <- "The Expression of the Emotions in Man and Animals"
book_words$book[book_words$book == 1228] <- "On the Origin of Species By Means of Natural Selection"
book_words$book[book_words$book == 2300] <- "The Descent of Man, and Selection in Relation to Sex"

```

Now lets disect

```{r}
book_words <- book_words %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

book_words

```
```{r}

book_words$n <- as.numeric(book_words$n)

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

book_words

```
```{r}

book_words <- left_join(book_words, total_words)

book_words

```

You can see that the usual suspects are the most common words, but don't tell us anything about what the books topic is.

```{r}

library(ggplot2)

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) + 
  facet_wrap(~book, ncol = 2, scales = "free_y")
  
```

Zipf's Law 

The frequency that a words appears is inversely proportional to its rank when predicting a topic.

Lets apply Zipf's law to Darwin's work

```{r}

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank

```
```{r}

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

```
Lets us TF - IDF to find words for each document by decreasing the weight for commonly used words and increasing the weight for words that are not used very much in a collection of documents.

```{r}

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf

```
Lets look at terms with high tf-idf in Darwin's works

```{r}

book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

```
Lets look at a visualization for these high tf-idf words

```{r}

library(ggplot2)
library(dplyr)
library(forcats)

book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>% 
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

```
