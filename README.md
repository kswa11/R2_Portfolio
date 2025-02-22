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
## EDA (part 1 - 5)
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



































