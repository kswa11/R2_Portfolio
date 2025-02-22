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

# Boxplots

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

# Histograms

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



