library(ggplot2)

## Question 1
df = read.csv("Global-Temperature-R.csv")

# creating the linear model 
model = lm(Ave.Global.Temp.in.C ~ Year, data = df)

# display the summary of the model 
summary(model)

# creating a line plot of the data 
ggplot(df, aes(x = Year, y = Ave.Global.Temp.in.C)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "green", se = FALSE) +
  ggtitle("Global Average Temperature (1950 - Present)") +
  xlab("Year") +
  ylab("Average Global Temperature (°C)") +
  theme_minimal()

## Question 2

firstTemp = df$Ave.Global.Temp.in.C[1]
lastTemp = df$Ave.Global.Temp.in.C[nrow(df)]

totalIncrease = lastTemp - firstTemp

numOfYears = df$Year[nrow(df)] - df$Year[1]
averageIncrease = totalIncrease / numOfYears

## Question 3 

# calculating the yearly temperature change 
df$TempChange = c(NA, diff(df$Ave.Global.Temp.in.C))
head(df)

# average yearly change
decade_change = aggregate(TempChange ~ Decade, data = df, FUN = function(x) mean(x, na.rm = TRUE))

# trying to rename my columns so I can create the line plot 

str(decade_change)
colnames(decade_change)
colnames(decade_change)[colnames(decade_change) == "TempChange"] <- "Avg_Change"
head(decade_change)

# line plot
ggplot(decade_change, aes(x = factor(Decade), y = Avg_Change)) +
  geom_line(aes(group = 1), color = "purple", linewidth = 1) +   
  geom_point(color = "purple", size = 3) +                       
  ggtitle("Average Yearly Temperature Change per Decade") +
  xlab("Decade") +
  ylab("Average Yearly Change (°C)") +
  theme_minimal()

## Question 4

# the year with the highest temp
maxTemp <- df[which.max(df$Ave.Global.Temp.in.C), ]

# the year with the lowest temp 
minTemp <- df[which.min(df$Ave.Global.Temp.in.C), ]

# plot
ggplot(df, aes(x = Year, y = Ave.Global.Temp.in.C)) +
  geom_line(color = "black") +
  geom_point(aes(x = maxTemp$Year, y = maxTemp$Ave.Global.Temp.in.C), color = "pink", size = 3) +
  geom_point(aes(x = minTemp$Year, y = minTemp$Ave.Global.Temp.in.C), color = "lightblue", size = 3) +
  ggtitle("Global Average Temperature by Year") +
  xlab("Year") +
  ylab("Average Temperature (°C)") +
  theme_minimal()
