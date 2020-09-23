
# A. Import datasets
Fama <- read.csv("~/Downloads/F-F_Research_Data_Factors 3.csv",skip=3)
Manager <- read.csv("~/Downloads/PM.csv")
library(ggplot2) 

#Full Sample 

Data1 <- Fama[943:1122,]
View(Data1)

row.names(Data1) <- NULL
row.names(Manager) <- NULL

for (i in 1:4){
  Data1[,i+1] <- as.numeric(as.character(Data1[,i+1]))
}
for (i in 1:2){
  Manager[,i+1] <- as.numeric(as.character(Manager[,i+1]))
}

# B. Make a scatter plot
ER_Manager1 <- Manager$PM1-Data1$RF
ER_Manager2 <- Manager$PM2-Data1$RF

Mkt_Excess <- Data1$Mkt.RF
Manager1 <- Manager$PM1
Manager2 <- Manager$PM2

Data2 <- data.frame(cbind(Manager[,1:3],Data1$Mkt.RF))

View(Data2)

#Create scatterplot
gg <- ggplot(Data2, aes(x=Data1.Mkt.RF, y=ER_Manager1, ER_Manager2)) + geom_point() + labs(title="Scatterplot", x="Market Return", y="Manager")  # add axis lables and plot title.
print(gg)

#Create boxplot
boxplot(x=Manager$PM1, y=Manager$PM2)


#Create average
Mean_PM1 <- mean(Manager$PM1)
Mean_PM2 <- mean(Manager$PM2)
print(Mean_PM1)
#1.131389
print(Mean_PM2)
#0.8183333

#Create SD
SD_PM1 <- sd(Manager$PM1)
SD_PM2 <- sd(Manager$PM2)
print(SD_PM1)
#6.07423
print(SD_PM2)
#4.438485


# Sharpe ratio
SR_PM1 <- mean(Manager$PM1)/sd(Manager$PM1)
SR_PM2 <- mean(Manager$PM2)/sd(Manager$PM2)
print(SR_PM1)
# 0.1862605
print(SR_PM2)
# 0.1843722


# Information ratio using market portfolio as benchmark

for (i in 1:4){
  Data1[,i+1] <- as.numeric(as.character(Data1[,i+1]))
}
Mkt_Return <- Data1$Mkt.RF+Data1$RF

IR <- function(a,b,c){
  # a = portfolio return; b = benchmark return; c = frequency
  tracking_error <- sqrt(sum((a - b)^2) / (length(a) - 1)) * sqrt(c)
  information_ratio <- mean(a-b)/tracking_error 
  return(information_ratio)
}
Manager1_IR <- IR(Manager$PM1,Mkt_Return,12)
Manager2_IR <- IR(Manager$PM2,Mkt_Return,12)
print(Manager1_IR)
# 0.03121959
print(Manager2_IR)
# -0.0005103072

# Maximum drawdown
MDD <- function(d){
  # d = time series of returns of a portfolio without % sign
  d_temp <- cbind(d, "index" = 1)
  d_temp[1,2] <- 100 + d_temp[1,1]
  for (i in 2:length(d)){
    d_temp[i, "index"] <- d_temp[i-1, "index"] * (100 + d_temp[i, 1]) / 100
  }
  return((min(d_temp[,2]) - max(d_temp[,2])) / max(d_temp[,2]))
}
Manager1_MDD <- MDD(Manager$PM1)
Manager2_MDD <- MDD(Manager$PM2)
print(Manager1_MDD)
# -0.8929424
print(Manager2_MDD)
# -0.8158294

# B. Evidence outperform market?

Data3 <- data.frame(cbind(Manager[,1:3],Data1[,2:5]))
View(Data3)

X1 <- Data3$Mkt.RF+Data3$RF
X2 <- Data3$SMB
X3 <- Data3$HML
Y1 <- Data3$PM1


Excess_Model <- lm(PM1 ~ X1 + X2 + X3, Data3)
summary(Excess_Model)


Data3 <- data.frame(cbind(Manager[,1:3],Data1[,2:5]))

X1 <- Data3$Mkt.RF+Data3$RF
X2 <- Data3$SMB
X3 <- Data3$HML
Y2 <- Data3$PM2


Excess_Model <- lm(PM2 ~ X1 + X2 + X3, Data3)
summary(Excess_Model)

#3. C Statistical Test

#Average Return
Mkt <- Data1$Mkt.RF+Data1$RF
print(t.test(Manager1-Mkt, mu=0))


Mkt <- Data1$Mkt.RF+Data1$RF
print(t.test(Manager2-Mkt, mu=0))

