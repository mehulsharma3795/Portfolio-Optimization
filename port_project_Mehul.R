install.packages("ggplot2")
install.packages("tseries")
install.packages("fportfolio")
library("fPortfolio")
library("tseries")
library("ggplot2")

# Reading the data
dj <- readRDS("prices_DJIA.RDS")

# Viewing the structure of data
glimpse(dj)
str(dj)
head(dj)
tail(dj)
class(dj)

# Calculating Returns
djr <- Return.calculate(dj)
djr <- djr[(-1),]
head(djr)

#checking returns and risk

# Including library tseries for portfolio.optim function

mean_sd <- function(x) {
  c(mean = mean(x), sd = sd(x))
}

mean_ret <- sapply(djr,FUN= mean_sd)
opt_port <- portfolio.optim(djr)
head(opt_port)
port_weights <- opt_port$pw
names(port_weights) <- colnames(djr)
assets <-  colnames(djr)
df <- data.frame(port_weights)
bar <- ggplot(df, aes(x = "", y = port_weights, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("No Constraints Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + theme_minimal()



#selecting optimum weights
opt_weight <- port_weights[port_weights >= 0.01]
opt_ret <- opt_port$pm
opt_vol <- opt_port$ps
df2 <- data.frame(opt_weight)
bar <- ggplot(df2, aes(x = "", y = opt_weight, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("No Constraints Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + theme_minimal()


# Calculating optimum portfolio where target return is equal to 1.1x of mean returns and with max weight constraint of 10% for a constituent.
n.assets <- 30
reshigh <- rep(0.1,n.assets)
opt_port2 <- portfolio.optim(djr,pm= 1.1*mean(djr),reshigh = reshigh)
port2_weights <- opt_port2$pw
names(port2_weights) <- colnames(djr)
port2_ret <- opt_port2$pm
port2_sd <- opt_port2$ps
df2 <- data.frame(port2_weights)
bar <- ggplot(df2, aes(x = "", y = port2_weights, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("Target return 1.1% mean & 10% max weight Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + theme_minimal()

# Calculating mean of each column which will give us the return on a stock
stock_mean <- colMeans(djr)

# Create a grid of 50 target values starting from risk free rate upto maximum of stock mean
grid <- seq(from = 0.01, to = 0.02249647, length.out = 100)

# Create empty vectors to store means and deviations
efpm <- efsd <- rep(NA,length(grid))
  
# Create an empty matrix to store weights
efweights <- matrix(NA, 100, 30)

# Creating a portfolio for each of the return un the grid
for(i in 1:length(grid)) {
  opt <- portfolio.optim(x =djr, pm = grid[i])
  efpm[i] <- opt$pm
  efsd[i] <- opt$ps
  efweights[i, ] <- opt$pw
}


#Plotting the efficient frontier
plot(x=efsd,y=efpm)

#Creating portfolio with the least risk or minimum variance portfolio
weights_minvar <- efweights[efsd == min(efsd), ]
names(weights_minvar) = colnames(djr)
df3 <- data.frame(weights_minvar)
bar <- ggplot(df3, aes(x = "", y =weights_minvar, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("Minimum Variance Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + theme_minimal()


# Calculate the Sharpe ratio
vsr <- (efpm - 0.01) / efsd

# Create weights_max_sr as the portfolio with the maximum Sharpe ratio
weights_max_sr <- efweights[vsr == max(vsr),]
names(weights_max_sr) = colnames(djr)
df4 <- data.frame(weights_max_sr)
bar <- ggplot(df4, aes(x = "", y = weights_max_sr, fill=assets)) + geom_bar(width= 1, stat="identity") + ggtitle("Maximum Sharpe Ratio Portfolio Weights")+ theme(plot.title = element_text(hjust = 0.5)) 
pie <- bar + coord_polar("y", start=0)
pie + theme_minimal()


# Create bar plot of weights_minvar and weights_max_sr
par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
barplot(weights_minvar[weights_minvar > 0.01])
barplot(weights_max_sr[weights_max_sr > 0.01])

#Calculating VaR & ES for all portfolios created

#Creating two empty vectors
VaR <- rep(NA,100)
ES <-  rep(NA,100)

install.packages("fPortfolio")

# calculating VaR for 95% confidence interval 
for(i in 1:length(grid)){
    VaR[i] <-  (vpm[i] + (vpsd[i]*1.645))*100
    ES[i] <- cvarRisk(data=djr,weights=efweights[i,],alpha = 0.05)
}
# VaR minimum variance portfolio
min_var_ret <- vpm[vpsd == min(vpsd)]
Var_min_var <- (min_var_ret + min(vpsd)*1.645)*100
ES_min_var <- cvarRisk(djr,weights = efweights[efsd == min(efsd), ])
# VaR minimum max sharpe ratio portfolio
max_sr_ret <- vpm[vsr == max(vsr)]
Var_max_sr <- (max_sr_ret + 1.645*vpsd[vsr == max(vsr)])*100
ES_max_sr <- cvarRisk(djr,weights = efweights[vsr == max(vsr), ])

