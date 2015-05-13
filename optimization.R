require(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)

# Initialize the portfolio objectve 
port=portfolio.spec(assets=manager.names)


# set a constraint of no more than 2 short positions, and no more than 10 positions overall. 
port=add.constraint(port,'position_limit',max_pos_short=2,max_pos=10,message="can only have 2 short positons",enabled=TRUE)

# 
# port=add.constraint(port,type="full_investment",enabled=FALSE)
# port=add.constraint(port,type="long_only",enabled=FALSE)
# port=add.constraint(port,'leverage_exposure',enabled=FALSE)

#set range Weights for Specific Assets, namely the short.selling and the CTA Global to be between a long of 5% 
# of the  portfolio, and a short of -5% 
port=add.constraint(port,type="box",max=c(0.3,0.05,rep(0.3,9),0.05,1),min=c(-0.3,-0.05,rep(-0.3,9),-0.05,-1),enabled=TRUE)

# Add a return objective and a maximum Expected Shortfall Asset Contribution Objective

port=add.objective(port,type="risk_budget",name="ES",
                   min_prisk=0,max_prisk=0.5, enabled= TRUE)

port=add.objective(port,type="return",name="mean",target=0.001,enabled=TRUE                       
                     )


# rp <- random_portfolios(port, 10000, "sample")
opt2=optimize.portfolio.rebalancing(R=managers1, port, optimize_method="random", rebalance_on="quarters", training_period=10, trace=TRUE, search_size = 5000)
# opt2=optimize.portfolio(R=managers1, port, optimize_method="random",  training_period=10, trace=TRUE, search_size = 50000)

print(opt2)
par(mfrow=c(1,1))
# plot(opt2, main="Dollar Neutral Portfolio", risk.col="ES", neighbors=10)
# head(extractStats(opt2),4)
# chart.RiskReward(opt2,chart.assets=TRUE, risk.col = "ES")
chart.Weights(opt2, main=" Weights")
extractWeights(opt2)
chart.RiskBudget(opt2)
summary(opt2)

EF=create.EfficientFrontier(R=managers1,portfolio=port,type= "mean-ES",n.portfolios=25,match.col="ES",search_size=2000)
# summary(EF,digits=2)
chart.EfficientFrontier(EF, match.col="ES", tangent.line=TRUE,type="b",rf=0.001)

#Charting Efficient Frontier Weights 
par(mar=c(9, 5, 5, 3)-1, xpd=TRUE)
chart.EF.Weights(EF, colorset=bluemono, match.col="ES", cex.lab=0.6,legend.loc=NULL)
par(cex=.65)
legend("bottom", legend=colnames(managers1),text.width=4.5 , pt.cex=2,inset=-0.42, fill=bluemono, bty="n", ncol=4, cex=0.8)
par(cex=.8)

# 
# ef <- extractEfficientFrontier(object=opt2, match.col="ES", n.portfolios=15)
# summary(ef, digits=5)

# chart.Concentration(opt2,return.col="mean",chart.assets = TRUE,risk.col="ES",conc.type = "pct_contrib")
# points(0.02506101, 0.006236144 ,pch = 19, col = "dark red")

# Compute Returns
# ret.bt.opt <- do.call(cbind, lapply(opt2,
#                                     function(x) summary(x)$portfolio_returns))
# colnames(ret.bt.opt) <- c("min ES", "min ES RB", "min ES Eq RB")

#Chart Performance
# charts.PerformanceSummary(ret.bt.opt)
# weights =as.matrix(opt2$Weights)

