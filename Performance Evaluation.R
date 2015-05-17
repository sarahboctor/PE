
# Load Packages 
library(PerformanceAnalytics)
library(ellipse)
library(tseries)
require(PortfolioAnalytics)
library(DEoptim)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
require("RColorBrewer")

# Set up the data for the Funds and the Indices  
data(edhec)
managers1 = edhec["2004::2006"]
loadPath = "C:\\Users\\Sarah\\Desktop\\R PROJECTS\\sarah\\"
dev.df = na.omit(read.csv(file=paste(loadPath, "msci eafe.csv", sep=""), 
                          header=TRUE, stringsAsFactors=FALSE))
em.df = na.omit(read.csv(file=paste(loadPath, "EEMfrom2003.csv", sep=""), 
                         header=TRUE, stringsAsFactors=FALSE))
sg.df = na.omit(read.csv(file=paste(loadPath, "small cap growth 2000.csv", sep=""), 
                         header=TRUE, stringsAsFactors=FALSE))
sv.df = na.omit(read.csv(file=paste(loadPath, "small cap value 2000.csv", sep=""), 
                         header=TRUE, stringsAsFactors=FALSE))
lg.df =na.omit( read.csv(file=paste(loadPath, "russell 1000 growth from march 2000.csv", sep=""), 
                         header=TRUE, stringsAsFactors=FALSE))
lv.df = na.omit(read.csv(file=paste(loadPath, "russell 1000 value from march 2000.csv", sep=""), 
                         header=TRUE, stringsAsFactors=FALSE))
# to produce zoo objects
rownames(lg.df) = lg.df$Date
rownames(lv.df) = lv.df$Date
rownames(sv.df) = sv.df$Date
rownames(sg.df) = sg.df$Date
rownames(dev.df) = dev.df$Date
rownames(em.df) = em.df$Date



em.df = em.df[-37, "Adj.Close", drop=FALSE]

dev.df = dev.df[, "Adj.Close", drop=FALSE]
sg.df = sg.df[, "Adj.Close", drop=FALSE]
sv.df = sv.df[, "Adj.Close", drop=FALSE]
lv.df = lv.df[, "Adj.Close", drop=FALSE]
lg.df = lg.df[, "Adj.Close", drop=FALSE]

dates.em = as.Date(rownames(em.df), format="%m/%d/%Y")
dates.sg = as.Date(rownames(sg.df), format="%m/%d/%Y")
dates.dev = as.Date(rownames(dev.df), format="%m/%d/%Y")
dates.sv = as.Date(rownames(sv.df), format="%m/%d/%Y")
dates.lg = as.Date(rownames(lg.df), format="%m/%d/%Y")
dates.lv = as.Date(rownames(lv.df), format="%m/%d/%Y")


Styles=cbind(dev.df,sg.df,sv.df,lv.df,lg.df)
colnames(Styles)=c("Ex-US","Small Growth","Small Value","Large Value","Large Growth")
Style=na.omit(CalculateReturns(xts(Styles, order.by=dates.lg)))
em=na.omit(CalculateReturns(xts(em.df,order.by=dates.em)))
colnames(em)="Emerging"
Managerz=xts(managers1,order.by=dates.em[-1])

#Append the index Values             
Managers=na.omit(merge(Managerz["2003::2006"],Style["2003::2006"],em["2003::2006"]))



# Create Optimal Portfolio Weights for the analysis period, to be then used as hypothetical
# portfolio
# Plot Returns of Assets

par(cex=0.86)
par(mfrow=c(1,1))

chart.CumReturns(Managers[,1:13], main="Cumulative Returns",
                 wealth.index=TRUE,col= brewer.pal(13, "Spectral"), legend.loc=NULL)
par(cex=.65)
legend("topleft", legend=manager.names,text.width=5.7 , pt.cex=2,inset=0, fill= brewer.pal(13, "Spectral"), bty="n", ncol=2, cex=0.8)
par(cex=.8)


# Initialize the portfolio objectve 
port=portfolio.spec(assets=manager.names)


# set a constraint of no more than 2 short positions, and no more than 10 positions overall. 
port=add.constraint(port,'position_limit',max_pos_short=2,max_pos=10,message="can only have 2 short positons",enabled=TRUE)

#set range Weights for  Assets, weights for any assets to not exceed 30% long/short 
# and  the short.selling and the CTA Global funds as 5% long/short 

port=add.constraint(port,type="box",max=c(0.3,0.05,rep(0.3,9),0.05,1),min=c(-0.3,-0.05,rep(-0.3,9),-0.05,-1),enabled=TRUE)

# Add a return objective of 0.1% monthly and a maximum Expected Shortfall asset budget of 0.5 
 

port=add.objective(port,type="risk_budget",name="ES",
                   min_prisk=0,max_prisk=0.5, enabled= TRUE)

port=add.objective(port,type="return",name="mean",target=0.001,enabled=TRUE )

# The Optimization Function
opt2=optimize.portfolio.rebalancing(R=managers1, port, optimize_method="random", rebalance_on="quarters", training_period=10, trace=TRUE, search_size = 5000)
chart.Weights(opt2, main=" Weights")
chart.RiskBudget(opt2)

summary(opt2)
print(opt2)
par(mfrow=c(1,1))

# Create an Efficient Froniter and Chart the Weights 
EF=create.EfficientFrontier(R=managers1,portfolio=port,type= "mean-ES",n.portfolios=25,match.col="ES",search_size=2000)

#Charting Efficient Frontier Weights 
par(mar=c(9, 5, 5, 3)-1, xpd=TRUE)
chart.EF.Weights(EF, colorset=bluemono, match.col="ES", cex.lab=0.6,legend.loc=NULL)
par(cex=.65)
legend("bottom", legend=colnames(managers1),text.width=4.5 , pt.cex=2,inset=-0.42, fill=bluemono, bty="n", ncol=4, cex=0.8)
par(cex=.8)



# Extract the Optimal Weights 
weights = as.matrix(extractWeights(opt2)[1,])
rownames(weights)="2004-01-31"

# Calculate Returns for the Hypothetical Portfolio (HP)

HP=Return.portfolio(R=managers1,weights=weights,geometric=TRUE,rebalance_on="quarters",value=1,verbose=FALSE)
colnames(HP)="Portfolio"

#Setting the Portfolio Benchmark to be the the SP 500 TR 
sp500=(managers["2004::2006",8])

#PERFORMANCE RELATIVE TO THE BENCHMARK 

HAM=merge(HP,sp500)
table.AnnualizedReturns(HAM,scale=12,Rf=0.0009,digits=3)

table.CalendarReturns(HAM,digits=1,as.perc=TRUE)


results=table.TrailingPeriods(HAM,periods= c( 6, 12, 18, 24, 30))

require("Hmisc")
textplot(format.df(results, na.blank=TRUE, numeric.dollar=FALSE,
                   cdec=rep(3,dim(result)[2])), rmar = 0.01, cmar = 0.5,
         max.cex=.9, halign = "center", valign = "bottom", row.valign="center",
         wrap.rownames=15, wrap.colnames=3, mar = c(0,0,3,0)+0.1)
title(main="Trailing Period Statistics")

Return.annualized.excess(HP,sp500,scale=12)
Return.annualized(sp500,scale=12)
Return.annualized(HP,scale=12)
charts.PerformanceSummary(HAM,Rf=0.001,main="Performance Summary", methods = "ModifiedES",legend.loc="topleft")
Return.cumulative(HP["2004"])
chart.CumReturns(HAM,legend.loc="left",main="Cumulative Return")
chart.RollingPerformance(HAM, width = 6,FUN = "Return.annualized", legend.loc="topleft",
                         ylim = NULL, main = "6-months Rolling Return" , fill = NA)
chart.RelativePerformance(HP,sp500,main = "Relative Performance",legend.loc="topleft",
                          xaxis = TRUE, colorset = (1:12))
mean(Return.relative(HP,sp500))




#OUT AND UNDERPERFORMANCE 

table.UpDownRatios(HP,sp500,digits=2)
UpDownRatios(HP,sp500,method=c("Number","Percent","Capture"), side = c("Up", "Down"))

table.ProbOutPerformance(HP,sp500,period_lengths=c(1, 3, 6, 9, 12, 18, 36))

a=UpsideFrequency(HAM,MAR=0.008)
b=DownsideFrequency(HAM,MAR=0.008)

c=rbind(a,b)
c

#

chart.CaptureRatioss(HP,sp500, main = "Capture Ratio", add.names = TRUE,
                    xlab = "Downside Capture", ylab = "Upside Capture",cex.legend = 1, cex.axis = 0.8, cex.main = 1, cex.lab = 1,
                    element.color = "darkgray", benchmark.color = "darkgray")
chart.Drawdown(HAM,legend.loc="topleft")


#  RISK ANALYSIS

Cash= (managers["2004::3006",10])
par(mfrow=c(1,1))

StdDev(HP)
table.SpecificRisk(HP,sp500,Rf = Cash)
HAM2=cbind(HAM,Style["2004::2006"])
head(HAM2)
#Chartng Rolling Standard Deviation, Rolling Mean, and Rolling Sharpe Ratios 

par(cex=.65)
charts.RollingPerformance(HAM2[,1:4],width=6,
                          Rf=Cash,
                          colorset=tim8equal,
                          main="Rolling 12-Month Performance",
                          legend.loc="bottomleft", box.col=NULL)


# legend("topleft", legend=colnames(HAM2),box.col=NULL,text.width=3,inset=-0.069, pt.cex=1,fill=tim8equal, bty="n", cex=0.5)
par(cex=.8)

MeanAbsoluteDeviation(HP) 
SharpeRatio(HP,Rf=Cash,FUN = "StdDev" )
DrawdownDeviation(HP)
DownsideDeviation(HP,MAR=sp500)
InformationRatio(HP,sp500,12)
SortinoRatio(HP,sp500)
chart.TimeSeries(apply.rolling(HAM, width = 6,gap = 2, FUN = "MeanAbsoluteDeviation",main="Rolling 6-months MAD", Rb=Cash))
chart.RollingPerformance(HAM,width = 6,FUN = "SharpeRatio.annualized",main = "Rolling 6-Month Sharpe Ratio",legend.loc="topleft")
chart.RollingPerformance(HAM,width = 6,FUN = "StdDev",main = "Rolling 6-months Standard Deviation",legend.loc="topleft")
chart.RollingPerformance(HAM,width = 6,FUN = "MeanAbsoluteDeviation",main = "Rolling 6-months MAD",legend.loc="topleft")
chart.RollingPerformance(HP,width = 6,FUN = "TrackingError",main = "Rolling 6-months Tracking Error",Rb=sp500,legend.loc="topleft")
chart.RollingPerformance(HP,width = 6,FUN = "InformationRatio",main = "Rolling 6-months Information Ratio",Rb=sp500,legend.loc="topleft")




apply.rolling(HP, width = 6,gap = 2, FUN = "StdDev", Rb=Cash)
apply.rolling(HP, width = 6,gap = 2, FUN = "SortinoRatio", Rb=sp500,Rf=Cash)
apply.rolling(HP, width = 6,gap = 2, FUN = "TrackingError", Rb=Cash)

TrackingError(HP,sp500,scale = 12)
TreynorRatio(HP,sp500,Rf = Cash,scale = 12,modified = TRUE)
MSquared(HP,Rb = sp500,Rf=Cash )
table.DownsideRisk(HAM,scale = 12,Rf = Cash,MAR=0.0085, digits = 3)
table.DownsideRiskRatio(HAM,MAR=0.0085,scale = 12,digits = 3)

table.Correlation 
M2Sortino(Ra =HP,Rb=sp500 )
maxDrawdown (R = HP)







# STYLE ANALYSIS


##
# estimate multiple factor model using loop b/c of unequal histories for the hedge funds
#

# initialize list object to hold regression objects
reg.list = list()
# initialize matrices and vectors to hold estimated betas,
# residual variances, and R-square values from
# fitted factor models
Betas = matrix(0, length(manager.names), length(index.names))
colnames(Betas) = index.names
rownames(Betas) = manager.names
Alphas = ResidVars = R2values = rep(0, length(manager.names))
names(Alphas) = names(ResidVars) = names(R2values) = manager.names

# loop over all assets and estimate time series regression
for (i in manager.names) {
    reg.df = na.omit(Managers.df[, c(i, index.names)])
    fm.formula = as.formula(paste(i,"~", ".", sep=" "))
    fm.fit = lm(fm.formula, data=reg.df)
    fm.summary = summary(fm.fit)
    reg.list[[i]] = fm.fit
    Alphas[i] = coef(fm.fit)[1]
    Betas[i, ] = coef(fm.fit)[-1]
    ResidVars[i] = fm.summary$sigma^2
    R2values[i] =  fm.summary$r.squared
}


# summarize factor model results
reg.results = cbind(Betas, sqrt(ResidVars), R2values)
colnames(reg.results)[7] = "ResidSD"
reg.results

#
# compute factor model covariance matrix
#

# risk factor sample covariance matrix
cov.factors = var(Managers.df[, index.names])
# FM covariance matrix
cov.fm = Betas%*%cov.factors%*%t(Betas) + diag(ResidVars)
# FM correlation matrix
cor.fm = cov2cor(cov.fm)
# plot correlations using plotcorr() from ellipse package
rownames(cor.fm) = colnames(cor.fm)
ord <- order(cor.fm[1,])
ordered.cor.fm <- cor.fm[ord, ord]
plotcorr(cor.fm, col=cm.colors(11)[5*cor.fm + 6])

# compute mu.fm and sigma.fm for all assets. Set alpha = 0 for mu.fm

mu.factors = colMeans(Managers[, index.names])
mu.fm = Betas%*%mu.factors
sigma.fm = sqrt(diag(cov.fm))

# portfolio factor model
#

# equally weighted portfolio of 6 hedge funds
w.vec =as.numeric(weights)

# portfolio returns
r.p = as.matrix(HP)

r.p.zoo = zoo(HP, as.yearmon(rownames(HP)))


# portfolio factor model
alpha.p = as.numeric(crossprod(Alphas,w.vec))
beta.p = t(Betas)%*%w.vec
var.p.systematic = t(beta.p)%*%cov.factors%*%beta.p
var.p.resid = t(w.vec)%*%diag(ResidVars)%*%w.vec
var.fm.p = var.p.systematic + var.p.resid
var.fm.p = as.numeric(var.fm.p)
r.square.p = as.numeric(var.p.systematic/var.fm.p)
fm.p = c(alpha.p, beta.p, sqrt(var.fm.p), r.square.p)
names(fm.p) = c("intercept", index.names, "sd", "r-squared")
fm.p
fm=c(beta.p)
names(fm)=index.names


par(oma = c(1, 1, 1, 1))  # Outside margins: b, l, t, r
par(mar = c(4, 5, 2, 1))  # Sets plot margins
barplot(fm, horiz  = TRUE,
        las    = 1,  # Orientation of axis labels
        col    = brewer.pal(12, "Set3"),
        border = NA,  # No borders on bars
        main   = "Style Contributions to the Composite Portfolio",
        xlab   = "Factor Beta")

# factor model residuals - need to extract over truncated sample
# will be use later in factor risk budgeting
fm.resid = rep(0, nrow(HP))
smpl = rownames(HP)
for (i in manager.names) {
    reg.resid = residuals(reg.list[[i]])
    fm.resid = fm.resid + w.vec[i]*reg.resid[smpl]
}





-----------------
    
    # RISK ANALYSIS AND RISK DECOMPOSITION 
    
    
# VaR and ETL for portfolio
#

# note: portfolio returns have to use truncated sample due to missing values
# (unequal histories) in HAM5 and HAM6
# Normal VaR and Normal FM VaR
VaR(HP, p=0.95, method="gaussian")
VaR(HP, p=0.99, method="gaussian")



mu.fm.p = crossprod(beta.p, mu.factors)
sd.fm.p = sqrt(var.fm.p)
normalVaR(mu.fm.p, sd.fm.p, tail.prob=0.05)
normalVaR(mu.fm.p, sd.fm.p, tail.prob=0.01)

# Cornish-Fisher (modified VaR)
VaR(HP, p=0.95, method="modified")
VaR(HP, p=0.99, method="modified")
# Historical VaR
VaR(HP, p=0.95, method="historical")
VaR(HP, p=0.99, method="historical")

# Normal ES and Normal FM ES
ES(HP, p=0.95, method="gaussian")
ES(HP, p=0.99, method="gaussian")
normalES(mu.fm.p, sd.fm.p, tail.prob=0.05)
normalES(mu.fm.p, sd.fm.p, tail.prob=0.01)

# Cornish-Fisher (modified ES)
ES(HP, p=0.95, method="modified")
ES(HP, p=0.99, method="modified")
# Historical ES
ES(HP, p=0.95, method="historical")
ES(HP, p=0.99, method="historical")


################################################################################
# Factor Risk Budgeting
################################################################################

#
# risk factor contribution to standard deviation
par(mfrow=c(1,1))
# Example: factor SD decomposition for Global.Macro
factor.sd.decomp.Global.Macro = factorModelFactorSdDecomposition(Betas["Global.Macro",],
                                                         cov.factors, ResidVars["Global.Macro"])

factor.sd.decomp.Global.Macro
# 
# # loop over all assets and store results in list
factor.sd.decomp.list = list()
for (i in manager.names) {
    factor.sd.decomp.list[[i]] = factorModelFactorSdDecomposition(Betas[i,],
                                                                  cov.factors, ResidVars[i])
}
# add portfolio factor SD decomposition to list
factor.sd.decomp.list[["portfolio"]] = factorModelFactorSdDecomposition(beta.p,
                                                                   cov.factors, var.p.resid)
names(factor.sd.decomp.list)
# 
# stacked bar charts of percent contributions to SD
getCSD = function(x) {
    x$cr.fm
}
cr.sd = sapply(factor.sd.decomp.list, getCSD)
rownames(cr.sd) = c(index.names, "residual")

par(oma = c(1, 1, 1, 1))  # Outside margins: b, l, t, r
par(mar = c(4, 5, 2, 1))  # Sets plot margins
barplot(cr.sd, main="Factor Contributions to SD",
        horiz  = TRUE,
#         legend=NULL,
        cex.names=0.5,
        las    = 1,
#         names.arg=c("CA","CTA","DS","EM","EN","Event","FIA","GM","LSE","MA","RV","SS","FoF"),
        xlab="Percent Contribution",
        legend=T, 
        col=brewer.pal(12, "Set3"))
        


# #
# risk factor contribution to ETL
#

# combine fund returns, factor returns and residual returns
tmpData = cbind(Managers.df[,1], Managers.df[,index.names],
                residuals(reg.list[[1]])/sqrt(ResidVars[1]))
colnames(tmpData)[c(1,5)] = c(manager.names[1], "residual")
factor.es.decomp.Global.Macro = factorModelFactorEsDecomposition(tmpData, Betas[1,],
                                                         ResidVars[1], tail.prob=0.05)
names(factor.es.decomp.Global.Macro)
factor.es.decomp.Global.Macro

# compute decomposition in loop
factor.es.decomp.list = list()
for (i in manager.names) {
    # check for missing values in fund data
    idx = which(!is.na(Managerz[,i]))
    tmpData = cbind(Managerz[idx,i], Managerz[idx,index.names],
                    residuals(reg.list[[i]])/sqrt(ResidVars[i]))
    colnames(tmpData)[c(1,5)] = c(manager.names[i], "residual")
    factor.es.decomp.list[[i]] = factorModelFactorEsDecomposition(tmpData, Betas[i,],
                                                                  ResidVars[i], tail.prob=0.05)
}
# add portfolo retsults - need factor model residuals
tmpData = cbind(HP,Managerz[rownames(HP),index.names],
                fm.resid/sqrt(as.numeric(var.p.resid)))
colnames(tmpData)[c(1,8)] = c("portfolio", "residual")
factor.es.decomp.list[["portfolio"]] = factorModelFactorEsDecomposition(tmpData, beta.p,
                                                                   var.p.resid, tail.prob=0.05)

# stacked bar charts of percent contributions to SD
getCETL = function(x) {
    x$cES.fm
}
# report as positive number
cr.etl = sapply(factor.es.decomp.list, getCETL)
rownames(cr.etl) = c(index.names, "residual")
barplot(cr.etl, main="Factor Contributions to ETL",
               horiz  = TRUE,
        #         legend=NULL,
        cex.names=0.5,
        las    = 1,
        #         names.arg=c("CA","CTA","DS","EM","EN","Event","FIA","GM","LSE","MA","RV","SS","FoF"),
        xlab="Percent Contribution",
        legend=T, 
        col=brewer.pal(12, "Set3"))

#
# illustrate with graphic. Plot fund returns, VaR line and factor returns. Identify
# those periods when fund return is less than its VaR, then look at where the
# factor returns are and compute average
#

HP.df=as.data.frame(HP[-36])
tmpData = cbind(HP.df, Managers.df[,index.names],
                residuals(reg.list[[1]])/sqrt(ResidVars[1]))
colnames(tmpData)[c(1,7)] = c("Portfolio", "residual")
# create zoo data for time series plots
tmpData.zoo = zoo(tmpData, as.Date(rownames(tmpData)))

# find those observations less than VaR
VaR.fm = quantile(tmpData[, 1], prob=0.05)
idx = which(tmpData[, 1] <= VaR.fm)
rownames(tmpData)[idx]
 # average factor data at obvs idx
mcETL.Ex.US = mean(tmpData[idx, 2])
mcETL.Small.Growth = mean(tmpData[idx, 3])
mcETL.Small.Value = mean(tmpData[idx, 4])
mcETL.residual = mean(tmpData[idx, 7])

# mcETL plot for EDHEC.LS.EQ
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(tmpData.zoo[,1], type="b", main="Portfolio returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.fm, lwd=2, col="red")
points(tmpData.zoo[idx, 1], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(tmpData.zoo[,2], type="b", main="Mean of Ex-US Index when Portfolio <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=mcETL.Ex.US , lwd=2, col="red")
points(tmpData.zoo[idx, 2], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for Small Growth
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(tmpData.zoo[,1], type="b", main="Portfolio returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.fm, lwd=2, col="red")
points(tmpData.zoo[idx, 1], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(tmpData.zoo[,3], type="b", main="Mean of Small.Growth when Portfolio <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=mcETL.Small.Growth, lwd=2, col="red")
points(tmpData.zoo[idx, 3], type="p", pch=16, col="red")
par(mfrow=c(1,1))



# mcETL plot for residual
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(tmpData.zoo[,1], type="b", main="Global.Macro returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.fm, lwd=2, col="red")
points(tmpData.zoo[idx, 1], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(tmpData.zoo[, 8], type="b", main="Mean of Standardized Residual when Global.Macro <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=mcETL.residual, lwd=2, col="red")
points(tmpData.zoo[idx, 8], type="p", pch=16, col="red")
par(mfrow=c(1,1))

################################################################################
# portfolio risk budgeting
################################################################################

#
# fund contribution to portfolio SD
#
# compute with sample covariance matrix (pairwise complete obvs)
cov.sample = cov(Managers.df[,manager.names],
                 use="pairwise.complete.obs")

port.sd.decomp.sample = portfolioSdDecomposition(w.vec, cov.sample)
names(port.sd.decomp.sample)
chart.Histogram(HP,methods = c("add.density", "add.rug","add.risk"),show.outliers=TRUE)
chart.Histogram(sp500,methods = c("add.density", "add.rug","add.risk"),show.outliers=TRUE)
chart.BarVaR(HP, width = 0, gap = 6, methods ="HistoricalES",show.greenredbars=TRUE) 

port.sd.decomp.sample

# show bar chart
names=as.vector(manager.names)
par(cex=0.65)
par(oma = c(1, 1, 1, 1))  # Outside margins: b, l, t, r
par(mar = c(4, 5, 2, 1))  # Sets plot margins
barplot(port.sd.decomp.sample$pcsd.p,
        main="Fund Percent Contributions to Portfolio SD",
        horiz  = TRUE,
#                    axisnames=FALSE,
                las    = 1,  # Orientation of axis labels,
cex.names=0.9,
names.arg=c("CA","CTA","DS","EM","EN","Event","FIA","GM","LSE","MA","RV","SS","FoF"),
        ylab="Percent Contribution", 
        col=brewer.pal(6, "Greens"))


# legend("topleft",legend=c( "Convertible.Arbitrage" , "CTA.Global"      ,       "Distressed.Securities" 
#                            ,"Emerging.Markets"    ,   "Equity.Market.Neutral" , "Event.Driven"          
#                            ,"Fixed.Income.Arbitrage" ,"Global.Macro"   ,        "Long.Short.Equity"     
#                            ,"Merger.Arbitrage"   ,    "Relative.Value"  ,       "Short.Selling"         
#                            ,"Funds.of.Funds"))



# compute with factor model covariance matrix
port.sd.decomp.fm = portfolioSdDecomposition(w.vec, cov.fm)
port.sd.decomp.fm

#
# fund contribution to portfolio ETL
#

# use ES() function in PerformanceAnalytics package

# need to remove missing values prior to computation
port.ES.decomp = ES(na.omit(Managers.df[,manager.names]),
                    p=0.95, method="historical",
                    portfolio_method = "component",
                    weights = w.vec)
port.ES.decomp

port.ES.decomp = portfolioEsDecomposition(na.omit(Managers.df[,manager.names]),
                                          w.vec, tail.prob=0.05)
names(port.ES.decomp)
port.ES.decomp

# show bar chart
barplot(port.ES.decomp$PCES,
        main="Fund Percent Contributions to Portfolio ETL",
        horiz  = TRUE,
        #                    axisnames=FALSE,
        las    = 1,  # Orientation of axis labels,
        cex.names=0.9,
        names.arg=c("CA","CTA","DS","EM","EN","Event","FIA","GM","LSE","MA","RV","SS","FoF"),
        ylab="Percent Contribution", 
        col=brewer.pal(6, "Greens"))

## graphical illustration of func marginal contribution to portfolio ETL
# find those observations less than VaR

nrow(HP)
VaR.p = quantile(HP, prob=0.05)
idx = which(HP.df <= VaR.p)
rownames(HP.df)[idx]
mcES = port.ES.decomp$MCES

# mcETL plot for Global.Macro
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(HP, type="b", main="Portfolio Returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.p, lwd=2, col="red")
points(HP[idx], type="p", pch=16, col="green")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(Managers[, "Global.Macro"], type="b", main="Mean of Global.Macro when PORT <= 5% VaR",
         col="blue", ylab="Returns",ylim=c(-0.02,0.03))
abline(h=0)
abline(h=-mcES[1,"Global.Macro"], lwd=2, col="red")
points(Managers[idx, "Global.Macro"], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for Emerging
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(HP, type="b", main="Portfolio Returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.p, lwd=2, col="red")
points(HP[idx], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(Managers[, "Emerging.Markets"], type="b", main="Mean of Emerging when PORT <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=-mcES[,"Emerging.Markets"], lwd=2, col="red")
points(Managers[idx, "Emerging.Markets"], type="p", pch=16, col="red")
par(mfrow=c(1,1))


################################################################################
# factor model Monte Carlo
################################################################################

# examime distribution of residuals
hist(residuals(reg.list$Global.Macro))
qqnorm(residuals(reg.list$Global.Macro))
qqline(residuals(reg.list$Global.Macro))

#
# FMMC with normal distribution for factor model residuals
#

# resample from historical factors
n.boot = 5000
# set random number sed
set.seed(123)
# reshuffled indices with replacement
bootIdx = sample(nrow(Managers.df),  n.boot, replace=TRUE)
# resampled factor data
factorDataBoot.mat = as.matrix(Managers.df[bootIdx, index.names])

# FMMC using normal distribution for residuals and alpha = 0
returns.boot = matrix(0, n.boot, length(manager.names))
resid.sim = matrix(0, n.boot, length(manager.names))
colnames(returns.boot) = colnames(resid.sim) = manager.names
for (i in manager.names) {
    returns.fm = factorDataBoot.mat%*%Betas[i, ]
    resid.sim[, i] = rnorm(n.boot,sd=sqrt(ResidVars[i]))
    returns.boot[, i] = returns.fm + resid.sim[, i]
}
# compute FMMC portfolio returns and factor model residuals
HP.boot = returns.boot%*%w.vec
resid.fm.p = resid.sim%*%w.vec

#
# compute factor contributions to ETL using FMMC sample
#

# compute decomposition in loop
factor.es.decomp.list = list()
for (i in manager.names) {
    tmpData = cbind(returns.boot[, i], factorDataBoot.mat,
                    resid.sim[, i]/sqrt(ResidVars[i]))
    colnames(tmpData)[c(1,5)] = c(manager.names[i], "residual")
    factor.es.decomp.list[[i]] = factorModelFactorEsDecomposition(tmpData, Betas[i,],
                                                                  ResidVars[i], tail.prob=0.05)
}
# add portfolo retsults - need factor model residuals
tmpData = cbind(HP.boot, factorDataBoot.mat,
                resid.fm.p/sqrt(as.numeric(var.p.resid)))
colnames(tmpData)[c(1,5)] = c("PORT", "residual")
factor.es.decomp.list[["PORT"]] = factorModelFactorEsDecomposition(tmpData, beta.p,
                                                                   var.p.resid, tail.prob=0.05)

# stacked bar charts of percent contributions to SD
getCETL = function(x) {
    x$cES.fm
}
# report as positive number
cr.etl = sapply(factor.es.decomp.list, getCETL)
rownames(cr.etl) = c(index.names, "residual")
barplot(cr.etl, main="Factor Contributions to ETL",
        legend.text=T, args.legend=list(x="topleft"),
        col=brewer.pal(12,"Set2"))


#
# compute fund contributions to ETL using FMMC sample
#

port.ES.decomp.fmmc = portfolioEsDecomposition(returns.boot,
                                               w.vec, tail.prob=0.05)
port.ES.decomp.fmmc

# show bar chart
barplot(port.ES.decomp.fmmc$PCES,
        main="Fund Percent Contributions to Portfolio ETL",
        horiz  = TRUE,
        #                    axisnames=FALSE,
        las    = 1,  # Orientation of axis labels,
        cex.names=0.9,
        names.arg=c("CA","CTA","DS","EM","EN","Event","FIA","GM","LSE","MA","RV","SS","FoF"),
        ylab="Percent Contribution", 
        col=brewer.pal(6, "Greens"))

# Conducting Style Analysis 

# add Cash to the asset classes

Styles=cbind(Style["2004::2006",],Cash)

nrow(Styles)

