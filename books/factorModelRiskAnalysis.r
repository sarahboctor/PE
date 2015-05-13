# factorModelRiskAnalysis.r
# Examples for Scottish Financial Risk Academy Factor Model tutorial
# author: Eric Zivot
# created: January 10, 2011
# updated: March 14, 2011

# set output options
options(width = 70, digits=4)

# load required packages
library(ellipse)
library(fEcofin)                # various data sets
library(PerformanceAnalytics)   # performance and risk analysis functions
library(tseries)
library(xts)
library(zoo)

# source in factorAnalytics functions
loadpath = "C:\\Users\\ezivot\\Documents\\R Development\\factorAnalytics\\"
source(paste(loadpath, "normalES.r", sep=""))
source(paste(loadpath, "normalVaR.r", sep=""))
source(paste(loadpath, "factorModelFactorSdDecomposition.r", sep=""))
source(paste(loadpath, "factorModelFactorEsDecomposition.r", sep=""))
source(paste(loadpath, "portfolioSdDecomposition.r", sep=""))
source(paste(loadpath, "portfolioEsDecomposition.r", sep=""))


################################################################################
# managers data set
################################################################################
?managers
# load hypothetical long-short equity asset managers data
# from PerformanceAnalytics package
data(managers)
class(managers)
start(managers)
end(managers)
colnames(managers)
# remove data prior to 1997-01-01 b/c HF index is not observed
managers = managers["1997::2006"]

#
# graphical analysis of manager data
#

# plot hedge fund returns over time to illustrate monotone missing data
# use plot.zoo() to create a multiple panel time series plot
# panel function to put horizontal lines at zero in each panel
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
# plot hedge fund data
plot.zoo(managers[, 1:6], main="Hedge Fund Returns",
         plot.type="multiple", type="h", lwd=2, col="blue",
         panel=my.panel)
# plot risk factor data         
plot.zoo(managers[, 7:10], main="Risk Factor Returns",
         plot.type="multiple", type="h", lwd=2, col="blue",
         panel=my.panel)

# plot cumulative returns using PerformanceAnalytics
# function chart.CumReturns()
# hedge funds
chart.CumReturns(managers[,1:6], main="Cumulative Returns",
                 wealth.index=TRUE, legend.loc="topleft")
# risk factors
chart.CumReturns(managers[,7:9], main="Cumulative Returns",
                 wealth.index=TRUE, legend.loc="topleft")




cor.managers = cor(managers, use="pairwise.complete.obs")
ord <- order(cor.managers[1,])
ordered.cor.managers <- cor.managers[ord, ord]
plotcorr(ordered.cor.managers, col=cm.colors(11)[5*ordered.cor.managers + 6])

################################################################################
# Macroeconomic factor model for 6 hedge fund managers
################################################################################

#
# create data frame for regression analysis
#

# with xts objects, you extract data using coredata() and you extract
# dates with index()
managers.df = as.data.frame(coredata(managers))
rownames(managers.df) = as.character(index(managers))
# subtract "US 3m TR" (risk free rate) from all returns. note: apply() changes
# managers.df to class "matrix" to coerce result back to data.frame
managers.df = apply(managers.df, 2,
                    function(x) {x - managers.df[,"US 3m TR"]})
managers.df = as.data.frame(managers.df)
# remove US 3m TR from data.frame
managers.df = managers.df[, -10]
manager.names = colnames(managers.df)[1:6]
# eliminate spaces in factor names which cause problems later if not removed
factor.names = c("EDHEC.LS.EQ", "SP500.TR", "US.10Y.TR")
colnames(managers.df)[7:9] = colnames(managers)[7:9] = factor.names
# truncated data set to be used later for graphs
managers.zoo = as.zoo(na.omit(managers[, manager.names]))

#
# compute descriptive statistics
#

# fund data
table.Stats(managers[, manager.names])

# factor data
table.Stats(managers[, factor.names])

#
# estimate multiple factor model using loop b/c of unequal histories for the hedge funds
#

# initialize list object to hold regression objects
reg.list = list()
# initialize matrices and vectors to hold estimated betas,
# residual variances, and R-square values from
# fitted factor models
Betas = matrix(0, length(manager.names), length(factor.names))
colnames(Betas) = factor.names
rownames(Betas) = manager.names
Alphas = ResidVars = R2values = rep(0, length(manager.names))
names(Alphas) = names(ResidVars) = names(R2values) = manager.names

# loop over all assets and estimate time series regression
for (i in manager.names) {
 reg.df = na.omit(managers.df[, c(i, factor.names)])
 fm.formula = as.formula(paste(i,"~", ".", sep=" "))
 fm.fit = lm(fm.formula, data=reg.df)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas[i, ] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
}

# examine the elements of reg.list  - they are lm objects!
names(reg.list)
class(reg.list$HAM1)
reg.list$HAM1
summary(reg.list$HAM1)

# plot actual vs. fitted over time for HAM1
# use chart.TimeSeries() function from PerformanceAnalytics package
dataToPlot = cbind(fitted(reg.list$HAM1), na.omit(managers.df$HAM1))
colnames(dataToPlot) = c("Fitted","Actual")
chart.TimeSeries(dataToPlot, main="FM fit for HAM1",
                 colorset=c("black","blue"), legend.loc="bottomleft")

# summarize factor model results
reg.results = cbind(Betas, sqrt(ResidVars), R2values)
colnames(reg.results)[4] = "ResidSD"
reg.results

#
# compute factor model covariance matrix
#

# risk factor sample covariance matrix
cov.factors = var(managers.df[, factor.names])
# FM covariance matrix
cov.fm = Betas%*%cov.factors%*%t(Betas) + diag(ResidVars)
# FM correlation matrix
cor.fm = cov2cor(cov.fm)
# plot correlations using plotcorr() from ellipse package
rownames(cor.fm) = colnames(cor.fm)
ord <- order(cor.fm[1,])
ordered.cor.fm <- cor.fm[ord, ord]
plotcorr(ordered.cor.fm, col=cm.colors(11)[5*ordered.cor.fm + 6])

#
# portfolio factor model
#

# equally weighted portfolio of 6 hedge funds
w.vec = rep(1,6)/6
names(w.vec) = manager.names
w.vec

# portfolio returns. Note: need to eliminate NA values from HAM5 and HAM6
r.p = as.matrix(na.omit(managers.df[, manager.names]))%*%w.vec
r.p.zoo = zoo(r.p, as.Date(rownames(r.p)))


# portfolio factor model
alpha.p = as.numeric(crossprod(Alphas,w.vec))
beta.p = t(Betas)%*%w.vec
var.p.systematic = t(beta.p)%*%cov.factors%*%beta.p
var.p.resid = t(w.vec)%*%diag(ResidVars)%*%w.vec
var.fm.p = var.p.systematic + var.p.resid
var.fm.p = as.numeric(var.fm.p)
r.square.p = as.numeric(var.p.systematic/var.fm.p)
fm.p = c(alpha.p, beta.p, sqrt(var.fm.p), r.square.p)
names(fm.p) = c("intercept", factor.names, "sd", "r-squared")
fm.p

# factor model residuals - need to extract over truncated sample
# will be use later in factor risk budgeting
fm.resid = rep(0, nrow(r.p))
smpl = rownames(r.p)
for (i in manager.names) {
  reg.resid = residuals(reg.list[[i]])
  fm.resid = fm.resid + w.vec[i]*reg.resid[smpl]
}

################################################################################
# Risk Measures
################################################################################

#
# use jarque.bera.test() function from tseries package to test for normality
#
# use jarque.bera.test() function from tseries package to test for normality
jarque.bera.test(managers.df$HAM1)
jarque.bera.test(managers.df$HAM2)
jarque.bera.test(managers.df$HAM3)
jarque.bera.test(managers.df$HAM4)
jarque.bera.test(na.omit(managers.df$HAM5))
jarque.bera.test(na.omit(managers.df$HAM6))

# use apply() to do everything at once
normalTest = function(x) {
 tmp.test = jarque.bera.test(na.omit(x))
 ans = c(tmp.test$statistic, tmp.test$p.value)
 names(ans) = c("statistic", "p-value")
 return(ans)
}
apply(managers.df[, manager.names], 2, normalTest)

#
# Gaussian VaR and ETL for the 6 hedge funds
#

# use VaR function from package PerformanceAnalytics
args(VaR)
VaR(managers.df[, manager.names], p=0.95, method="gaussian")
VaR(managers.df[, manager.names], p=0.99, method="gaussian")

# show histogram with 5% and 1% normal VaR values
 chart.Histogram(managers.df$HAM1, main="Normal VaR for HAM1",
                 methods=c("add.normal"),
                 note.lines=c(-0.0352, -0.0531),
                 note.labels=c("5% Normal VaR", "1% Normal VaR"),
                 note.color=c("blue", "red"),
                 note.cex=1.25)

# use ES function from package PerformanceAnalytics
args(ES)
ES(managers.df[, manager.names], p=0.95, method="gaussian")
ES(managers.df[, manager.names], p=0.99, method="gaussian")
                 
#
# FM Gaussian VaR and ETL for the 6 hedge funds
#

# compute mu.fm and sigma.fm for all assets. Set alpha = 0 for mu.fm
mu.factors = colMeans(managers.df[, factor.names])
mu.fm = Betas%*%mu.factors
sigma.fm = sqrt(diag(cov.fm))
# compute VaR using factorAnalytics function normalVaR
args(normalVaR)
VaR.05.fm = t(normalVaR(mu.fm, sigma.fm, tail.prob=0.05))
VaR.01.fm = t(normalVaR(mu.fm, sigma.fm, tail.prob=0.01))
rbind(VaR.05.fm, VaR.01.fm)

# compute ETL using factorAnalytics function normalES
args(normalES)
ES.05.fm = t(normalES(mu.fm, sigma.fm, tail.prob=0.05))
ES.01.fm = t(normalES(mu.fm, sigma.fm, tail.prob=0.01))
rbind(ES.05.fm, ES.01.fm)

#
# Cornish-Fisher modified VaR and ETL
#
VaR(managers.df[, manager.names], p=0.95, method="modified")
VaR(managers.df[, manager.names], p=0.99, method="modified")

ES(managers.df[, manager.names], p=0.95, method="modified")
ES(managers.df[, manager.names], p=0.99, method="modified")

#
# nonparametric VaR  and ETL
#
# nonparametric VaR is based on empirical quantile
quantile(managers.df$HAM1, probs=c(0.01, 0.05))
# use VaR() function with method = "historical"
VaR(managers.df[, manager.names], p=0.95, method="historical")
VaR(managers.df[, manager.names], p=0.99, method="historical")

# nonparametric ETL is sample mean below nonparametric VaR
q.hat.05 = quantile(managers.df$HAM1, probs=0.05)
smpl = managers.df$HAM1 <= q.hat.05
mean(managers.df$HAM1[smpl])

# use ES() function with method = "historical"
ES(managers.df[, manager.names], p=0.95, method="historical")
ES(managers.df[, manager.names], p=0.99, method="historical")

#
# VaR and ETL for portfolio
#

# note: portfolio returns have to use truncated sample due to missing values
# (unequal histories) in HAM5 and HAM6
# Normal VaR and Normal FM VaR
VaR(r.p, p=0.95, method="gaussian")
VaR(r.p, p=0.99, method="gaussian")
mu.fm.p = crossprod(beta.p, mu.factors)
sd.fm.p = sqrt(var.fm.p)
normalVaR(mu.fm.p, sd.fm.p, tail.prob=0.05)
normalVaR(mu.fm.p, sd.fm.p, tail.prob=0.01)

# Cornish-Fisher (modified VaR)
VaR(r.p, p=0.95, method="modified")
VaR(r.p, p=0.99, method="modified")
# Historical VaR
VaR(r.p, p=0.95, method="historical")
VaR(r.p, p=0.99, method="historical")

# Normal ES and Normal FM ES
ES(r.p, p=0.95, method="gaussian")
ES(r.p, p=0.99, method="gaussian")
normalES(mu.fm.p, sd.fm.p, tail.prob=0.05)
normalES(mu.fm.p, sd.fm.p, tail.prob=0.01)

# Cornish-Fisher (modified VaR)
ES(r.p, p=0.95, method="modified")
ES(r.p, p=0.99, method="modified")
# Historical VaR
ES(r.p, p=0.95, method="historical")
ES(r.p, p=0.99, method="historical")

################################################################################
# Factor Risk Budgeting
################################################################################

#
# risk factor contribution to standard deviation
#

# use factorModelFactorSdDecomposition() function from factorAnalytics package
args(factorModelFactorSdDecomposition)

# Example: factor SD decomposition for HAM1
factor.sd.decomp.HAM1 = factorModelFactorSdDecomposition(Betas["HAM1",],
                                         cov.factors, ResidVars["HAM1"])
names(factor.sd.decomp.HAM1)
factor.sd.decomp.HAM1

# loop over all assets and store results in list
factor.sd.decomp.list = list()
for (i in manager.names) {
 factor.sd.decomp.list[[i]] = factorModelFactorSdDecomposition(Betas[i,],
                                                cov.factors, ResidVars[i])
}
# add portfolio factor SD decomposition to list
factor.sd.decomp.list[["PORT"]] = factorModelFactorSdDecomposition(beta.p,
                                                 cov.factors, var.p.resid)
names(factor.sd.decomp.list)

# stacked bar charts of percent contributions to SD
getCSD = function(x) {
 x$cr.fm
}
cr.sd = sapply(factor.sd.decomp.list, getCSD)
rownames(cr.sd) = c(factor.names, "residual")
barplot(cr.sd, main="Factor Contributions to SD",
        legend.text=T, args.legend=list(x="topleft"),
        col=c("blue","red","green","white"))

#
# risk factor contribution to ETL
#

# combine fund returns, factor returns and residual returns
tmpData = cbind(managers.df[,1], managers.df[,factor.names],
               residuals(reg.list[[1]])/sqrt(ResidVars[1]))
colnames(tmpData)[c(1,5)] = c(manager.names[1], "residual")
factor.es.decomp.HAM1 = factorModelFactorEsDecomposition(tmpData, Betas[1,],
                                                ResidVars[1], tail.prob=0.05)
names(factor.es.decomp.HAM1)
factor.es.decomp.HAM1

# compute decomposition in loop
factor.es.decomp.list = list()
for (i in manager.names) {
# check for missing values in fund data
 idx = which(!is.na(managers.df[,i]))
 tmpData = cbind(managers.df[idx,i], managers.df[idx,factor.names],
               residuals(reg.list[[i]])/sqrt(ResidVars[i]))
 colnames(tmpData)[c(1,5)] = c(manager.names[i], "residual")
 factor.es.decomp.list[[i]] = factorModelFactorEsDecomposition(tmpData, Betas[i,],
                                                     ResidVars[i], tail.prob=0.05)
}
# add portfolo retsults - need factor model residuals
tmpData = cbind(r.p, managers.df[rownames(r.p),factor.names],
               fm.resid/sqrt(as.numeric(var.p.resid)))
colnames(tmpData)[c(1,5)] = c("PORT", "residual")
factor.es.decomp.list[["PORT"]] = factorModelFactorEsDecomposition(tmpData, beta.p,
                                                     var.p.resid, tail.prob=0.05)

# stacked bar charts of percent contributions to SD
getCETL = function(x) {
 x$cES.fm
}
# report as positive number
cr.etl = sapply(factor.es.decomp.list, getCETL)
rownames(cr.etl) = c(factor.names, "residual")
barplot(cr.etl, main="Factor Contributions to ETL",
        legend.text=T, args.legend=list(x="topleft"),
        col=c("blue","red","green","white"))

#
# illustrate with graphic. Plot fund returns, VaR line and factor returns. Identify
# those periods when fund return is less than its VaR, then look at where the
# factor returns are and compute average
#
tmpData = cbind(managers.df[,1], managers.df[,factor.names],
               residuals(reg.list[[1]])/sqrt(ResidVars[1]))
colnames(tmpData)[c(1,5)] = c(manager.names[1], "residual")
# create zoo data for time series plots
tmpData.zoo = zoo(tmpData, as.Date(rownames(tmpData)))

# find those observations less than VaR
VaR.fm = quantile(tmpData[, 1], prob=0.05)
idx = which(tmpData[, 1] <= VaR.fm)
rownames(tmpData)[idx]
# average factor data at obvs idx
mcETL.EDHEC.LS.EQ = mean(tmpData[idx, 2])
mcETL.SP500.TR = mean(tmpData[idx, 3])
mcETL.US.10Y.TR = mean(tmpData[idx, 4])
mcETL.residual = mean(tmpData[idx, 5])

# mcETL plot for EDHEC.LS.EQ
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(tmpData.zoo[,1], type="b", main="HAM1 returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.fm, lwd=2, col="red")
points(tmpData.zoo[idx, 1], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(tmpData.zoo[,2], type="b", main="Mean of EDHEC.LS.EQ when HAM1 <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=mcETL.EDHEC.LS.EQ, lwd=2, col="red")
points(tmpData.zoo[idx, 2], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for SP500.TR
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(tmpData.zoo[,1], type="b", main="HAM1 returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.fm, lwd=2, col="red")
points(tmpData.zoo[idx, 1], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(tmpData.zoo[,3], type="b", main="Mean of SP500.TR when HAM1 <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=mcETL.SP500.TR, lwd=2, col="red")
points(tmpData.zoo[idx, 3], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for US10.YR.TR
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(tmpData.zoo[,1], type="b", main="HAM1 returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.fm, lwd=2, col="red")
points(tmpData.zoo[idx, 1], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(tmpData.zoo[,4], type="b", main="Mean of US.10Y.TR when HAM1 <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=mcETL.US.10Y.TR, lwd=2, col="red")
points(tmpData.zoo[idx, 4], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for residual
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(tmpData.zoo[,1], type="b", main="HAM1 returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.fm, lwd=2, col="red")
points(tmpData.zoo[idx, 1], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(tmpData.zoo[,5], type="b", main="Mean of Standardized Residual when HAM1 <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=mcETL.residual, lwd=2, col="red")
points(tmpData.zoo[idx, 5], type="p", pch=16, col="red")
par(mfrow=c(1,1))

################################################################################
# portfolio risk budgeting
################################################################################

#
# fund contribution to portfolio SD
#

# use portfolioSdDecomposition() function from factorAnalytics package
args(portfolioSdDecomposition)

# compute with sample covariance matrix (pairwise complete obvs)
cov.sample = cov(managers.df[,manager.names],
                 use="pairwise.complete.obs")
port.sd.decomp.sample = portfolioSdDecomposition(w.vec, cov.sample)
names(port.sd.decomp.sample)
port.sd.decomp.sample

# show bar chart
barplot(port.sd.decomp.sample$pcsd.p,
        main="Fund Percent Contributions to Portfolio SD",
        ylab="Percent Contribution", legend.text=F,
        col="blue")

# compute with factor model covariance matrix
port.sd.decomp.fm = portfolioSdDecomposition(w.vec, cov.fm)
port.sd.decomp.fm

#
# fund contribution to portfolio ETL
#

# use ES() function in PerformanceAnalytics package
# note: result only gives percent contribution to ETL, no marginal or component
args(ES)
# need to remove missing values prior to computation
port.ES.decomp = ES(na.omit(managers.df[,manager.names]),
                    p=0.95, method="historical",
                    portfolio_method = "component",
                    weights = w.vec)
port.ES.decomp

# use portfolioEsDecomposition from factorAnalytics package
# results are slightly different due to different implementation of quantile function
args(portfolioEsDecomposition)
port.ES.decomp = portfolioEsDecomposition(na.omit(managers.df[,manager.names]),
                                          w.vec, tail.prob=0.05)
names(port.ES.decomp)
port.ES.decomp

# show bar chart
barplot(port.ES.decomp$PCES,
        main="Fund Percent Contributions to Portfolio ETL",
        ylab="Percent Contribution", legend.text=F,
        col="blue")

## graphical illustration of func marginal contribution to portfolio ETL
# find those observations less than VaR

nrow(r.p)
VaR.p = quantile(r.p, prob=0.05)
idx = which(r.p <= VaR.p)
rownames(r.p)[idx]
mcES = port.ES.decomp$MCES

# mcETL plot for HAM1
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(r.p.zoo, type="b", main="Portfolio Returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.p, lwd=2, col="red")
points(r.p.zoo[idx], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(managers.zoo[, "HAM1"], type="b", main="Mean of HAM1 when PORT <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=-mcES[1,"HAM1"], lwd=2, col="red")
points(managers.zoo[idx, "HAM1"], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for HAM2
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(r.p.zoo, type="b", main="Portfolio Returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.p, lwd=2, col="red")
points(r.p.zoo[idx], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(managers.zoo[, "HAM2"], type="b", main="Mean of HAM2 when PORT <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=-mcES[1,"HAM2"], lwd=2, col="red")
points(managers.zoo[idx, "HAM2"], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for HAM3
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(r.p.zoo, type="b", main="Portfolio Returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.p, lwd=2, col="red")
points(r.p.zoo[idx], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(managers.zoo[, "HAM3"], type="b", main="Mean of HAM3 when PORT <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=-mcES[1,"HAM3"], lwd=2, col="red")
points(managers.zoo[idx, "HAM3"], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for HAM4
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(r.p.zoo, type="b", main="Portfolio Returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.p, lwd=2, col="red")
points(r.p.zoo[idx], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(managers.zoo[, "HAM4"], type="b", main="Mean of HAM4 when PORT <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=-mcES[1,"HAM4"], lwd=2, col="red")
points(managers.zoo[idx, "HAM4"], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for HAM5
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(r.p.zoo, type="b", main="Portfolio Returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.p, lwd=2, col="red")
points(r.p.zoo[idx], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(managers.zoo[, "HAM5"], type="b", main="Mean of HAM5 when PORT <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=-mcES[1,"HAM5"], lwd=2, col="red")
points(managers.zoo[idx, "HAM5"], type="p", pch=16, col="red")
par(mfrow=c(1,1))

# mcETL plot for HAM6
par(mfrow=c(2,1))
# plot fund data with VaR violations
plot.zoo(r.p.zoo, type="b", main="Portfolio Returns and 5% VaR Violations",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=VaR.p, lwd=2, col="red")
points(r.p.zoo[idx], type="p", pch=16, col="red")
# plot factor data and highlight obvs associated with R <= VaR
plot.zoo(managers.zoo[, "HAM6"], type="b", main="Mean of HAM6 when PORT <= 5% VaR",
         col="blue", ylab="Returns")
abline(h=0)
abline(h=-mcES[1,"HAM6"], lwd=2, col="red")
points(managers.zoo[idx, "HAM6"], type="p", pch=16, col="red")
par(mfrow=c(1,1))

################################################################################
# factor model Monte Carlo
################################################################################

# examime distribution of residuals
hist(residuals(reg.list$HAM2))
qqnorm(residuals(reg.list$HAM1))
qqline(residuals(reg.list$HAM1))

#
# FMMC with normal distribution for factor model residuals
#

# resample from historical factors
n.boot = 5000
# set random number sed
set.seed(123)
# reshuffled indices with replacement
bootIdx = sample(nrow(managers.df),  n.boot, replace=TRUE)
# resampled factor data
factorDataBoot.mat = as.matrix(managers.df[bootIdx, factor.names])

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
r.p.boot = returns.boot%*%w.vec
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
tmpData = cbind(r.p.boot, factorDataBoot.mat,
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
rownames(cr.etl) = c(factor.names, "residual")
barplot(cr.etl, main="Factor Contributions to ETL",
        legend.text=T, args.legend=list(x="topleft"),
        col=c("blue","red","green","white"))


#
# compute fund contributions to ETL using FMMC sample
#

port.ES.decomp.fmmc = portfolioEsDecomposition(returns.boot,
                                               w.vec, tail.prob=0.05)
port.ES.decomp.fmmc

# show bar chart
barplot(port.ES.decomp.fmmc$PCES,
        main="Fund Percent Contributions to Portfolio ETL",
        ylab="Percent Contribution", legend.text=F,
        col="blue")


