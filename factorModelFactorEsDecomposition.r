## factorModelFactorEsDecomposition.r
##
## author: Eric Zivot
## created: January 1, 2009
## revised: March 11, 2011
factorModelFactorEsDecomposition <- function(bootData, beta.vec, sig2.e, tail.prob = 0.01,
                                            method=c("average"),
                                            VaR.method=c("HS", "CornishFisher")) {
## Compute factor model factor ES decomposition based on Euler's theorem given historic 
## or simulated data and factor model parameters.
## The partial derivative of ES wrt factor beta is computed
## as the expected factor return given fund return is less than or equal to its VaR
## VaR is compute either as the sample quantile or as an estimated quantile
## using the Cornish-Fisher expansion
## inputs:
## bootData   B x (k+2) matrix of bootstrap data. First column contains the fund returns,
##            second through k+1 columns contain factor returns, (k+2)nd column contain residuals
##            scaled to have variance 1.
## beta.vec   k x 1 vector of factor betas
## sig2.e			scalar, residual variance from factor model
## tail.prob  scalar tail probability
## method     character, method for computing marginal ES. Valid choices are
##            "average" for approximating E[Fj | R<=VaR]
## VaR.method character, method for computing VaR. Valid choices are "HS" for
##            historical simulation (empirical quantile); "CornishFisher" for
##            modified VaR based on Cornish-Fisher quantile estimate. Cornish-Fisher
##            computation is done with the VaR.CornishFisher in the PerformanceAnalytics
##            package
## output:
## A list with the following components:
## VaR.fm              scalar, bootstrap VaR value for fund reported as a positive number
## n.exceed            scalar, number of observations beyond VaR
## idx.exceed          n.exceed x 1 vector giving index values of exceedences
## ES.fm               scalar, bootstrap ES value for fund reported as a positive number
## mcES.fm             k+1 x 1 vector of factor marginal contributions to ES
## cES.fm              k+1 x 1 vector of factor component contributions to ES
## pcES.fm             k+1 x 1 vector of factor percent contributions to ES
## Remarks:
## The factor model has the form
## R(t) = beta'F(t) + e(t) = beta.star'F.star(t)
## where beta.star = (beta, sig.e)' and F.star(t) = (F(t)', z(t))'
## By Euler's theorem
## ES.fm = sum(cES.fm) = sum(beta.star*mcES.fm)
## References:
## 1. Hallerback (2003), "Decomposing Portfolio Value-at-Risk: A General Analysis",
##    The Journal of Risk 5/2.
## 2. Yamai and Yoshiba (2002). "Comparative Analyses of Expected Shortfall and
##    Value-at-Risk: Their Estimation Error, Decomposition, and Optimization
##    Bank of Japan.
## 3. Meucci (2007). "Risk Contributions from Generic User-Defined Factors," Risk.
  require(PerformanceAnalytics)
  VaR.method = VaR.method[1]
  bootData = as.matrix(bootData)
  ncol.bootData = ncol(bootData)
  if(is.matrix(beta.vec)) {
    beta.names = c(rownames(beta.vec), "residual")
  } else if(is.vector(beta.vec)) {
    beta.names = c(names(beta.vec), "residual")
  } else {
   stop("beta.vec is not an n x 1 matrix or a vector")
  }  
#  beta.names = c(names(beta.vec), "residual")
	beta.star.vec = c(beta.vec, sqrt(sig2.e))
	names(beta.star.vec) = beta.names

  if (VaR.method == "HS") {
    VaR.fm = quantile(bootData[, 1], prob=tail.prob)
    idx = which(bootData[, 1] <= VaR.fm)
    ES.fm = -mean(bootData[idx, 1])
  } else {
    VaR.fm = -VaR.CornishFisher(bootData[, 1], p=(1-tail.prob))
    idx = which(bootData[, 1] <= pVaR)
    ES.fm = -mean(bootData[idx, 1])
  }
  ##
  ## compute marginal contribution to ES
  ##
  if (method == "average") {
  ## compute marginal ES as expected value of factor return given fund
  ## return is less than or equal to VaR
    mcES.fm = -as.matrix(colMeans(bootData[idx, -1]))
  } else {
    stop("invalid method")
  }
  
## compute correction factor so that sum of weighted marginal ES adds to portfolio ES
#cf = as.numeric( ES.fm / sum(mcES.fm*beta.star.vec) )
#mcES.fm = cf*mcES.fm
cES.fm = mcES.fm*beta.star.vec
pcES.fm = cES.fm/ES.fm
colnames(mcES.fm) = "MCES"
colnames(cES.fm) = "CES"
colnames(pcES.fm) = "PCES"
ans = list(VaR.fm = -VaR.fm,
           n.exceed = length(idx),
           idx.exceed = idx,
           ES.fm = ES.fm, 
           mcES.fm = t(mcES.fm), 
           cES.fm = t(cES.fm),
           pcES.fm = t(pcES.fm))
return(ans)
}
