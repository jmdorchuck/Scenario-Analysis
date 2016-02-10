library(xlsx)
library(PerformanceAnalytics)
library(dygraphs)

############S

setwd("O:/SteveP/Matt/Board Memos/Expected Returns")
wb = loadWorkbook("stresstestmanagerdata.xlsx")
init.facs = read.xlsx("Stresstestmanagerdata.xlsx",sheetName = "FactorReturns",header = TRUE)
init.betas = read.xlsx("Stresstestmanagerdata.xlsx",sheetName = "Betas",header = TRUE)
in.wts = as.data.frame(read.xlsx("Stresstestmanagerdata.xlsx",sheetName = "Weights",header = TRUE))
init.wts = as.data.frame(in.wts$Weight); 
rownames(init.wts) = in.wts[,1]
init.class = read.xlsx("Stresstestmanagerdata.xlsx",sheetName = "AssetClass",header = FALSE,
                       stringsAsFactors = FALSE)
init.acf = read.xlsx("Stresstestmanagerdata.xlsx",sheetName = "Autocorr",header = TRUE)
init.liquidity = read.xlsx("Stresstestmanagerdata.xlsx",sheetName = "Liquidity",header = TRUE)
init.liquidity = as.data.frame(init.liquidity)
rownames(init.liquidity) = init.liquidity[,1]
init.liquidity = 1000*init.liquidity[,-1] #convert to millions
rownames(init.acf) = init.acf[,1]
init.acf = init.acf[,-1]

useACF = TRUE #lag the private manager returns sufficiently
rebalancerisky = TRUE #rebalance from risky assets to bonds 
covwindow = 36
cov.stressed.date = as.Date("2010-12-31")
duration.stress = 24
nsims = 5000
cash.target = .01 #what to rebalance cash to
bonds.target = .08 #what to rebalance bonds to

er.facs = c(.0578, .057, -0.015, .004, .015, .005)
er.facs = ((1 +er.facs) ^(1/12)-1)

#start subsequent runs from here
#set up lag function
lag = 25 #0 as instantaneous + 24 lags


init.commit.out = 330000000
commit.pace = .05 #5% per year
commit.function = function(LTP.NAV, commit.pace) {LTP.NAV * ((1 + commit.pace)^(1/12)-1)}
#linear calls over 3 years
call.function = function(commit.level) {commit.level / 36}

private.wts.init = as.data.frame(init.wts[which(init.class[,2] == "PE" | 
                                                  init.class[,2] == "RA"),])
private.wts.init = private.wts.init / sum(private.wts.init)
rownames(private.wts.init) = rownames(init.wts)[which(init.class[,2] == "PE" | 
                                                        init.class[,2] == "RA")]
colnames(private.wts.init) = "weights"

index.private = which(init.class[,2] == "PE" | init.class[,2] == "RA")
index.pubeq = which(init.class[,2] == "PUBEQ")
index.lseq = which(init.class[,2] == "LSEQ")
index.cmalt = which(init.class[,2] == "CMALT")
index.gbond = which(init.class[,2] == "GBOND")
index.pe = which(init.class[,2] == "PE")
index.ra = which(init.class[,2] == "RA")
#Set up initial constraints
init.NAV = 7500000000
init.NAV.mgrs = t(init.NAV * t(in.wts$Weight))
rownames(init.NAV.mgrs) = rownames(init.wts)

liquidity.LTP = as.data.frame(apply(init.liquidity,MARGIN = 2, function(x) (x/init.NAV.mgrs)))
rownames(liquidity.LTP) = rownames(init.wts)
liquidity.LTP["Cash",] = rep(1, 3)
liquidity.LTP["Government Bonds",] = rep(1, 3)
liquidity.LTP = liquidity.LTP / max(liquidity.LTP[,1]) 



#time of the total simulation, just enter the month ends you want to start with. The +/- 1s are for error handling (leap days)
time = seq(from = as.Date("2007-12-31")+1, to = as.Date("2010-02-28")+1,by = "1 month") -1

#specify functions
payout.function = function(NAV) {(((1+.05)^(1/12)-1) * NAV)}
capitalcall.function = function(CASH) {CASH * 1}
distribution.function = function(Private.NAV) {Private.NAV/(15*12)}



init.facs = xts(init.facs[,2:ncol(init.facs)], order.by = as.Date(init.facs[,1], format = "%m/%d/%Y"))
index(init.facs) = as.Date(as.yearmon(index(init.facs)), frac = 1)
time = as.Date(as.yearmon(time), frac = 1)

lagtime.start = which(index(init.facs)==time[1])-24
lagtime.end = which(index(init.facs) == time[length(time)])
lagtime.index = index(init.facs)[lagtime.start:lagtime.end]
facs.time = init.facs[lagtime.index, -ncol(init.facs)]
cash.time = init.facs[lagtime.index, "Cash"]

#i forgot to take cash out of the SD treasuries in excel
facs.time$SDTreas = facs.time$SDTreas - as.matrix(coredata(cash.time))

betas.mgr = as.matrix(init.betas[,3:7])
rownames(betas.mgr) =init.betas[,1]

rets.mgrs = array(dim = c(length(time),nrow(init.betas)))

if (useACF == TRUE){
  for (i in 1:length(time))
    for (j in 1:nrow(init.betas)){
      temp.lag = t(init.acf[j,]) %*% betas.mgr[j,]
      temp.lag <- apply(temp.lag, 2, rev)
      temp.start <- lagtime.index[i]
      temp.end = lagtime.index[i+24]
      temp.return = facs.time[paste(temp.start,temp.end,sep = "::")]
      temp.out <- sum(temp.lag * temp.return)
      rets.mgrs[i,j] = (temp.out +cash.time[i,])
    }
} else{
  for (i in 1:length(time))
    for (j in 1:nrow(init.betas)){
      rets.mgrs[i,j] = (betas.mgr[j,] %*% t(facs.time[i,]) +cash.time[i,])
    }
}


rets.mgrs = as.matrix(rets.mgrs)
colnames(rets.mgrs) = rownames(betas.mgr)

rets.mgrs.xts = xts(rets.mgrs, order.by = time)

time2 = which(index(init.facs) == time[1])
time2 = as.Date(append(index(init.facs)[time2-1], time))


index.cash.acct = which(rownames(betas.mgr) == "Cash")
index.gbond.acct = which(rownames(betas.mgr) == "Government Bonds")

NAV.mgrs = array(dim = c(length(time)+1,nrow(betas.mgr)))
NAV.LTP = array(dim = c(length(time)+1,1))
commitments = array(dim = c(length(time)+1,1))
calls = array(dim = c(length(time)+1,1))
distributions = array(dim = c(length(time)+1,1))
payout = array(dim = c(length(time)+1,1))

colnames(NAV.mgrs) = colnames(rets.mgrs)

NAV.mgrs[1,] =  init.NAV * t(in.wts$Weight)
NAV.LTP[1] = init.NAV
commitments[1,] = init.commit.out

maxt = length(NAV.LTP)
for(i in 1:maxt){
  
  if (i != 1){
    NAV.mgrs[i,] = NAV.mgrs[i-1,] * (1+rets.mgrs[i-1,])
  }
  
  temp = NAV.mgrs[i,]
  if(i != 1){
    commitments[i] = commit.function(LTP.NAV = NAV.LTP[i-1], commit.pace = commit.pace) + 
      commitments[i-1]
  }
  
  calls[i] = call.function(commit.level = commitments[i])
  commitments[i] = commitments[i] - calls[i]
  distributions[i] = distribution.function(Private.NAV = sum(NAV.mgrs[i, index.private]))
  if (i != 1){
    NAV.mgrs[i, index.private] = NAV.mgrs[i, index.private] + 
      calls[i] * t(private.wts.init) - distributions[i] * t(private.wts.init)
  }
  
  NAV.LTP[i] = sum(NAV.mgrs[i,])
  payout[i] = payout.function(NAV = NAV.LTP[i,])
  
  NAV.mgrs[i, "Cash"] = NAV.mgrs[i, "Cash"] - (calls[i] - distributions[i] + payout[i])
  cash.prc.temp = NAV.mgrs[i, index.cash.acct] / NAV.LTP[i]
  NAV.mgrs[i, index.gbond.acct] = NAV.mgrs[i, index.gbond.acct] - max(0, cash.target - cash.prc.temp) *  NAV.LTP[i]
  NAV.mgrs[i, index.cash.acct] = NAV.mgrs[i, index.cash.acct] + max(0, cash.target - cash.prc.temp) *  NAV.LTP[i]
  
  if(rebalancerisky == TRUE){
    bond.prc.temp = NAV.mgrs[i, index.gbond.acct] / NAV.LTP[i]
    NAV.mgrs[i, index.gbond.acct] = NAV.mgrs[i, index.gbond.acct] + max(0, bonds.target - bond.prc.temp) *  NAV.LTP[i]
    weighted.liquidity.3 = ((NAV.mgrs[i,] / NAV.LTP[i]) * liquidity.LTP[,1]) / (sum(NAV.mgrs[i,] / NAV.LTP[i] * liquidity.LTP[,1]))
    weighted.liquidity.3[is.nan(weighted.liquidity.3)] <- 0
    NAV.mgrs[i,] = NAV.mgrs[i,] - coredata(max(0, bonds.target - bond.prc.temp) * (NAV.LTP[i] * coredata(weighted.liquidity.3)))
  }
  
  temp2 = NAV.mgrs[i,]
  NAV.LTP[i] = sum(NAV.mgrs[i,])
}

NAV.mgrs = as.matrix(NAV.mgrs)

private.total = rowSums(NAV.mgrs[,index.private]) + commitments
private.prc.LTP = xts(private.total/ NAV.LTP, order.by = time2)
commit.prc.LTP = xts(commitments/ NAV.LTP, order.by = time2)
cash.amount = xts(NAV.mgrs[,"Cash"], order.by = time2)
cash.prc.LTP = xts(NAV.mgrs[,"Cash"] / NAV.LTP, order.by = time2)

pubeq.total = xts(rowSums(NAV.mgrs[,index.pubeq]), order.by =time2)
lseq.total = xts(rowSums(NAV.mgrs[,index.lseq]), order.by = time2)
cmalt.total = xts(rowSums(NAV.mgrs[,index.cmalt]), order.by = time2)
gbond.total = xts(rowSums(NAV.mgrs[,index.gbond]), order.by = time2)
pe.total = xts(rowSums(NAV.mgrs[,index.pe]), order.by = time2)
ra.total = xts(rowSums(NAV.mgrs[,index.ra]), order.by = time2)

all.classes.total = do.call(merge.xts, list(pubeq.total, lseq.total, cmalt.total, gbond.total, pe.total, ra.total))
colnames(all.classes.total) = c("Pub Equity", "LS Equity", "CMALT", "Cash and Bonds", "PE", "Real Assets")


liquidity.3 = t(apply(NAV.mgrs, MARGIN = 1, FUN = function(x) x * liquidity.LTP[,1]))
liquidity.6 = t(apply(NAV.mgrs, MARGIN = 1, FUN = function(x) x * liquidity.LTP[,2]))
liquidity.12 = t(apply(NAV.mgrs, MARGIN = 1, FUN = function(x) x * liquidity.LTP[,3]))
total.liquidity.3 = as.xts(rowSums(liquidity.3), order.by = time2)
total.liquidity.6 = as.xts(rowSums(liquidity.6), order.by = time2)
total.liquidity.12 = as.xts(rowSums(liquidity.12), order.by = time2)
total.liquidity.historical.graph = merge.xts(merge.xts(total.liquidity.3 / NAV.LTP, total.liquidity.6 / NAV.LTP), total.liquidity.12 / NAV.LTP)


#######################################


NAV.LTP = as.xts(NAV.LTP, colnames = "LTP NAV", order.by = time2)

dygraph(NAV.LTP, main = "LTP NAV")
dygraph(private.prc.LTP, "Private Aggregate as % of LTP")
dygraph(commit.prc.LTP, main = "Commitments as % of LTP")
dygraph(cash.prc.LTP, "Cash as % of LTP")
dygraph(cash.amount, "Cash Amount in USD")
dygraph(total.liquidity.historical.graph, "Total Liquidity as % of LTP by Timing of Availability")
dygraph(pubeq.total, "Total Public Equity")
dygraph(lseq.total, "Total L/S Equity")
dygraph(cmalt.total,"Total CMALT")
dygraph(gbond.total, "Total Cash and Bonds")
dygraph(pe.total, "Total Private Equity")
dygraph(ra.total, "Total Real Assets")
dygraph(all.classes.total)

dygraph(xts(apply(all.classes.total* 100, MARGIN = 2,FUN =  function(x)x / NAV.LTP), order.by = time2), "Asset Classes as % of LTP") %>%
  dyOptions(stackedGraph = TRUE, fillGraph = TRUE)



######################################################
#Begin MC sim
#

cov.facs = array(data = NA, dim = c(nrow(init.facs), ncol(init.facs), ncol(init.facs)))

for(i in 1:nrow(init.facs)){
  if ((i - covwindow) < 1){
    cov.facs[i,,] = NA }
  else{
    cov.facs[i,,] = cov(init.facs[(i-(covwindow-1)):i,])
  }
}

cov.facs.avg = cov(init.facs)
cov.facs.stressed = cov.facs[which(index(init.facs) == cov.stressed.date),,]

cor.facs.avg = cov2cor(cov.facs.avg)
cor.facs.stressed = cov2cor(cov.facs.stressed)
sig.facs.avg = diag(cov.facs.avg)^.5
sig.facs.stressed = diag(cov.facs.stressed)^.5

#chart rolling vols for reference
diag <- array(dim = c(nrow(cov.facs[,1,]),1))
for (i in 1:nrow(cov.facs[,1,])){
  diag[i] = diag(cov.facs[i,,])[1]
}

diag = xts(diag^.5, order.by = index(init.facs))
dygraph(diag * sqrt(12))


########### start the MC
str(init.facs)

mc.n = duration.stress + lag

stress.facs = array(NA, dim = c(nsims, ncol(init.facs), mc.n))

for (i in 1:nsims){
  stress.facs[i,,] = rep(t(as.matrix(er.facs)), mc.n) + 
    matrix(rnorm(mc.n *  length(er.facs)),nrow = mc.n) %*% chol(cov.facs.stressed)
}

betas.mgr.mc = cbind(as.matrix(init.betas[,3:7]), rep(1, length(init.betas[,1])))
rownames(betas.mgr.mc) = init.betas[,1]

rets.mgrs.mc = array(dim = c(nsims, duration.stress,nrow(init.betas)))

if (useACF == TRUE){
  for (i in 1:duration.stress){
    for (j in 1:nrow(init.betas)){
      for(k in 1:nsims){
        temp.lag = t(init.acf[j,]) %*% betas.mgr.mc[j,]
        temp.lag <- apply(temp.lag, 2, rev)
        temp.return = stress.facs[k,,i:(i+lag-1)]
        temp.out <- sum(temp.lag * t(temp.return))
        rets.mgrs.mc[k,i,j] = temp.out
      }
    }
  }
} 

#####################

liquidity.table = liquidity.LTP[,1] * NAV.mgrs[1,] / init.NAV
names(liquidity.table) = rownames(init.NAV.mgrs)


index.cash.acct = which(rownames(betas.mgr) == "Cash")
index.gbond.acct = which(rownames(betas.mgr) == "Government Bonds")
NAV.mgrs.mc = array(dim = c(nsims,duration.stress +1, nrow(init.betas)))
NAV.LTP.mc = array(dim = c(nsims,duration.stress + 1))
commitments.mc = array(dim = c(nsims,duration.stress + 1))
calls.mc = array(dim = c(nsims,duration.stress + 1))
distributions.mc = array(dim = c(nsims,duration.stress+1))
payout.mc = array(dim = c(nsims,duration.stress+1))
private.total.mc = array(data = NA, dim = c(nsims, duration.stress + 1))
pubeq.total.mc = array(data = NA, dim = c(nsims, duration.stress + 1))
lseq.total.mc = array(data = NA, dim = c(nsims, duration.stress + 1))
cmalt.total.mc = array(data = NA, dim = c(nsims, duration.stress + 1))
gbond.total.mc = array(data = NA, dim = c(nsims, duration.stress + 1))
pe.total.mc = array(data = NA, dim = c(nsims, duration.stress + 1))
ra.total.mc = array(data = NA, dim = c(nsims, duration.stress + 1))
liquidity.mc.3 = array(data = NA, dim = c(nsims, duration.stress+1, nrow(init.betas)))
liquidity.mc.6 = array(data = NA, dim = c(nsims, duration.stress+1, nrow(init.betas)))
liquidity.mc.12 = array(data = NA, dim = c(nsims, duration.stress+1, nrow(init.betas)))
total.liquidity.mc.3 = array(data = NA, dim = c(nsims, duration.stress + 1))
total.liquidity.mc.6 = array(data = NA, dim = c(nsims, duration.stress + 1))
total.liquidity.mc.12 = array(data = NA, dim = c(nsims, duration.stress + 1))

# This isn't working for some reason: rownames(NAV.mgrs) = as.Date(time2)
NAV.mgrs.mc[,1,] =  matrix(rep(as.matrix(init.NAV * t(in.wts$Weight)), nrow(NAV.mgrs.mc[,1,])), byrow = TRUE, nrow = nsims)
NAV.LTP.mc[,1] = init.NAV
commitments.mc[,1] = init.commit.out

maxt = duration.stress +1
for (j in 1:nsims){
  colnames(NAV.mgrs.mc[j,,]) = colnames(rets.mgrs)
  for(i in 1:maxt){
    
    if (i != 1){
      NAV.mgrs.mc[j,i,] = NAV.mgrs.mc[j,i-1,] * (1+rets.mgrs.mc[j,i-1,])
    }
    
    temp = NAV.mgrs.mc[j,i,]
    if(i != 1){
      commitments.mc[j,i] = commit.function(LTP.NAV = NAV.LTP.mc[j,i-1], commit.pace = commit.pace) + 
        commitments.mc[j,i-1]
    }
    
    calls.mc[j,i] = call.function(commit.level = commitments.mc[j,i])
    commitments.mc[j,i] = commitments.mc[j,i] - calls.mc[j,i]
    distributions.mc[j,i] = distribution.function(Private.NAV = sum(NAV.mgrs.mc[j,i, index.private]))
    if (i != 1){
      NAV.mgrs.mc[j,i, index.private] = NAV.mgrs.mc[j,i, index.private] + 
        calls.mc[j,i] * t(private.wts.init) - distributions.mc[j,i] * t(private.wts.init)
    }
    
    NAV.LTP.mc[j,i] = sum(NAV.mgrs.mc[j,i,])
    payout.mc[j,i] = payout.function(NAV = NAV.LTP.mc[j,i])
    
    NAV.mgrs.mc[j,i, index.cash.acct] = NAV.mgrs.mc[j,i, index.cash.acct] - (calls.mc[j,i] - distributions.mc[j,i] + payout.mc[j,i])
    cash.prc.temp = NAV.mgrs.mc[j,i, index.cash.acct] / NAV.LTP.mc[j,i]
    NAV.mgrs.mc[j,i, index.gbond.acct] = NAV.mgrs.mc[j,i, index.gbond.acct] - max(0, cash.target - cash.prc.temp) *  NAV.LTP.mc[j,i]
    NAV.mgrs.mc[j,i, index.cash.acct] = NAV.mgrs.mc[j,i, index.cash.acct] + max(0, cash.target - cash.prc.temp) *  NAV.LTP.mc[j,i]
    
    
    temp2 = NAV.mgrs.mc[j,i,]
    NAV.LTP.mc[j,i] = sum(NAV.mgrs.mc[j,i,])
    liquidity.mc.3[j,i,] = as.matrix(t(liquidity.LTP[,1] * NAV.mgrs.mc[j,i,]))
    liquidity.mc.6[j,i,] = as.matrix(t(liquidity.LTP[,2] * NAV.mgrs.mc[j,i,]))
    liquidity.mc.12[j,i,] = as.matrix(t(liquidity.LTP[,3] * NAV.mgrs.mc[j,i,]))
    
    
  }
  private.total.mc[j,] = rowSums(NAV.mgrs.mc[j,,index.private]) + commitments.mc[j,]
  pubeq.total.mc[j,] = rowSums(NAV.mgrs.mc[j,,index.pubeq])
  lseq.total.mc[j,] = rowSums(NAV.mgrs.mc[j,,index.lseq])
  cmalt.total.mc[j,] = rowSums(NAV.mgrs.mc[j,,index.cmalt])
  gbond.total.mc[j,] = rowSums(NAV.mgrs.mc[j,,index.gbond])
  pe.total.mc[j,] = rowSums(NAV.mgrs.mc[j,,index.pe])
  ra.total.mc[j,] = rowSums(NAV.mgrs.mc[j,,index.ra])
  total.liquidity.mc.3[j,] = rowSums(liquidity.mc.3[j,,])
  total.liquidity.mc.6[j,] = rowSums(liquidity.mc.6[j,,])
  total.liquidity.mc.12[j,] = rowSums(liquidity.mc.12[j,,])
  
  
}



private.prc.LTP.mc = xts(t(private.total.mc/ NAV.LTP.mc), order.by = index(init.facs)[1:(duration.stress+1)])
cash.amount.mc = t(NAV.mgrs.mc[,,index.cash.acct])
commit.prc.LTP.mc = xts(t(commitments.mc/ NAV.LTP.mc), order.by = index(init.facs)[1:(duration.stress+1)])
cash.amount.mc = xts(t(NAV.mgrs.mc[,,index.cash.acct]), order.by = index(init.facs)[1:(duration.stress+1)])
cash.prc.LTP.mc =  xts(t(NAV.mgrs.mc[,,index.cash.acct]/ NAV.LTP.mc), order.by = index(init.facs)[1:(duration.stress+1)])



quantiles.set = c(.25, .5, .75)
index.mc = array(NA, dim = c(length(quantiles.set),1))
quantiles.max = quantiles.set + .025
quantiles.min = quantiles.set - .025
quantiles.sort.num = length((round(quantiles.min * nsims)[1] : round(quantiles.max * nsims)[1]))

for(i in 1: length(quantiles.set)){
  temp = (round(quantiles.min * nsims)[i] : round(quantiles.max * nsims)[i])
  names(temp) = rep(quantiles.set[i], length(temp))
  if (i == 1){ quantiles.lookup = temp} 
  else{
    quantiles.lookup = append(quantiles.lookup, temp)
  }
}

quantiles = sort(NAV.LTP.mc[,(duration.stress+1)], decreasing = FALSE)[quantiles.lookup]
index.mc = which((NAV.LTP.mc[,(duration.stress+1)]) %in% (quantiles))
quantile.order = rank(t(NAV.LTP.mc[index.mc,(duration.stress+1)]))

quantiles.names = names(quantiles.lookup)
for(i in (1:length(quantiles.names))){
  inx.temp = which(rank(t(NAV.LTP.mc[index.mc,(duration.stress+1)])) == i)
  quantiles.names[inx.temp] = names(quantiles.lookup)[i]
}

private.total.mc.quant = t(private.total.mc[index.mc,])
colnames(private.total.mc.quant) = quantiles.names
NAV.LTP.mc.quant = t(NAV.LTP.mc[index.mc,])

quantile.out <- function(mcqdata){
  for(i in 1:length(quantiles.set)){
    temp.inx = which(quantiles.names %in% quantiles.set[i])
    temp = apply(mcqdata[,temp.inx], MARGIN = 1, mean)
    if (i == 1){
      out = temp
    } else{
      out = cbind(out, temp)
    }
  }
  out = xts(out, order.by = index(init.facs)[1:(duration.stress+1)])
  colnames(out) = quantiles.set
  return(out)
}

private.total.mc.quant.out = quantile.out(private.total.mc.quant)
NAV.LTP.mc.quant.out = quantile.out(NAV.LTP.mc.quant)
private.prc.LTP.mc.quant.out = quantile.out(private.prc.LTP.mc[,index.mc])
cash.prc.LTP.mc.quant.out = quantile.out(cash.prc.LTP.mc[,index.mc])
commitments.mc.quant.out = quantile.out(t(commitments.mc[index.mc,]))
commitments.prc.LTP.mc.quant.out = quantile.out(t(commitments.mc[index.mc,])) / NAV.LTP.mc.quant.out
gbond.mc.quant.out = quantile.out(t(gbond.total.mc[index.mc,]))
pubeq.mc.quant.out = quantile.out(t(pubeq.total.mc[index.mc,]))
lseq.mc.quant.out = quantile.out(t(lseq.total.mc[index.mc,]))
cmalt.mc.quant.out = quantile.out(t(cmalt.total.mc[index.mc,]))
pe.mc.quant.out = quantile.out(t(pe.total.mc[index.mc,]))
ra.mc.quant.out = quantile.out(t(ra.total.mc[index.mc,]))
gbond.prc.LTP.mc.quant.out = gbond.mc.quant.out / NAV.LTP.mc.quant.out
pubeq.prc.LTP.mc.quant.out = pubeq.mc.quant.out / NAV.LTP.mc.quant.out
lseq.prc.LTP.mc.quant.out = lseq.mc.quant.out / NAV.LTP.mc.quant.out
cmalt.prc.LTP.mc.quant.out = cmalt.mc.quant.out / NAV.LTP.mc.quant.out
pe.prc.LTP.mc.quant.out = pe.mc.quant.out / NAV.LTP.mc.quant.out
ra.prc.LTP.mc.quant.out = ra.mc.quant.out / NAV.LTP.mc.quant.out

liquidity.mc.3.out = quantile.out(t(total.liquidity.mc.3))
liquidity.mc.6.out = quantile.out(t(total.liquidity.mc.6))
liquidity.mc.12.out = quantile.out(t(total.liquidity.mc.12))




dygraph(private.total.mc.quant.out, "NAV of Private Aggregate")
dygraph(NAV.LTP.mc.quant.out, main = "NAV of LTP")
dygraph(private.prc.LTP.mc.quant.out, main = "Private Aggregate as % of LTP")
dygraph(cash.prc.LTP.mc.quant.out*100, "Cash as % of LTP")
dygraph(commitments.prc.LTP.mc.quant.out, "Commitments as % of LTP")
dygraph(cash.prc.LTP.mc.quant.out / commitments.prc.LTP.mc.quant.out, "Cash as % of Commitments")
dygraph(gbond.mc.quant.out, "Total Cash and Govt Bonds")
dygraph(gbond.prc.LTP.mc.quant.out * 100, "Total Cash and Govt Bonds as % of LTP")
dygraph(pubeq.prc.LTP.mc.quant.out * 100, "Total Public Equity as % of LTP")
dygraph(lseq.prc.LTP.mc.quant.out * 100, "Total Long/Short Equity as % of LTP")
dygraph(cmalt.prc.LTP.mc.quant.out * 100, "Total Credit and Marketable Alternatives as % of LTP")
dygraph(pe.prc.LTP.mc.quant.out * 100, "Total Private Equity as % of LTP")
dygraph(ra.prc.LTP.mc.quant.out * 100, "Total Real Assets as % of LTP")
dygraph(liquidity.mc.3.out / NAV.LTP.mc.quant.out * 100, "3-Mo Liquidity, % of LTP")
dygraph(liquidity.mc.6.out / NAV.LTP.mc.quant.out * 100, "6-Mo Liquidity, % of LTP")
dygraph(liquidity.mc.12.out / NAV.LTP.mc.quant.out * 100, "12-Mo Liquidity, % of LTP")

# private.total = rowSums(NAV.mgrs[,index.private]) + commitments.mc[j,]
# private.prc.LTP = xts(private.total/ NAV.LTP, order.by = time2)
# commit.prc.LTP = xts(commitments/ NAV.LTP, order.by = time2)
# cash.amount = xts(NAV.mgrs[,"Cash"], order.by = time2)
# cash.prc.LTP = xts(NAV.mgrs[,"Cash"] / NAV.LTP, order.by = time2)