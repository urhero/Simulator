###########################     PRESS "ALT + O" (in RStudio to fold all sections)
###########################         * N.B. As aiming SIMPLE one easily managable universal code,
###########################           this source is on the premise of 
###########################           1. 'SHORT SHORT SHORT CODING!' w/ self explanatory variables and proper annotations
###########################           2. No seperate one-time function or function of function
###########################           3. Taking full advantage of vector operation (loops when necessary)


setwd("M:/GlobalInvestment/"); 
source("aa/QA/Rcommon/DataAcessFunctions.r"); rm(list = ls()[!(ls() %in% c('fetch_AA', 'fetch_sqlStr', 'fetch_FF', 'pal', 'rlist'))])
source("aa/QA/Rcommon/AA_Functions.r")

for (package in c("plotly", "forecast", "PerformanceAnalytics", "rlist"))
  if (!require(package, character.only=T, quietly=T)) { install.packages(package); library(package, character.only=T) }; rm(package)

# inputs = list(ASTRATEGY = "VALUE")
# inputs = list(ASTRATEGY = "TONG")
# inputs = list(ASTRATEGY = "FOF")
inputs = list(ASTRATEGY = "KIC")
# inputs = list(ASTRATEGY = "BOK")

# Get fetch related info
source("aa/QA/Rcommon/AA_Inputs_Fetch.r");    inputs = inputDefaultValues(inputs)
# inputs$E = "2016-12-31"

f = fetch.all(inputs)
# load("C:/Users/IKM/Desktop/Rdata_temp/BOK_20170510_synamic.RData")
# Get strategy related info
source("aa/QA/Rcommon/AA_Inputs_Strategy.r"); inputs = inputDefaultStrategy(inputs)

# unique(fetch_AA("AA_dscrpt", inputs$S, inputs$E, "*", "acct", distinct=T)$acct, fetch_AA("AA_market", inputs$S, inputs$E, "*", "acct", distinct=T)$acct)

#  DATA(d) PREPERATION (Suspention treat, Normalize factor terms, etc)-----------------------------------------------------------------
source("aa/QA/Rcommon/AA_Functions.r"); d = dPrep(f, inputs)
# source("aa/QA/Rcommon/AA_Recent_Perfoming_Factors.r");
d$ftrB4Fltr = Reduce('+', lapply(1:length(inputs$f.f$acct), function(x) eval(parse(text=paste0("d$f", x))) * inputs$f.f$wgt[x]))
# Filter out
d$ftr = d$ftrB4Fltr + getFltr(f, d, inputs)

# TEST TEMP
if(inputs$ASTRATEGY == "FOF"){
  d$ftr = t( apply(f$MDD1y, 1, rank, na.last = "keep") + apply(f$OCR3y,    1, rank, na.last = "keep") )
  d$ftr = t( apply(d$ert24m_6m, 1, rank, na.last = "keep") + apply(-d$art6m,   1, rank, na.last = "keep") )
  dimnames(d$ftr) = dimnames(f$art)
  d$ftr[ , d$uni == F] = NA }


#  PORTFOLIO SIMULATION --------------------------------------------------------------------------------------------------------
d = portConstruction(d, f, inputs)

dates    = dimnames(f$art)[[1]]
inputs$ASTRATEGY
inputs$maxTilt
source("aa/QA/Rcommon/JSH_Functions.r")
fromD <- dates[d$i.s.t]
# fromD <- "2010-10-31"

end <- "2099-12-31"
f_plotly_ER_Cuml(as.Date(dates),d$i.bVal,d$i.sVal, fromD,end, "BM","Portfolio","")
inputs$f.f
f_performance_total(d$i.bVal,d$i.sVal,as.Date(dates),fromD,end,d$i.tovr)
f_annual_alpha3(as.Date(dates),d$i.sVal,d$i.bVal,fromD,end)

f_plotly_ER_fwdNmonth(as.Date(dates),d$i.bVal,d$i.sVal,1, fromD,end,"BM","Portfolio","")
f_plotly_ER_fwdNmonth(as.Date(dates),d$i.bVal,d$i.sVal,3, fromD,end,"BM","Portfolio","")

# #  PLOTTING --------------------------------------------------------------------------------------------------------------------
# source("aa/QA/Rcommon/AA_Plot.r")
# fromDate <- "2012-02-01"
# plotGraph2(d, fromDate = NA, PerfSumm = T)
# 
# a = with(d, cbind(i.sVal, i.bVal, exVal=i.sVal-i.bVal, tover=i.tovr*100, i.newN, i.pN, i.hN, i.nSusp,
#         i.delistN, lackHN=i.lackHShrsN, i.pSum, i.pR*100, i.bR*100, exR=d$i.exR*100, cuExR=d$i.cuExR*100,
#         noFillN=i.notFilledN, cppiR=i.cppiR*100, i.bigN, deparse.level = 2))
# colnames(a) = gsub("i[.]| [*] 100", "", colnames(a))
# cat("Removed column: ", colnames(a)[apply(a, 2, function(x){ Reduce('&', is.na(x)) })], "\n")
# b = a[,apply(a, 2, function(x){ !Reduce('&', is.na(x)) })]
# print(round(b, 2), print.gap=1, max = 200 * (ncol(b)))


f_d <- f_decile(d$ftr,1)
all_decile_idx <- f_all_decile(mret_fwd_r,f_d)# (fwd return, factor_decile)
f_plot_all_deciles(plotdates,all_decile_idx,fromD,end,"")


#csv for BOK & KIC
last.mul <- d$sHldW[nrow(f$bWgt),which(f$bWgt[nrow(f$bWgt),]>0)]/f$bWgt[nrow(f$bWgt),which(f$bWgt[nrow(f$bWgt),]>0)]
names(last.mul) <- gsub(" CH", "-CN", names(last.mul))
write.table(last.mul,paste0("China-A/dump/", inputs$ASTRATEGY, "_stock_multiplier_", gsub("-", "", Sys.Date()),".csv"), row.names = T, col.names = F, sep = ",")

#csv for Axioma (only KIC)
# if (inputs$ASTRATEGY == "KIC"){
  last.sHldW <- d$sHldW[nrow(f$bWgt),which(f$bWgt[nrow(f$bWgt),]>0)]
  names(last.sHldW) <- gsub(" CH", "-CN", names(last.sHldW))
  write.table(last.sHldW, paste0("China-A/dump/", inputs$ASTRATEGY, "_port_holdings_", gsub("-", "", Sys.Date()),".csv"), row.names = T, col.names = F, sep = ",")
  last.sHldW <- last.sHldW * 100
  write.table(last.sHldW, paste0("China-A/dump/", inputs$ASTRATEGY, "_port_holdings_99991231.csv"), row.names = T, col.names = F, sep = ",")
# }






# Constant proportion portfolio insurance (CPPI)--------------------------------------------------------------------------------
#   cppi_mom(d, f, maxLoss, minVal(floor), nMon, factor, fromDate)
d = cppi_mom(d, f,      .5,            .8,   11,    1.5, fromDate)
plotGraph2(d, fromDate, cppi = T)


#  BBU FILE GENERATION ---------------------------------------------------------------------------------------------------------
bbu <- bbu.csv.data(d$sHldW, inputs)

name_map = list(VALUE = "ChinaValue", TONG = "TongChina")  ##HARD##
write.table(bbu, file = paste0("M:/MOON_Inkyu/TEMP/", name_map[inputs$ASTRATEGY], "_BBU.csv"), sep = ",", row.names = F, col.names = F)  ##HARD##


dates    = dimnames(f$art)[[1]]
universe = dimnames(f$art)[[2]]
a = c("700 HK", "BABA US")
for (i in 1:length(dates))
  a = c(a, unique(universe[which(d$sHldW[i,]>0.04)]))



# Fetch MSCI Indicies

# mxus000g	MSCI U.S GROWTH INDEX	USD	
# mxus000v	MSCI U.S VALUE INDEX	USD	
# MXUS	    MSCI USA	USD	;
# mxuslc	  MSCI USA LARGE CAP	USD	
# mxussc	  MSCI USA SMALL CAP	USD	
dates    = dimnames(f$art)[[1]]
universe = dimnames(f$art)[[2]]

mscibm = c("mxuslc", "mxussc", "mxus000g", "MXUS", "mxus000v")
mcsids = c(     "L",      "S",        "G",    "M",        "V")

msci = list()

for(i in 1: length(mscibm)){
  msci[[i]] <- fetch_AA("AA_market", inputs$S, inputs$E, mscibm[i], "art", idxAsId = T)$val
  msci[[i]] <- msci[[i]][1:length(dates)]
  names(msci[[i]]) <- dates
}
names(msci) = mscibm

####

dd$spx = d$i.bR

ddd = do.call(cbind.data.frame, dd)
dddd = as.matrix(cumprod(ddd+1))



ddddd = as.data.frame(as.table(dddd))
names(ddddd) = c("Date", "BM", "Val")
plot_ly(ddddd, yaxis = F) %>%
  #     add_trace(x = as.Date(dfn$Var1), y = dfn$Freq, type = 'scatter', name = 'N',
  #               marker = list(color = '#C9EFF9')
  #               ) %>%
  add_trace(df, x = ~as.Date(Date), y = ~Val, type = 'scatter', mode = 'lines',
            yaxis = 'y2', name = 'ntile', 
            #               line = list(
            #                 color = c("black", "red", colorRampPalette(RColorBrewer::brewer.pal(11,"Spectral"))(10))
            #               )
            color = ~BM
  )
####



require(rlist)
f = list.remove(f, c("msci"))
f = c(f, list(msci = msci))




rt = f$art
rt = cbind(as.matrix(do.call(cbind.data.frame, msci)), rt)
rt = rt - f$i.bR
t = 36
n = 36
while(t <= length(dates)){
  corr = cor(rt[(t-n+1):t, ])[-(1:length(msci)), 1:length(msci)]
  
  corr_rank = t(apply(corr, 1, function(x) rank(x)))
  colnames(corr_rank) = colnames(corr)
  
  t = t + 1
}


cor(ddd)

pn <- function(X){crossprod(!is.na(X))}
cor.prob <- function(X){
  # Correlations Below Main Diagonal
  # Significance Tests with Pairwise Deletion
  # Above Main Diagonal
  # Believe part of this came from Bill Venables
  pair.SampSize <- pn(X)
  above1 <- row(pair.SampSize) < col(pair.SampSize)
  pair.df <- pair.SampSize[above1] - 2
  R <- cor(X, use="pair")
  above2 <- row(R) < col(R)
  r2 <- R[above2]^2
  Fstat <- (r2 * pair.df)/(1 - r2)
  R[above2] <- 1 - pf(Fstat, 1, pair.df)
  R
}









library(PerformanceAnalytics)
chart.Correlation(mydata)
require(psych)
pairs.panels(dddd)




# Ntile Portfolio test
source("AA_Plot.r")
fromDate <- "2003-04-01"
fromDate <- NA
ntile = 10
obj = paste0("f$", names(inputs$ac)[-1:-4])
paste0(1:length(obj), ":", obj)
i = 1
dddd = data.frame(row.names = dimnames(f$art)[[1]])

d$fltr[] = 0
d$fltr = getFltr(f, d, inputs)

while(i <= length(obj)){
  d = getNtileRtn(d, f, obj[i], ntile, filter = T)
  # dddd[[substring(obj[i], 3)]] = cumExVal(d, inputs, fromDate = NA)
  
  p <- plotNtile(d, inputs, fromDate = NA)
  htmlwidgets::saveWidget(p, paste0("C:/FOF_Decile_Test/20161228_All_factor__after_filter/", ntile,"_ntile_", inputs$ac[[substring(obj[i], 3)]] ,".html"))
  i = i + 1
}
pd = cbind(d$i.pR, d$i.bR, d$i.pR - d$i.bR)  # Plot Data
pd[d$i.s.t, ] = rep(0, 3)
pd = pd[-(1:(d$i.s.t - 1)), ]
colnames(pd) = c("Portfolio", "Benchmark", "Excess Return")
cat(inputs$f.f$wgt, "IR=", InformationRatio(pd[ ,1], pd[ ,2]), "MD=", maxDrawdown(pd[ ,3]), "DD=", DrawdownDeviation(pd[ ,3]), "\n")




# Team2 request info ####
#### release 1 : 3mUSD/day, 40 stocks
inputs$nPort = 40
inputs$fltr_abs$aPnt[which(inputs$fltr_abs$name == "d$tVal3")] = 3 * 10^6 * 20
d = dPrep(f, inputs)
d$ftrB4Fltr = Reduce('+', lapply(1:length(inputs$f.f$acct), function(x) eval(parse(text=paste0("d$f", x))) * inputs$f.f$wgt[x]))
d$ftr = d$ftrB4Fltr + getFltr(f, d, inputs)
d = portConstruction(d, f, inputs)
bbu1 <- bbu.csv.data(d$sHldW, inputs)
#### release 2 : 3mUSD/day, 100 stocks
inputs$nPort = 100
inputs$fltr_abs$aPnt[which(inputs$fltr_abs$name == "d$tVal3")] = 3 * 10^6 * 20
d$ftr = d$ftrB4Fltr + getFltr(f, d, inputs)
bbu2 <- bbu.csv.data(portConstruction(d, f, inputs)$sHldW, inputs)
#### release 3 : 7mUSD/day, 40 stocks
inputs$nPort = 40
inputs$fltr_abs$aPnt[which(inputs$fltr_abs$name == "d$tVal3")] = 7 * 10^6 * 20
d$ftr = d$ftrB4Fltr + getFltr(f, d, inputs)
bbu3 <- bbu.csv.data(portConstruction(d, f, inputs)$sHldW, inputs)
#### release 4 : 7mUSD/day, 100 stocks  ## and store ftr before filtering
inputs$nPort = 100
inputs$fltr_abs$aPnt[which(inputs$fltr_abs$name == "d$tVal3")] = 7 * 10^6 * 20
d$ftr = d$ftrB4Fltr + getFltr(f, d, inputs)
d = portConstruction(d, f, inputs)
bbu4 <- bbu.csv.data(d$sHldW, inputs)
#### BB Update file release
bbu <- rbind(bbu1, bbu2, bbu3, bbu4)
name_map = list(VALUE = "ChinaValue", TONG = "TongChina")  ##HARD##
write.table(bbu, file = paste0("M:/차이나 직접운용/Screeening/", name_map[inputs$ASTRATEGY], "_BBU.csv"), sep = ",", row.names = F, col.names = F)  ##HARD##
#### all factor summary release (before filtering)
path = "M:/차이나 직접운용/Screeening/"
ftr.detail.writer(inputs, f, d, "2016-12-31", path)







############################################################scratch (garbage) ####

t=34
cbind(
d$sHldW[t, which(d$sHldW[t,]>0)],
f$uSec[t, which(d$sHldW[t,]>0)],
d$ftr[t, which(d$sHldW[t,]>0)],
deparse.level = 2)


data(managers)

chart.RelativePerformance.Cus(d$i.pR, d$i.bR, type = "h", xaxis = F, yaxis.right = T, ylab = "", colorset = "darkolivegreen2")

system('CMD /C "ECHO The R process has finished running && PAUSE"', 
       invisible=FALSE, wait=FALSE)




# Weighting test
wgt = NA
wgt = read.csv("aa/QA/Rcommon/wgt.csv", header = F, sep = ",", stringsAsFactors = F)
source("aa/QA/Rcommon/AA_Functions.r")
for (i in 1:nrow(wgt)){
  inputs$f.f$wgt = as.numeric(wgt[i, ])
  interval = 0.025
  local_search(as.numeric(wgt[i, ]), f, d, inputs, interval , 2, highest_scroe = 0.96)
}

wgt = NA
wgt = read.csv("aa/QA/Rcommon/wgt.csv", header = F, sep = ",", stringsAsFactors = F)

inputs$f.f$wgt = as.numeric(wgt[1, ])









apply(f$mom2, 1, function(x) length(which(!is.na(x))))


dates    = dimnames(f$art)[[1]]
universe = dimnames(f$art)[[2]]
a = cbind(
  apply(f$mktcap, 1, function(x) length(which(!is.na(x)))),
  apply(f$grpr2, 1, function(x) length(which(!is.na(x)))),
  apply(f$grprps2, 1, function(x) length(which(!is.na(x)))),
  apply(f$mktcap, 1, function(x) length(which(!is.na(x)))) - apply(f$grpr2, 1, function(x) length(which(!is.na(x)))),
  apply(f$shout, 1, function(x) length(which(!is.na(x))))
)
colnames(a) = c("mktcap", "grpr2", "grprps2", "mktcap-grpr", "shout")

a[100:128,]

universe[which(!is.na(f$grpr[128, ]) & is.na(f$grprps[128, ]))]

universe[which(is.na(f$mktcap[127, ]) & !is.na(f$grpr[127, ]))]

