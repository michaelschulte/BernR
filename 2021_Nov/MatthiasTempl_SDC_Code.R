library(dplyr)

## --------------------------------------------------------------------------------------------------------------
library(sdcMicro)
args(createSdcObj)


## ---- eval=FALSE-----------------------------------------------------------------------------------------------
## ?createSdcObj; ?testdata


## --------------------------------------------------------------------------------------------------------------
testdata$relat <- as.factor(testdata$relat) # needed afterwards
testdata$roof <- as.factor(testdata$roof) # needed afterwards
sdc <- createSdcObj(testdata,
  keyVars=c('urbrur','relat','sex','age','hhcivil'),
  numVars=c('expend','income','savings'), 
  w='sampling_weight',
  pramVars = "roof") # switch to R, explanation S4 class


## --------------------------------------------------------------------------------------------------------------
print(sdc, "kAnon")
sdc
print(sdc, "risk")
sdc@risk$individual

## --------------------------------------------------------------------------------------------------------------
sdc <- suda2(sdc)
slot(sdc, "risk")$suda2


## --------------------------------------------------------------------------------------------------------------
slot(sdc, "risk")$individual %>% head


## --------------------------------------------------------------------------------------------------------------
riskyCells(sdc, maxDim = 5, threshold = 3) %>% tail


## ---- eval=FALSE-----------------------------------------------------------------------------------------------
## # only makes sense after anonymization
## slot(sdc, "numrisk")


## ---- eval=FALSE-----------------------------------------------------------------------------------------------
## ?groupAndRename
## ?globalRecode


## --------------------------------------------------------------------------------------------------------------
sdc <- globalRecode(sdc, 
                    column="age", 
                    breaks=c(1,9,19,29,39,49,59,69,100))
print(sdc, "kAnon")
# print(sdc, "risk")


## --------------------------------------------------------------------------------------------------------------
sdc <- groupAndRename(sdc, 
                      var="relat", 
                      before=1:9, 
                      after=c(1:6,"7+","7+","7+"))
# print(sdc, "kAnon")
print(sdc, "risk")


## --------------------------------------------------------------------------------------------------------------
sdc <- kAnon(sdc, k = 3, importance = c(3,4,1,2,5))
print(sdc, "kAnon")
print(sdc, "risk")


## --------------------------------------------------------------------------------------------------------------
sdc <- pram(sdc)
print(sdc, "pram")


## --------------------------------------------------------------------------------------------------------------
sdc <- addNoise(sdc, method = "correlated2")
print(sdc, "numrisk")


## --------------------------------------------------------------------------------------------------------------
sdc <- undolast(sdc)
sdc <- addNoise(sdc, method = "additive", noise = 10)
sdc <- dRiskRMD(sdc)
slot(sdc, "risk")$numericRMD$wrisk2
print(sdc, "numrisk")
print(sdc, type="comp_numvars")

### Utility

## --------------------------------------------------------------------------------------------------------------
print(sdc, "ls")

## ---- echo=TRUE, message=FALSE, warning=FALSE---------------------------------
X <- testdata
Y <- extractManipData(sdc)


## ---- echo=TRUE---------------------------------------------------------------
ct <- c("roof", "hhcivil")
Tx <- table(X[, ct])
Ty <- table(Y[, ct])
Tx
Ty


## ---- echo=TRUE---------------------------------------------------------------
n1 <- nrow(Ty); n2 <- ncol(Ty)
## UT
sum(abs(Tx - Ty)) / (n1 * n2)

## ---- echo=TRUE, eval = FALSE, fig.height=4, fig.width=4, message=FALSE, warning=FALSE, out.width="5cm"----
require(vcd)
ct <- c("roof", "relat", "sex")
library(simPop)
Tx <- tableWt(X[, ct], X$sampling_weight)
Ty <- tableWt(Y[, ct], X$sampling_weight)
par(mfrow=c(1,2))
mosaic(Tx); mosaic(Ty)


## ---- echo = TRUE-------------------------------------------------------------
get.sdcMicroObj(sdc, "utility")




