## ----setup, include = FALSE----------------------------------------------
library(knitr)
opts_chunk$set(fig.path = 'figure/beamer-', fig.align = 'center',
               fig.show = 'hold', size = 'footnotesize')
## markup inline code http://stackoverflow.com/a/16406120/173755
knit_hooks$set(inline = function(x) {
  if (is.numeric(x)) return(knitr:::format_sci(x, 'latex'))
  highr:::hi_latex(x)
})
# make the printing fit on the page
options(width = 70, digits = 3, show.signif.stars=FALSE)
par(mar = c(4, 4, .1, .1)) # reduce space above/below plots
set.seed(1121)   # make the results repeatable
library(gnm)
library(logmult)

## ----glm, eval = FALSE---------------------------------------------------
## glm(y ~ row + col, family = poisson)

## ----quasiIndep, eval = FALSE--------------------------------------------
## y ~ row + col + Diag(row, col)

## ----quasiSymm, eval = FALSE---------------------------------------------
## y ~ row + col + Symm(row, col)

## ----Symm, eval = FALSE--------------------------------------------------
## y ~ Symm(row, col)

## ----RChomog, tidy = FALSE-----------------------------------------------
RCh <- gnm(Freq ~ origin + destination + Diag(origin, destination) +
               MultHomog(origin, destination), family = poisson,
           data = occupationalStatus, verbose = FALSE)
getContrasts(RCh, pickCoef(RCh, "MultHomog"))

## ----getContrasts--------------------------------------------------------
getContrasts(RCh, pickCoef(RCh, "MultHomog"), ref = "last")

## ----mentalHealth--------------------------------------------------------
xtabs(count ~ SES + MHS, mentalHealth)

## ----trtContr------------------------------------------------------------
mentalHealth$MHS <- C(mentalHealth$MHS, treatment)
mentalHealth$SES <- C(mentalHealth$SES, treatment)

## ----RC------------------------------------------------------------------
RC <- gnm(count ~ SES + MHS + Mult(SES, MHS), family = poisson,
          data = mentalHealth, verbose = FALSE, ofInterest = "Mult")
coef(RC)

## ----colScores-----------------------------------------------------------
colProbs <- with(mentalHealth, tapply(count, MHS, sum) / sum(count))
colScores <- getContrasts(RC, pickCoef(RC, "[.]MHS"), ref = colProbs,
                          scaleRef = colProbs, scaleWeights = colProbs)
colScores

## ----rowScores-----------------------------------------------------------
rowProbs <- with(mentalHealth, tapply(count, SES, sum) / sum(count))
rowScores <- getContrasts(RC, pickCoef(RC, "[.]SES"), ref = rowProbs,
                          scaleRef = rowProbs, scaleWeights = rowProbs)

## ----assoc---------------------------------------------------------------
phi <- pickCoef(RC, "[.]SES", value = TRUE)
psi <- pickCoef(RC, "[.]MHS", value = TRUE)
sqrt(sum(rowProbs*(phi - sum(rowProbs*phi))^2)) *
         sqrt(sum(colProbs*(psi - sum(colProbs*psi))^2))


## ----RC2-----------------------------------------------------------------
RC2 <- update(RC, count ~ SES + MHS + instances(Mult(SES, MHS), 2))

## ----MHtab---------------------------------------------------------------
MHtab <- xtabs(count ~ SES + MHS, data = mentalHealth)
rc(MHtab, verbose = FALSE)

## ----RC_update, results = "hide", fig.show = "hide"----------------------
RC <- rc(MHtab, se = "jackknife", verbose = FALSE, ncpus = 1)
plot(RC, conf.int = 0.95)

## ----RC2_update, results = "hide", fig.show = "hide"---------------------
RC2 <- rc(MHtab, nd = 2, se = "jackknife", verbose = FALSE, ncpus = 1)
plot(RC2, conf.int = 0.95)

## ----anoas---------------------------------------------------------------
anoas(MHtab, nd=2, verbose = FALSE)

## ----unidiffGNM, eval = FALSE--------------------------------------------
## unidiff <- gnm(y ~ row:table + col:table + Mult(Exp(table), row:col),
##                family = poisson)

## ----yaish, eval = FALSE-------------------------------------------------
## yaish <- as.table(yaish[,,-7])
## yaish <- aperm(yaish, c("orig", "dest", "educ"))

## ----plotLayer, eval = FALSE---------------------------------------------
## plot(model, se.type = "se")

## ----segments, eval = FALSE----------------------------------------------
## segments(1:5+0.1, exp(conf[,1]), 1:5+0.1, exp(conf[,2]), col = "red")

## ----backPain------------------------------------------------------------
backPain[1:5,]

## ----backPainLong--------------------------------------------------------
backPainLong <- expandCategorical(backPain, "pain", group = TRUE)
head(backPainLong)

## ----stereotype----------------------------------------------------------
stereotype <- gnm(count ~ pain + Mult(pain, x1 + x2 + x3),
                  eliminate = id, family = poisson,
                  data = backPainLong, verbose = FALSE)

## ----multLogistic--------------------------------------------------------
logistic <- gnm(count ~ pain + pain:(x1 + x2 + x3),
                eliminate = id, family = poisson, data = backPainLong)
anova(stereotype, logistic)

## ----constrainStereotype, results = "hide"-------------------------------
stereotype <- update(stereotype,
                     . ~ pain + Mult(pain, offset(x1) + x2 + x3),
                     constrain = "[.]paincomplete.relief",
                     constrainTo = 1)

## ----ofInterestsAssign, results = "hide"---------------------------------
ofInterest(stereotype) <- pickCoef(stereotype, "Mult")

## ----parameters----------------------------------------------------------
parameters(stereotype)

## ----stereotype5---------------------------------------------------------
.pain <- backPainLong$pain
levels(.pain)[2:3] <- paste(levels(.pain)[2:3], collapse = " | ")
stereotype5 <- update(stereotype,
                      ~ pain + Mult(.pain, x1 + x2 + x3))
anova(stereotype, stereotype5)

## ----stereotypeOther, echo = FALSE---------------------------------------
levels(.pain)[4:5] <- paste(levels(.pain)[4:5], collapse = " | ")
stereotype4 <- update(stereotype5)
levels(.pain)[2:3] <- paste(levels(.pain)[2:3], collapse = " | ")
stereotype3 <- update(stereotype4)
levels(.pain)[2:3] <- paste(levels(.pain)[2:3], collapse = " | ")
stereotype2 <- update(stereotype3)
anova(stereotype, stereotype5, stereotype4, stereotype3, stereotype2)

## ----House2001prep, echo = FALSE-----------------------------------------
## Put the votes in a matrix, and discard members with too many NAs etc:
House2001m <- as.matrix(House2001[-1])
informative <- apply(House2001m, 1,
                     function(row){
                         valid <- !is.na(row)
                         validSum <- if (any(valid)) sum(row[valid]) else 0
                         nValid <- sum(valid)
                         uninformative <- (validSum == nValid) || (validSum == 0) || (nValid < 10)
                         !uninformative})
House2001m <- House2001m[informative, ]
## Make a vector of colours, blue for Republican and red for Democrat:
parties <- House2001$party[informative]
## Expand the data for statistical modelling:
House2001v <- as.vector(House2001m)
House2001f <- data.frame(member = rownames(House2001m),
                         party = parties,
                         rollCall = factor(rep((1:20),
                             rep(nrow(House2001m), 20))),
                         vote = House2001v)
voteAdj <- 0.5 + 0.94*(House2001f$vote - 0.5)

## ----residSVD------------------------------------------------------------
baseModel <- glm(vote ~ -1 + rollCall,
                 family = binomial, data = House2001f)
Start <- residSVD(baseModel, rollCall, member)

## ----rasch1--------------------------------------------------------------
rasch1 <- gnm(voteAdj ~ Mult(rollCall, member),
              eliminate = rollCall,
              family = binomial, data = House2001f,
              na.action = na.exclude, tolerance = 1e-03,
              start = -Start, verbose = FALSE)

## ----raschPlots, fig.show = "hold", out.width = "0.49\\linewidth"--------
plot(pickCoef(rasch1, "[.]member", value = TRUE),
     col = c("red", "black", "black", "blue")[parties],
     xlab = "Alphabetical index", ylab = "Member's relative position")
dotchart(pickCoef(rasch1, "[.]rollCall", value = TRUE),
         paste0("Roll call ", 1:20))

## ----LeeCarter, eval = FALSE---------------------------------------------
## LCmodel <- gnm(Deaths ~ Mult(Exp(Age), Year),
##                eliminate = Age, offset = log(Exposure),
##                family = "quasipoisson")

## ----eliminated, eval = FALSE--------------------------------------------
## AgeCoef <- attr(coef(model1), "eliminated")

## ----start, eval = FALSE-------------------------------------------------
## start = c(AgeCoef, rep(0, length(AgeCoef)), 0, coef(model1))

## ----conformity, echo = FALSE--------------------------------------------
conformity <- read.table("E:/Repos/gnm-svn/DataSets/Van_der_Slik/conformity.txt",
                         colClasses = c("character", "numeric", "numeric",
                         "factor", "factor", rep("numeric", 6)))

## ----A-------------------------------------------------------------------
A <- gnm(MCFM ~ -1 +
             AGEM + MRMM + FRMF + MWORK + MFCM + Dref(MOPLM, FOPLF),
           family = gaussian, data = conformity, verbose = FALSE)

## ----w, message = FALSE--------------------------------------------------
w <- DrefWeights(A)
w

## ----wCI-----------------------------------------------------------------
w$MOPLM["weight"] + qnorm(c(0.025, 0.975)) * w$MOPLM["se"]

## ----A2, echo = FALSE----------------------------------------------------
A2 <- update(A, . ~ -1 +  AGEM + MRMM + FRMF + MWORK + MFCM + FOPLF)
anova(A2, A, test = "Chisq")

## ----F-------------------------------------------------------------------
F <- gnm(MCFM ~ -1 + AGEM + MRMM + FRMF + MWORK + MFCM +
          Dref(MOPLM, FOPLF, delta = ~ 1 + MFCM),
          family = gaussian, data = conformity, verbose = FALSE)

## ----wF, message = FALSE-------------------------------------------------
DrefWeights(F)

## ----TypeII--------------------------------------------------------------
TypeII <- function(x){
  list(predictors = list(a = 1, h = 1),
       variables = list(substitute(x)))
}
class(TypeII) <- "nonlin"

## ----paste0--------------------------------------------------------------
term = function(predLabels, varLabels){
    paste0(predLabels[1], "*", varLabels[1], "/(1 + ",
           predLabels[1], "*", predLabels[2], "*", varLabels[1], ")")
}
term(c("a", "h"), "x")

## ----sprintf-------------------------------------------------------------
term = function(predLabels, varLabels){
    sprintf("%s * %s / (1 + %s * %s * %s)",
            predLabels[1], varLabels[1],
            predLabels[1], predLabels[2], varLabels[1])
}

## ----nonlin--------------------------------------------------------------
TypeII <- function(x){
  list(predictors = list(a = 1, h = 1),
       variables = list(substitute(x)),
       term = function(predLabels, varLabels){
           sprintf("%s * %s / (1 + %s * %s * %s)",
                   predLabels[1], varLabels[1],
                   predLabels[1], predLabels[2], varLabels[1])
})
}
class(TypeII) <- "nonlin"

## ----prey----------------------------------------------------------------
Density <- rep(c(2,5,10,15,20,30), each = 4)
Eaten <- c(1,1,0,0,2,2,1,1,1,2,3,2,2,2,3,3,3,3,4,3,3,3,4,3)

## ----mod1----------------------------------------------------------------
mod1 <- gnm(Eaten ~ -1 + TypeII(Density), start = c(a = 0.1, h = 0.1),
            family = quasipoisson(link = "identity"))

## ----mod1Summary, echo = FALSE-------------------------------------------
summary(mod1)

## ----factor--------------------------------------------------------------
TypeII <- function(C, x){
  list(predictors = list(a = substitute(C), h = substitute(C)),
       variables = list(substitute(x)),
       term = function(predLabels, varLabels){
           sprintf("%s * %s / (1 + %s * %s * %s)",
                   predLabels[1], varLabels[1],
                   predLabels[1], predLabels[2], varLabels[1])
})
}
class(TypeII) <- "nonlin"

## ----factorResult--------------------------------------------------------
Catchment <- factor(rep(1:2, 6, each = 2))
mod2 <- gnm(Eaten ~ -1 + TypeII(Catchment, Density),
            start = rep(0.2, 4),
            family = quasipoisson(link = "identity"))
coef(mod2)

## ----formula-------------------------------------------------------------
TypeII <- function(f, x){
  list(predictors = list(a = f, h = f),
       variables = list(substitute(x)),
       term = function(predLabels, varLabels){
           sprintf("(%s) * (%s)/ (1 + (%s) * (%s) * %s)",
                   predLabels[1], varLabels[1],
                   predLabels[1], predLabels[2], varLabels[1])
})
}
class(TypeII) <- "nonlin"

## ----formulaResult-------------------------------------------------------
mod2 <- gnm(Eaten ~ -1 + TypeII(~ 1 + Catchment, Density),
            start = c(0.2, -0.1, 0.2, -0.1),
            family = quasipoisson(link = "identity"))
coef(mod2)

## ----binomial, eval = FALSE----------------------------------------------
## count <- with(voting, percentage/100 * total)
## yvar <- cbind(count, voting$total - count)

## ----upward, eval = FALSE------------------------------------------------
## origin <- as.numeric(as.character(voting$origin))
## destination <- as.numeric(as.character(voting$destination))
## upward <- origin > destination

## ----inOut, eval = FALSE-------------------------------------------------
## in1 <- origin != 1 & destination == 1
## out1 <- origin == 1 & destination != 1

