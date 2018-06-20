#'Practical 1
#'===========

#' Step 1
#' ------
library(gnm)


#' Step 2
#' ------
plot(occupationalStatus)
occupationalStatus

#' Step 3
#' ------
nullModel <- gnm(Freq ~ origin + destination, family = poisson,
                 data = occupationalStatus)
nullModel


#' Step 4
#' ------
?plot.gnm
plot(nullModel, 1)


#' Step 5
#' ------
library(vcdExtra)
mosaic(nullModel)


#' Step 6
#' ------
multModel <- gnm(Freq ~ origin + destination + MultHomog(origin, destination),
                 family = poisson, data = occupationalStatus)
mosaic(multModel)
multModel2 <- gnm(Freq ~ origin + destination + Diag(origin, destination) +
                    MultHomog(origin, destination),
                 family = poisson, data = occupationalStatus)
mosaic(multModel2)

#' Step 7
#' ------
coef1 <- coef(multModel2)
multModel2 <- update(multModel2)
coef2 <- coef(multModel2)
cbind(coef1, coef2)

#' Step 8
#' ------
summary(multModel2)
multModel2b <- gnm(Freq ~ origin + destination + Diag(origin, destination) +
                      MultHomog(origin, destination),
                   family = poisson, data = occupationalStatus,
                   constrain = "MultHomog(origin, destination)1")
summary(multModel2b)

