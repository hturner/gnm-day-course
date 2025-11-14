#'Practical 4b
#'============

#' Step 1
#' ------
Richard <- function(time){
    list(predictors = list(A = 1, K = 1, B = 1, M = 1, v = 1),
       variables = list(substitute(time)),
       term = function(predLabels, varLabels){
         sprintf("%s + (%s - %s)/((1 + exp(-%s * (%s - %s)))^(1/%s))",
                 predLabels[1], predLabels[2],predLabels[1],
                 predLabels[3], varLabels[1], predLabels[4],
                 predLabels[5])
       })
}
class(Richard)<- "nonlin"

Richard(time)$term(c("A", "K", "B", "M", "v"), c("t"))


#' Step 2
#' ------
dat <- read.table("data/Richard.txt")
plot(y ~ t, data = dat)
mod1 <- gnm(y ~ Richard(t) - 1, data = dat,
            start = c(A = 0, K = 100, B = -0.5, M = 10, v = 1))

#' Step 3
#' ------
plot(y ~ t, data = dat)
lines(fitted(mod1) ~ t, data = dat)
summary(mod1)

mod2 <- update(mod1, constrain = "v", constrainTo = 1)
summary(mod2)

# avoid bug in gnm 1.1-5 when constraining >1 parameter by name: 
# use parameter indices instead, e.g. A = 0, v = 1
mod3 <- update(mod1, constrain = c(1, 5), constrainTo = c(0, 1))
summary(mod3)

# A = 0, K = 100, v = 1
mod4 <- update(mod1, constrain = c(1, 2, 5), constrainTo = c(0, 100, 1))
summary(mod4)

# A = 0, K = 100
mod5 <- update(mod1, constrain = c(1, 2), constrainTo = c(0, 100))
summary(mod5)

#' P.S.
#' ----
#'
#' If assuming data are Gaussian, using nls makes more sense, e.g.
mod1nls <- nls(y ~ A + (K - A)/(1 + exp(-B*(t - M)))^(1/v), data = dat,
               start = c(A = 0, K = 100, B = -0.5, M = 10, v = 1))
