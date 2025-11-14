#'Practical 3
#'===========

#' Step 1
#' ------
Canada <- read.table("data/Canada.txt")
Canada$Year <- as.factor(Canada$Year)
Canada$Age <- as.factor(Canada$Age)
model1 <- gnm(Deaths ~ Year,
              offset = log(Exposure), eliminate = Age,
              family = "quasipoisson", data = Canada)


#' Step 2
#' ------
AgeCoef <- attr(coef(model1), "eliminated")
LCmodel <- update(model1, . ~ Mult(Exp(Age), Year),
                  start = c(AgeCoef, rep(0, length(AgeCoef)), 0, coef(model1)))

#' Step 3
#' ------
deviance(LCmodel)
df.residual(LCmodel)

#' Step 4
#' ------
res <- residuals(LCmodel, type = "pearson")
plot(res ~ Age, data = Canada)
age <- as.numeric(as.character(Canada$Age))
plot(res ~ Year, data = Canada, subset = age > 24 & age < 36)
plot(res ~ Year, data = Canada, subset = age > 49 & age < 66)


#' Step 5
#' ------
LCmodel2 <- update(LCmodel, subset = age > 44, start = NULL)
deviance(LCmodel2)
df.residual(LCmodel2)
res <- residuals(LCmodel2, type = "pearson")
plot(res ~ Age, data = model.frame(LCmodel2))
plot(res ~ Year, data = model.frame(LCmodel2))


#' Step 6
#' ------
contr <- getContrasts(LCmodel2, pickCoef(LCmodel2, "[.]Age"))
plot(contr, levelNames = 45:98)


