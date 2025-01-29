# --- Load and prepare data
data("ReadingSkills", package = "betareg")
datTrain <- data.table(ReadingSkills)
datTrain[, dyslexia_Ind := ifelse(dyslexia == "yes", 1, 0)]


# --- Fit beta regression model
m <- betareg(accuracy ~ iq * dyslexia_Ind | iq + dyslexia_Ind, data = datTrain)
summary(m)


# --- Calculate pseudo R^2 from first principles
# Ferrari2004 describe their pseudo R^2 as the "squared sample correlation between the linear predictor
# and the link-transformed response g(y)".
# Additional sources include:
# https://stats.stackexchange.com/questions/231365/calculating-a-pseudo-r2-value-when-deviance-is-negative/231383#231383
# https://stats.stackexchange.com/questions/552281/calculating-different-pseudo-r2-for-a-betareg-model


# Get linear predictor: Method 1
lp <- m$coefficients$mean[1] + 
  as.matrix(datTrain[,list(iq, dyslexia_Ind, iq_dyslexia_Int= iq*dyslexia_Ind)]) %*% m$coefficients$mean[-1]
lp <- setNames(as.numeric(lp), 1:length(lp))

# Get linear predictor: Method 2
lp2 <- predict(m, type = "link")

# Equality?
all.equal(lp, lp2) # TRUE

# Pseudo R^2
(pseudoRsquared <- (cor(log(m$y/(1-m$y)), lp)) ^ 2)

pseudoRsquared == m$pseudo.r.squared # TURE

# - Cleanup
rm(datTrain, ReadingSkills, pseudoRsquared, m, lp, lp2)