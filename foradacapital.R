# fora da capital

library(readxl)
library(glmnet)
library(dplyr)

estado <- read_excel("C:/Users/DLO/Downloads/BANCO_SP_EXCETOCAPITAL.xlsx")
matriz_sp <- estado[,-c(1:11)]

X_sp <- matriz_sp[, -((ncol(matriz_sp)-4):(ncol(matriz_sp)))]

X_sp <- scale(X_sp)

set.seed(2)
n <- nrow(as.matrix(X_sp))
divisao <- sample(1:n, size = 0.8 * n, replace = FALSE)
treino <- X_sp[divisao, ]
teste <- X_sp[-divisao, ]

############## Domicílios precários #############################

dp <- as.matrix(estado$`DOMICILIOS PRECARIOS`)

set.seed(2)
divisao <- sample(1:n, size = 0.8 * n, replace = FALSE)
treino_dp <- dp[divisao, ]
teste_dp <- dp[-divisao, ]


# LASSO
set.seed(2)
lasso.dp <- cv.glmnet(treino, treino_dp, alpha = 1, nfolds = 5)
cf.dp <- coef(lasso.dp, s = "lambda.min")
lasso.dp$lambda.min

var <- rownames(cf.dp)[cf.dp[, 1] != 0]

# Métricas
pred_dp <- predict(lasso.dp, newx = X_sp, s = "lambda.min")
# R² = 1 - (Soma dos quadrados dos resíduos / Soma total dos quadrados)
SSE <- sum((dp - pred_dp)^2)
SST <- sum((dp - mean(dp))^2)
R2 <- 1 - SSE/SST
R2
RMSE <- sqrt(mean((dp - pred_dp)^2))
RMSE
MAE <- mean(abs(dp - pred_dp))
MAE
plot(dp, pred_dp,
     xlab = "Valores observados",
     ylab = "Valores preditos",
     main = "Valores observados vs preditos (Domicílios Precários)", col = "darkblue")
abline(0, 1, col = "red", lwd = 2)  # linha ideal (y = ŷ)

residuos.dp <- dp - pred_dp
plot(pred_dp, residuos.dp,
     xlab = "Valores preditos",
     ylab = "Resíduos",
     main = "Resíduos vs preditos (Adensamento)")
abline(h = 0, col = "red", lwd = 2)

qqnorm(residuos.dp)
qqline(residuos.dp, col = "red", lwd = 2)

shapiro.test(residuos.dp)

############## Coabitação #############################

cb <- as.matrix(estado$COABITACAO)


set.seed(2)
divisao <- sample(1:n, size = 0.8 * n, replace = FALSE)
treino_cb <- cb[divisao, ]
teste_cb <- cb[-divisao, ]


# LASSO
set.seed(2)
lasso.cb <- cv.glmnet(treino, treino_cb, alpha = 1, nfolds = 5)
cf.cb <- coef(lasso.cb, s = "lambda.min")
lasso.cb$lambda.min

var <- rownames(cf.cb)[cf.cb[, 1] != 0]

# Métricas
pred_cb <- predict(lasso.cb, newx = X_sp, s = "lambda.min")
# R² = 1 - (Soma dos quadrados dos resíduos / Soma total dos quadrados)
SSE <- sum((cb - pred_cb)^2)
SST <- sum((cb - mean(cb))^2)
R2 <- 1 - SSE/SST
R2
RMSE <- sqrt(mean((cb - pred_cb)^2))
RMSE
MAE <- mean(abs(cb - pred_cb))
MAE
plot(cb, pred_cb,
     xlab = "Valores observados",
     ylab = "Valores preditos",
     main = "Valores observados vs preditos (Coabitação)", col = "darkblue")
abline(0, 1, col = "red", lwd = 2)  # linha ideal (y = ŷ)

residuos.cb <- cb - pred_cb
plot(pred_cb, residuos.cb,
     xlab = "Valores preditos",
     ylab = "Resíduos",
     main = "Resíduos vs preditos (Coabitação)")
abline(h = 0, col = "red", lwd = 2)

qqnorm(residuos.cb)
qqline(residuos.cb, col = "red", lwd = 2)

shapiro.test(residuos.cb)

############## ônus excessivo #############################

oe <- as.matrix(estado$`ONUS EXCESSIVO`)

set.seed(2)
divisao <- sample(1:n, size = 0.8 * n, replace = FALSE)
treino_oe <- oe[divisao, ]
teste_oe <- oe[-divisao, ]


# LASSO
set.seed(2)
lasso.oe <- cv.glmnet(treino, treino_oe, alpha = 1, nfolds = 5)
cf.oe <- coef(lasso.oe, s = "lambda.min")
lasso.oe$lambda.min

var <- rownames(cf.oe)[cf.oe[, 1] != 0]

# Métricas
pred_oe <- predict(lasso.oe, newx = X_sp, s = "lambda.min")
# R² = 1 - (Soma dos quadrados dos resíduos / Soma total dos quadrados)
SSE <- sum((oe - pred_oe)^2)
SST <- sum((oe - mean(oe))^2)
R2 <- 1 - SSE/SST
R2
RMSE <- sqrt(mean((oe - pred_oe)^2))
RMSE
MAE <- mean(abs(oe - pred_oe))
MAE
plot(oe, pred_oe,
     xlab = "Valores observados",
     ylab = "Valores preditos",
     main = "Valores observados vs preditos (Ônus Excessivo)", col = "darkblue")
abline(0, 1, col = "red", lwd = 2)  # linha ideal (y = ŷ)

residuos.oe <- oe - pred_oe
plot(pred_oe, residuos.oe,
     xlab = "Valores preditos",
     ylab = "Resíduos",
     main = "Resíduos vs preditos (Ônus Excessivo)")
abline(h = 0, col = "red", lwd = 2)

qqnorm(residuos)
qqline(residuos, col = "red", lwd = 2)

shapiro.test(residuos)

############## Adensamento #############################

ad <- as.matrix(estado$ADENSAMENTO)

set.seed(2)
divisao <- sample(1:n, size = 0.8 * n, replace = FALSE)
treino_ad <- ad[divisao, ]
teste_ad <- ad[-divisao, ]


# LASSO
set.seed(2)
lasso.ad <- cv.glmnet(treino, treino_ad, alpha = 1, nfolds = 5)
cf.ad <- coef(lasso.ad, s = "lambda.min")
lasso.ad$lambda.min

var <- rownames(cf.ad)[cf.ad[, 1] != 0]

# Métricas
pred_ad <- predict(lasso.ad, newx = X_sp, s = "lambda.min")
# R² = 1 - (Soma dos quadrados dos resíduos / Soma total dos quadrados)
SSE <- sum((ad - pred_ad)^2)
SST <- sum((ad - mean(ad))^2)
R2 <- 1 - SSE/SST
R2
RMSE <- sqrt(mean((ad - pred_ad)^2))
RMSE
MAE <- mean(abs(ad - pred_ad))
MAE
plot(ad, pred_ad,
     xlab = "Valores observados",
     ylab = "Valores preditos",
     main = "Valores observados vs preditos (Adensamento)", col = "darkblue")
abline(0, 1, col = "red", lwd = 2)  # linha ideal (y = ŷ)

residuos.ad <- ad - pred_ad
plot(pred_ad, residuos.ad,
     xlab = "Valores preditos",
     ylab = "Resíduos",
     main = "Resíduos vs preditos (Adensamento)")
abline(h = 0, col = "red", lwd = 2)

qqnorm(residuos.ad)
qqline(residuos.ad, col = "red", lwd = 2)

shapiro.test(residuos)
