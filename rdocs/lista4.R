pacman::p_load(tidyverse,readxl)
df <- read_excel("dados/ConsumoEnergiaEAgua.xlsx",
                 sheet = "Dados", col_types = c("date","skip", "numeric",
                                                "numeric", "skip","skip",
                                                "skip", "skip", "skip", "skip",
                                                "skip"))
df$ano = factor(year(df$mes))
df$mes = factor(month(df$mes, label = TRUE))
df = df |>
  mutate(gasto_medio_diario = Energia/Dias)
df = df[,c(4,1,5)]

df = rbind(df,c('2024','mar',419/33))
df$gasto_medio_diario = as.numeric(df$gasto_medio_diario)

rho = acf(df$gasto_medio_diario, lag.max = nrow(df), plot = FALSE)
plot(rho)

rho = acf(df$gasto_medio_diario, lag.max = 36, plot = FALSE)
plot(rho)

phi = pacf(df$gasto_medio_diario, lag.max = nrow(df), plot = FALSE)
plot(phi)

phi = pacf(df$gasto_medio_diario, lag.max = 36, plot = FALSE)
plot(phi)

df$gasto_medio_diario1 = c(NA,(df$gasto_medio_diario[1:(nrow(df)-1)]))

df$dif = c(NA,diff(df$gasto_medio_diario))
plot(df$dif, type = "l")

x = na.omit(df$dif)
rho = acf(x, lag.max = nrow(x), plot = FALSE)
plot(rho)

phi = pacf(x, lag.max = nrow(x), plot = FALSE)
plot(phi)

pacman::p_load(pracma)
x = na.omit(df$dif)
n.size = nrow(df)
n.training = ceiling(n.size/2)
observed = NULL
predicted = NULL
for(t in (n.training+1):n.size){
  x.training = x[1:(t-1)]
  rho = acf(x.training, lag=(t-1),plot=F)
  last.lag = length(rho$acf)
  Rho = rho$acf
  Omega = toeplitz(Rho[-last.lag])
  beta = inv(Omega) %*% Rho[-1]
  beta.0 = mean(x.training)*(1-sum(beta))
  predicted[t] = beta.0 + sum(rev(beta)*x.training[-1])
  observed[t] = x[t]
}
plot(df$dif, type = "l")
lines(predicted, type = "l", col = "red")
cor(predicted,observed,use="complete.obs")

MAE = mean(abs(na.omit(rev(predicted)[-1])-na.omit(rev(observed)[-1])))
MAE

Y.hat = NULL
Y.t = NULL

for(h in (n.training+1):n.size){
Y.t[h+1] = df$gasto_medio_diario[h] + df$dif[h+1]
Y.hat[h+1] = df$gasto_medio_diario[h] + predicted[h+1]
}

MAE = mean(abs(na.omit(Y.hat)-na.omit(Y.t)))
MAE
MAPE = mean(abs(na.omit(Y.hat)/na.omit(Y.t)-1))
MAPE

plot(na.omit(Y.hat),na.omit(Y.t))
cor(na.omit(Y.hat),na.omit(Y.t))

shapiro.test(na.omit(Y.t-Y.hat))
sd(Y.t-Y.hat,na.rm=T)

# ---------------------------------------------------------------------------- #
# Exercício
# prevendo 1 mês a frente
pacman::p_load(forecast)

serie <- ts(df$gasto_medio_diario,
            frequency = 12,
            start = c(1997,6),
            end = c(2024,3))

# Usando modelo AR(2)
AR = 2
fit <- Arima(serie, order = c(AR, 1, 0))
Y.next <- forecast(fit, h = 1)
Y.next
forecast(fit, h = 1) %>% autoplot()
# ---------------------------------------------------------------------------- #