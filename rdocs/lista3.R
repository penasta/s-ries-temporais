# Cryer & Chan - Questão 4.10 ----
theta <- c(0.6, -0.6, 0.95, 0.3)
lag <- c(10, 10, 20, 10)
l = list()
for (i in seq_along(theta)) {
  l[[i]] = (ARMAacf(ar = theta[i], lag.max = lag[i]))}

a = data.frame(y=l[1],x=1:11)
colnames(a) = c('y','x')
b = data.frame(y=l[2],x=1:11)
colnames(b) = c('y','x')
c = data.frame(y=l[3],x=1:21)
colnames(c) = c('y','x')
d = data.frame(y=l[4],x=1:11)
colnames(d) = c('y','x')

pacman::p_load(tidyverse,latex2exp)
# ggplot(a, aes(x = x, y = y)) +
#   geom_line() +
#   theme_classic()
# 
# ggplot(b, aes(x = x, y = y)) +
#   geom_line() +
#   theme_classic()
# 
# ggplot(c, aes(x = x, y = y)) +
#   geom_line() +
#   theme_classic()
# 
# ggplot(d, aes(x = x, y = y)) +
#   geom_line() +
#   theme_classic()

gridExtra::grid.arrange(ggplot(a, aes(x = x, y = y)) +
                          geom_line() +
                          geom_hline(yintercept = 0, linetype = "dashed") +
                          labs(title = TeX(r'(AR(1) com $\phi$ = 0,6)'),
                               x="Lag",
                               y="Função autocorrelação") +
                          theme_classic(),
                       ggplot(b, aes(x = x, y = y)) +
                         geom_line() +
                         geom_hline(yintercept = 0, linetype = "dashed") +
                         labs(title = TeX(r'(AR(1) com $\phi$ = -0,6)'),
                              x="Lag",
                              y="Função autocorrelação") +
                         theme_classic(),
                       ggplot(c, aes(x = x, y = y)) +
                         geom_line() +
                         geom_hline(yintercept = 0, linetype = "dashed") +
                         labs(title = TeX(r'(AR(1) com $\phi$ = 0,95)'),
                              x="Lag",
                              y="Função autocorrelação") +
                         theme_classic(),
                       ggplot(d, aes(x = x, y = y)) +
                         geom_line() +
                         geom_hline(yintercept = 0, linetype = "dashed") +
                         labs(title = TeX(r'(AR(1) com $\phi$ = 0,3)'),
                              x="Lag",
                              y="Função autocorrelação") +
                         theme_classic(),
                       nrow = 2, ncol = 2)

