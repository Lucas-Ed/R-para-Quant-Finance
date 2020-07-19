library(tidyverse)
library(ggthemes)
library(tidyquant)
library(RQuantLib)


acao <- "COGN3.SA"
p_exer <- 5.00
d_exer <- as.Date("2020-07-20")
d_atual <- as.Date("2020-06-29")
dias <- seq(d_atual, d_exer, by = 1)
dias <- dias[isBusinessDay("Brazil", dias)]
nsims <- 10000
ndias <- length(dias) - 1
sim_nomes <- paste0("sim", 1:nsims)

# Carregar os precos historicos da acao
p_hist <- tq_get(acao, from = d_atual - years(1), to = d_atual + days(1)) %>% 
  filter(volume != 0.0)
ret_hist <- p_hist %>% 
  tq_mutate(select = adjusted,
            mutate_fun = periodReturn,
            period = "daily",
            type = "log",
            leading = FALSE,
            col_rename = "log_ret") %>% 
  na.omit()
rf <- log(1 + 0.00225)
div <- 0
S0 <- last(ret_hist$adjusted)
P0 <- 0.14
mi <- 252 * mean(ret_hist$log_ret) # retorno medio em termos anuais
sigma <- EuropeanOptionImpliedVolatility("put", P0, S0, p_exer, div, rf, 
                                         (ndias + 1) / 252, 0.30)
# Funcao para realizar uma simulacao
mc_sim_fun <- function(valor_i, N, media, volat){
  med_d <- media / 252
  volat_d <- volat / sqrt(252)
  ans <- tibble(c(valor_i, rnorm(N, med_d - (volat_d^2 / 2), volat_d))) %>% 
    `colnames<-`("log_ret") %>%
    mutate(ret_ac = cumsum(log_ret)) %>% 
    select(ret_ac)
  
  return(ans)
}

# Funcao para precificar uma opcao europeia
eur_option <- function(type, underlying, strike, dividendYield, riskFreeRate, 
                       maturity, volatility) {
  if (maturity == 0.0) {
    ans <- switch(type,
                  put = max(strike - underlying, 0),
                  call = max(underlying - strike, 0))
    return(ans)
  }
  
  ans <- EuropeanOption(type = type,
                        underlying = underlying,
                        strike = strike,
                        dividendYield = dividendYield,
                        riskFreeRate = riskFreeRate,
                        maturity = maturity,
                        volatility = volatility)$value
  return(ans)
}
# Simulacao de Monte Carlo
# Valores Iniciais
inic <- rep(0, nsims) 
set.seed(12345)
ret_ac_mc <- map_dfc(inic,
                     mc_sim_fun,
                     N = ndias,
                     media = mi,
                     volat = sigma)

precos_acao <- (S0 * exp(ret_ac_mc)) %>% 
  set_names(sim_nomes) %>% 
  mutate(anos_exp = (ndias:0) / 252) %>% 
  gather(key = sims, value = St, -anos_exp)

# Evolucao do Portfolio
port_mc <- precos_acao %>% 
  mutate(Pt = map2_dbl(St, anos_exp, 
                       ~eur_option(type = "put",
                                   underlying = .x,
                                   strike = p_exer,
                                   dividendYield = div,
                                   riskFreeRate = rf,
                                   maturity = .y,
                                   volatility = sigma)),
         port_valor = Pt + St,
         data = rep(dias, nsims))
head(port_mc)

brk <- round(sort(c(p_exer, seq(min(port_mc$St),
                                max(port_mc$St),
                                length.out = 5))),
             digits = 2)
ggplot(port_mc, aes(x = data, y = St)) + 
  geom_line(aes(color = sims)) +
  geom_hline(yintercept = p_exer, color = "red") +
  guides(color = FALSE) +
  labs(title = "Simulações do Valor da Ação",
       x = "data",
       y = "Valor (R$)") +
  scale_y_continuous(breaks = brk) +
  scale_x_date(date_breaks = "2 days", date_labels = "%d") +
  scale_color_viridis_d() +
  theme_economist_white()

p_baixo <- port_mc %>% 
  filter(data == d_exer) %>% 
  summarise(num_baixo = sum(St < p_exer)) %>% 
  as.double()
prob <- p_baixo / nsims

