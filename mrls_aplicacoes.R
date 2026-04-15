

############

library(aopdata)

# Definição da cidade e modo
cidade_aop <- "for"               # Fortaleza
modo_aop   <- "public_transport"  # transporte público
ano_aop    <- 2019

# Download dos dados
aop_raw <- read_access(
  city = cidade_aop,
  mode = modo_aop,
  peak = TRUE,
  year = ano_aop,
  geometry = FALSE,
  showProgress = TRUE
)

# Salva em formato RDS
saveRDS(aop_raw, "bases/aop_fortaleza.rds")

# Leitura local (usar nas análises)
aop_raw <- readRDS("bases/aop_fortaleza.rds")

# Conferir estrutura
head(aop_raw)

## ANAC

url_anac <- paste0(
  "https://sistemas.anac.gov.br/dadosabertos/",
  "Voos%20e%20opera%C3%A7%C3%B5es%20a%C3%A9reas/",
  "Dados%20Estat%C3%ADsticos%20do%20Transporte%20A%C3%A9reo/",
  "Dados_Estatisticos_parte.csv"
)


arquivo_anac <- "bases/anac_parte.csv"

# Aumenta tempo limite para download
options(timeout = 600)

# Baixa apenas se o arquivo não existir
if(!file.exists(arquivo_anac)){
  download.file(
    url_anac,
    destfile = arquivo_anac,
    mode = "wb"
  )
}


library(data.table)

anac_raw <- fread(
  arquivo_anac,
  encoding = "UTF-8",
  showProgress = TRUE
)

names(anac_raw)

saveRDS(anac_raw, "bases/anac_parte.rds")

anac_raw <- readRDS("bases/anac_parte.rds")
head(anac_raw)





# =========================================================
# CAPÍTULO DE APLICAÇÕES EM MRLS
# Script consolidado e corrigido
# =========================================================

library(dplyr)
library(ggplot2)
library(broom)
library(patchwork)
library(MASS)
library(janitor)
library(data.table)

theme_set(theme_minimal(base_size = 13))

# ---------------------------------------------------------
# 1) Leitura das bases salvas
# ---------------------------------------------------------

anac_raw <- readRDS("bases/anac_parte.rds")
aop_raw  <- readRDS("bases/aop_fortaleza.rds")

anac_raw <- janitor::clean_names(anac_raw)
aop_raw  <- janitor::clean_names(aop_raw)

names(anac_raw)
names(aop_raw)

write.csv2(anac_raw,file = file("bases/anac_raw.csv", encoding = "Latin1"), row.names = TRUE)

# =========================================================
# 2) APLICAÇÃO 1 — ANAC
# =========================================================

anac <- anac_raw |>
  dplyr::select(
    ano,
    mes,
    empresa_sigla,
    empresa_nome,
    aeroporto_de_origem_sigla,
    aeroporto_de_destino_sigla,
    distancia_voada_km,
    combustivel_litros,
    assentos,
    passageiros_pagos
  ) |>
  mutate(
    distancia_voada_km = as.numeric(distancia_voada_km),
    combustivel_litros = as.numeric(combustivel_litros),
    assentos = as.numeric(assentos),
    passageiros_pagos = as.numeric(passageiros_pagos)
  ) |>
  filter(
    !is.na(distancia_voada_km),
    !is.na(combustivel_litros),
    distancia_voada_km > 0,
    combustivel_litros > 0
  )

glimpse(anac)

#write.csv2(anac,file = file("bases/anac.csv", encoding = "Latin1"), row.names = TRUE)

summary(anac$distancia_voada_km)
summary(anac$combustivel_litros)

set.seed(123)
if(nrow(anac) > 25000){
  anac_plot <- slice_sample(anac, n = 25000)
} else {
  anac_plot <- anac
}


g1_anac <- ggplot(anac_plot, aes(x = distancia_voada_km, y = combustivel_litros)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "ANAC: combustível consumido e distância da etapa",
    x = "Distância da etapa (km)",
    y = "Combustível consumido (litros)"
  )

g1_anac

mod_anac_1 <- lm(combustivel_litros ~ distancia_voada_km, data = anac)

summary(mod_anac_1)
anova(mod_anac_1)
confint(mod_anac_1)
broom::tidy(mod_anac_1, conf.int = TRUE)

x0_anac <- median(anac$distancia_voada_km)
novo_anac <- data.frame(distancia_voada_km = x0_anac)

predict(mod_anac_1, newdata = novo_anac, interval = "confidence")
predict(mod_anac_1, newdata = novo_anac, interval = "prediction")

anac_aug <- augment(mod_anac_1)

g2_anac <- ggplot(anac_aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "ANAC - resíduos vs ajustados", x = "Valores ajustados", y = "Resíduos")

g3_anac <- ggplot(anac_aug, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line() +
  labs(title = "ANAC - QQ-plot", x = "Quantis teóricos", y = "Quantis amostrais")

g4_anac <- ggplot(anac_aug, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  labs(title = "ANAC - escala-localização", x = "Valores ajustados",
       y = expression(sqrt(abs("resíduo padronizado"))))

g5_anac <- ggplot(anac_aug, aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_col() +
  labs(title = "ANAC - distância de Cook", x = "Índice da observação", y = "Cook")

(g2_anac + g3_anac) / (g4_anac + g5_anac)

anac_aug |>
  mutate(indice = row_number()) |>
  dplyr::select(indice, .fitted, .resid, .std.resid, .hat, .cooksd) |>
  arrange(desc(.cooksd)) |>
  slice(1:10)

boxcox(mod_anac_1)

mod_anac_2 <- lm(log(combustivel_litros) ~ distancia_voada_km, data = anac)
mod_anac_3 <- lm(log(combustivel_litros) ~ log(distancia_voada_km), data = anac)

summary(mod_anac_2)
summary(mod_anac_3)

tibble(
  aplicacao = "ANAC",
  modelo = c("linear", "log_y", "log_log"),
  r2 = c(
    summary(mod_anac_1)$r.squared,
    summary(mod_anac_2)$r.squared,
    summary(mod_anac_3)$r.squared
  ),
  r2_aj = c(
    summary(mod_anac_1)$adj.r.squared,
    summary(mod_anac_2)$adj.r.squared,
    summary(mod_anac_3)$adj.r.squared
  ),
  AIC = c(AIC(mod_anac_1), AIC(mod_anac_2), AIC(mod_anac_3)),
  BIC = c(BIC(mod_anac_1), BIC(mod_anac_2), BIC(mod_anac_3))
)

# =========================================================
# 3) APLICAÇÃO 2 — AOP
# =========================================================

aop <- aop_raw |>
  mutate(
    cmatt60 = as.numeric(cmatt60),
    r001 = as.numeric(r001),
    p001 = as.numeric(p001)
  ) |>
  filter(
    !is.na(cmatt60),
    !is.na(r001),
    !is.na(p001),
    p001 > 0,
    r001 > 0,
    cmatt60 >= 0
  )

glimpse(aop)
summary(aop$cmatt60)
summary(aop$r001)

g1_aop <- ggplot(aop, aes(x = r001, y = cmatt60)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "AOP: acessibilidade a empregos e renda",
    x = "Renda domiciliar per capita média",
    y = "Empregos acessíveis em 60 minutos"
  )

g2_aop <- ggplot(aop, aes(x = log(r001), y = log1p(cmatt60))) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "AOP: log(1 + acessibilidade) e log(renda)",
    x = "log(renda domiciliar per capita média)",
    y = "log(1 + empregos acessíveis)"
  )

g1_aop + g2_aop

mod_aop_1 <- lm(cmatt60 ~ r001, data = aop)

summary(mod_aop_1)
anova(mod_aop_1)
confint(mod_aop_1)
broom::tidy(mod_aop_1, conf.int = TRUE)

x0_aop <- median(aop$r001)
novo_aop <- data.frame(r001 = x0_aop)

predict(mod_aop_1, newdata = novo_aop, interval = "confidence")
predict(mod_aop_1, newdata = novo_aop, interval = "prediction")

aop_aug <- augment(mod_aop_1)

g3_aop <- ggplot(aop_aug, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(title = "AOP - resíduos vs ajustados", x = "Valores ajustados", y = "Resíduos")

g4_aop <- ggplot(aop_aug, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line() +
  labs(title = "AOP - QQ-plot", x = "Quantis teóricos", y = "Quantis amostrais")

g5_aop <- ggplot(aop_aug, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  labs(title = "AOP - escala-localização", x = "Valores ajustados",
       y = expression(sqrt(abs("resíduo padronizado"))))

g6_aop <- ggplot(aop_aug, aes(x = seq_along(.cooksd), y = .cooksd)) +
  geom_col() +
  labs(title = "AOP - distância de Cook", x = "Índice da observação", y = "Cook")

(g3_aop + g4_aop) / (g5_aop + g6_aop)

aop_aug |>
  mutate(indice = row_number()) |>
  dplyr::select(indice, .fitted, .resid, .std.resid, .hat, .cooksd) |>
  arrange(desc(.cooksd)) |>
  slice(1:10)

mod_aop_2 <- lm(log1p(cmatt60) ~ r001, data = aop)
mod_aop_3 <- lm(log1p(cmatt60) ~ log(r001), data = aop)

summary(mod_aop_2)
summary(mod_aop_3)

tibble(
  aplicacao = "AOP",
  modelo = c("linear", "log1p_y", "log1p_y_log_x"),
  r2 = c(
    summary(mod_aop_1)$r.squared,
    summary(mod_aop_2)$r.squared,
    summary(mod_aop_3)$r.squared
  ),
  r2_aj = c(
    summary(mod_aop_1)$adj.r.squared,
    summary(mod_aop_2)$adj.r.squared,
    summary(mod_aop_3)$adj.r.squared
  ),
  AIC = c(AIC(mod_aop_1), AIC(mod_aop_2), AIC(mod_aop_3)),
  BIC = c(BIC(mod_aop_1), BIC(mod_aop_2), BIC(mod_aop_3))
)

# =========================================================
# 4) Comparação geral
# =========================================================

tibble(
  aplicacao = c("ANAC", "ANAC", "ANAC", "AOP", "AOP", "AOP"),
  modelo = c("linear", "log_y", "log_log", "linear", "log1p_y", "log1p_y_log_x"),
  r2 = c(
    summary(mod_anac_1)$r.squared,
    summary(mod_anac_2)$r.squared,
    summary(mod_anac_3)$r.squared,
    summary(mod_aop_1)$r.squared,
    summary(mod_aop_2)$r.squared,
    summary(mod_aop_3)$r.squared
  ),
  r2_aj = c(
    summary(mod_anac_1)$adj.r.squared,
    summary(mod_anac_2)$adj.r.squared,
    summary(mod_anac_3)$adj.r.squared,
    summary(mod_aop_1)$adj.r.squared,
    summary(mod_aop_2)$adj.r.squared,
    summary(mod_aop_3)$adj.r.squared
  ),
  AIC = c(
    AIC(mod_anac_1), AIC(mod_anac_2), AIC(mod_anac_3),
    AIC(mod_aop_1), AIC(mod_aop_2), AIC(mod_aop_3)
  ),
  BIC = c(
    BIC(mod_anac_1), BIC(mod_anac_2), BIC(mod_anac_3),
    BIC(mod_aop_1), BIC(mod_aop_2), BIC(mod_aop_3)
  )
)