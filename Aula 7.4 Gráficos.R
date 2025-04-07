library(WDI)
library(tidyverse)

options(scipen = 999)

basemilitar <- WDI(country = 'all',
                   indicator = 'MS.MIL.XPND.ZS')

basemilitar2023 <- WDI(country = 'all',
                       indicator = 'MS.MIL.XPND.ZS',
                       start = 2023, end = 2023)

basemilitarBR <- WDI(country = 'BR',
                     indicator = 'MS.MIL.XPND.ZS')            

# DADOS EM PAINEL

grafpainel <- ggplot(basemilitar,
                     mapping = aes(y = MS.MIL.XPND.ZS,
                                   x = year)) +
  
  geom_line()

print(grafpainel)

# CORTE TRANSVERSAL

grafcorte <- ggplot(basemilitar2023,
                    mapping = aes(y = MS.MIL.XPND.ZS,
                                  x = year)) +
  
  geom_line()

print(grafcorte)

# SÉRIE TEMPORAL

grafserie <- ggplot(basemilitarBR,
                    mapping = aes(y = MS.MIL.XPND.ZS,
                                  x = year)) +
  
  geom_line()

print(grafserie)


#com chat GPT

library(WDI)
library(ggplot2)
library(dplyr)
library(forcats)

dados_militar <- WDI(
  country = "all", 
  indicator = "MS.MIL.XPND.ZS", 
  start = 2000, end = 2023, 
  extra = TRUE
)

dados_militar <- dados_militar %>%
  filter(!is.na(region), !is.na(MS.MIL.XPND.ZS))

top_paises <- dados_militar %>%
  group_by(country, region) %>%
  summarize(media_gasto = mean(MS.MIL.XPND.ZS, na.rm = TRUE), .groups = "drop") %>%
  group_by(region) %>%
  top_n(5, media_gasto) %>%
  pull(country)
dados_top <- dados_militar %>%
  filter(country %in% top_paises)

ggplot(dados_top, aes(x = year, y = MS.MIL.XPND.ZS, color = country)) +
  geom_line(size = 1) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Gastos Militares (% do PIB)",
    subtitle = "Top 5 países por continente",
    x = "Ano",
    y = "% do PIB",
    color = "País"
  ) +
  theme_minimal()

ggplot(dados_top, aes(x = year, y = MS.MIL.XPND.ZS, color = country)) +
  geom_line(size = 1) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Gastos Militares como Percentual do PIB",
    subtitle = "5 maiores potências militares por continente (2000–2023)",
    x = "Ano",
    y = "Gasto Militar (% do PIB)",
    color = "País"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

# Criar um dicionário de tradução
regioes_traduzidas <- c(
  "East Asia & Pacific" = "Ásia Oriental e Pacífico",
  "Europe & Central Asia" = "Europa e Ásia Central",
  "Latin America & Caribbean" = "América Latina e Caribe",
  "Middle East & North Africa" = "Oriente Médio e Norte da África",
  "North America" = "América do Norte",
  "South Asia" = "Sul da Ásia",
  "Sub-Saharan Africa" = "África Subsaariana"
)

# Aplicar as traduções
dados_top$region <- regioes_traduzidas[dados_top$region]

ggplot(dados_top, aes(x = year, y = MS.MIL.XPND.ZS, color = country)) +
  geom_line(size = 1) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Gastos Militares como Percentual do PIB",
    subtitle = "5 maiores potências militares por continente (2000–2023)",
    x = "Ano",
    y = "Gasto Militar (% do PIB)",
    color = "País"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )

paises_traduzidos <- c(
  "United States" = "Estados Unidos",
  "China" = "China",
  "Russia" = "Rússia",
  "India" = "Índia",
  "Brazil" = "Brasil",
  "Germany" = "Alemanha",
  "France" = "França",
  "United Kingdom" = "Reino Unido",
  "Saudi Arabia" = "Arábia Saudita",
  "South Africa" = "África do Sul",
  "Turkey" = "Turquia",
  "Japan" = "Japão",
  "Israel" = "Israel",
  "Pakistan" = "Paquistão",
  "Mexico" = "México",
  "Canada" = "Canadá",
  "Australia" = "Austrália",
  "Iran" = "Irã",
  "Egypt" = "Egito",
  "South Korea" = "Coreia do Sul",
  "Indonesia" = "Indonésia",
  "Nigeria" = "Nigéria"
  # Adicione mais conforme necessário
)
# Aplicar a tradução de países
dados_top$country <- ifelse(
  dados_top$country %in% names(paises_traduzidos),
  paises_traduzidos[dados_top$country],
  dados_top$country
)
ggplot(dados_top, aes(x = year, y = MS.MIL.XPND.ZS, color = country)) +
  geom_line(size = 1) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Gastos Militares como Percentual do PIB",
    subtitle = "5 maiores potências militares por continente (2000–2023)",
    x = "Ano",
    y = "Gasto Militar (% do PIB)",
    color = "País"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10)
  )
install.packages("ggimage")      # necessário
install.packages("magick")       # necessário para imagens
