library(WDI)

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