rm(list = ls())

#pacotes necessários
if (!require("pacman")) install.packages("pacman")
pacman::p_load(sf, terra, spData)
library(dplyr)
library(ggplot2)
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)

#selecionando o arquivo do MapBiomas e definindo o estado no geobr
my_rast = rast(file.path("C:\\Users\\hguzz\\4o_Ex","brasil_coverage_2020.tif"))

municipio = read_municipality(year=2020)
rio_de_janeiro = mun %>% filter(abbrev_state == "RJ") 

#fazendo crop e mask 
cr = crop(my_rast, vect(rio_de_janeiro))
ms = mask(cr, vect(rio_de_janeiro))
ex = extract(ms, vect(rio_de_janeiro))

#definindo os valores de coberturas total e vegetal para cada município
cobertura_total <- ex %>%
  group_by(ID) %>%
  summarise(cobertura_t = n())

cobertura_vegetal <- ex %>%
  group_by(ID) %>%
  filter(brasil_coverage_2020 %in% c(1, 3, 4, 5, 49)) %>%
  summarise(cobertura_v = n())

#calculando a porcentagem de cobertura vegetal para cada município e combinando no dataframe original
cobertura <- cobertura_total %>%
    left_join(cobertura_vegetal, by = "ID") %>%
  mutate(pct_cobertura = cobertura_v/cobertura_t) %>%
  subset(select = c("ID", "pct_cobertura"))

rio_de_janeiro <- rio_de_janeiro %>%
  mutate(ID = 1:92) %>%
  left_join(cobertura, by = "ID")

#mapa
rio_de_janeiro %>%
  ggplot() +
  geom_sf(aes(fill=pct_cobertura), alpha = 0.8, col="white") +
  scale_fill_viridis_c(name = "Porcentagem", labels = scales::comma) + 
  labs(title = "Cobertura vegetal no Estado do Rio de Janeiro", subtitle = "Porcentagem por município", caption = "Fonte: MapBiomas, IPEA") +
  theme(plot.title = element_text(face = "bold"))

#qual a média da cobertura vegetal por distrito?
output <- rio_de_janeiro %>%
  select(c("ID", "pct_cobertura"))
output
