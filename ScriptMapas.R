#DESCRIÇÃO DADOS UTILIZADOS: 
#Pesquisa visando identificar a densidade de automóveis
#no Estado de São Paulo em 2021, utilizando dados coletados pelo Denatran

#links:
#https://www.gov.br/infraestrutura/pt-br/assuntos/transito/conteudo-Senatran/frota-de-veiculos-2021
#https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=acesso-ao-produto

#install.packages("ggplot2")
#install.packages("rgdal") 
#install.packages("rgeos")
#install.packages("gpclib")
#install.packages("maptools")
#install.packages("RColorBrewer",dependencies = T)

library(rgeos)
library(ggplot2)
library(rgdal)
library(gpclib)
library(maptools)
library(RColorBrewer)

#Ler arquivo
SP <- readOGR(, "SP_Municipios_2021")
Carros_BR <- readxl::read_xlsx(file.choose())
#ler arquivo Shapefile
mapa_shapefile <- rgdal::readOGR(file.choose())
head(mapa_shapefile)

head(Carros_BR)
head(SP@data)

#configurar dados para uso
NM_MUN <- SP@data$NM_MUN
SP_Carros = subset(Carros_BR, UF=="SAO PAULO")
SP_Carros = SP_Carros$`Qtd. Veículos`[order(SP@data$CD_MUN)]
SP$CD_MUN <- substr(SP$CD_MUN,1,6)
CD_MUN <- SP@data$CD_MUN

#unir as tabelas
SP_Carros = data.frame(NM_MUN,CD_MUN, SP_Carros)+head(SP_Carros)


#criar malha 'x' para trabalhar com dados
x <- fortify(SP, region = "CD_MUN")+head(x)
x <- merge(x, SP@data, by.x = "id", by.y = "CD_MUN")+head(x)

#limpar colunas indesejadas e adcionar qtd. veiculos
x <- x[,-c(9,10)]+head(x)
x <- merge(x, SP_Carros, by.x="id", by.y="CD_MUN")
x <- x[,-c(9)]+head(x)

#separar qtd. veiculos por categorias
x$SP_Carros = cut(x$SP_Carros, breaks = c(0,3125,6250,12500,25000,50000,100000,200000),
                  labels = c('0-3125',
                             '3125-6250',
                             '6250-23500',
                             '25000-50000',
                             '50000-100000',
                             '100000-150000',
                             '+150000'),
                  include.lowest = T)

#criando mapa através do ggplot
mapa_ggplot <- ggplot(SP, aes(x$long, x$lat, group=x$group, fill=x$SP_Carros))+
  geom_polygon(color = "red")+
  coord_equal()+
  labs(x="longitude", y="latitude", fill=x$SP_Carros)

mapa_ggplot

#customizando mapa
mapa_ggplot + ggtitle("Carros") +
  scale_fill_manual(values = brewer.pal(9,'Greens')[4:9]) +
  theme(plot.title = element_text(size = rel(1), lineheight = 0.9, face = "bold",
                                  colour = 'blue'))
