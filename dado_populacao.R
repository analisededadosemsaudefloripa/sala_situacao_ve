options(encoding = 'UTF-8')
library(readr)
library(ggplot2)
library(plotly)
populacacao <- read_csv("populacao/populacacao.csv")
populacacao <- populacacao[-c(12,13),]
names(populacacao) <- c("x", "y")
ggplot(populacacao, aes(x,y))+
                geom_point()+
                geom_line(data=data.frame(spline(populacacao, n=6)))+
                geom_text(aes(label=x),hjust=0, vjust=0)


a <- spline(populacacao, n=6)



devtools::install_github("tbrugz/ribge")
library(ribge)
pop2000 <- populacao_municipios(2000)
pop2001 <- populacao_municipios(2001)
pop2002 <- populacao_municipios(2002)
pop2003 <- populacao_municipios(2003)
pop2004 <- populacao_municipios(2004)
pop2005 <- populacao_municipios(2005)
pop2006 <- populacao_municipios(2006)
pop2007 <- populacao_municipios(2007)
pop2008 <- populacao_municipios(2008)
pop2009 <- populacao_municipios(2009)
pop2010 <- populacao_municipios(2010)
pop2011 <- populacao_municipios(2011)
pop2012 <- populacao_municipios(2012)
pop2013 <- populacao_municipios(2013)
pop2014 <- populacao_municipios(2014)
pop2015 <- populacao_municipios(2015)
pop2016 <- populacao_municipios(2016)
pop2017 <- populacao_municipios(2017)



munArea <- area_municipios()

library(lodown)
library(survey)
library(convey)
library(srvyr)
