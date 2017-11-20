library(readr)
chicago <- read_csv("C:/Users/ra164912/Desktop/Chicago_Crimes_2012_to_2017.csv")

findNA <-  which(is.na(chicago$Location)) #achando os valores NA
chicago <- chicago[-findNA,]                #tirando do banco
chicago <- chicago[ , c(4, 7, 9, 19, 21, 22)] #pegando as colunas de interesse
chicago <- separate(chicago, Date, c("Date", "Hour", "MeioDia"), sep = " ")#separando coluna
chicago <- chicago[ ,-c(2, 3)] #elimino as que naum preciso
names(chicago)[c(2)] <- c("tipoCrime")
#Tira o dia
banco <- separate(banco, Date, c("Mes", "Dia", "Ano"), sep = "/")
banco <- banco[ ,-2] 

#Altera o nome das colunas
names(banco)[3] <- c("Tipo de Crime")

library(dplyr)
# Banco com as 2 primeiras colunas e quantas vezes cada crime ocorre por mes
dados <- banco %>%
  group_by(`Tipo de Crime`, Ano, Mes) %>%
  summarize(Quantidade = n())
#Cada crime é uma variável
dados <- spread(dados, `Tipo de Crime`, Quantidade)

#NA's <- 0 (para ter registro de todos os crimes em cada mês)
for (i in 3:(ncol(dados))){
  for (j in 1:nrow(dados)){
    if (is.na(dados[j, i])){
      dados[j, i] <- 0
    }
  }
}
remove(i, j)

#Cada crime num mês é uma observação
dados <- gather(dados, `Tipo de Crime`, Quantidade, -c(Ano, Mes))

library(ggplot2)
library(stringr)
library(rebus)
library(lubridate)

# 1ª letra maiúscula e as demais minúsculas
maiuscula <- function(x){
  y <- str_sub(x, 2, -1)
  z <- str_replace(x, y, tolower(y))
  return(z)
}

# Gráfico para ver comportamento do crime ao longo do tempo
comport <- function(crime = "", quando = NULL){
  
  # Padronizando a data (tirando o dia e deixando formato mm/yyyy)
  MesAno <- str_extract(quando, pattern = START %R% one_or_more(DGT) %R% 
                          or("/", "-", "_", "\\", "|", " ") %R% 
                          one_or_more(DGT))
  Anos <- str_extract(MesAno, pattern = START %R% one_or_more(DGT))
  
  Meses <- str_extract(MesAno, pattern = one_or_more(DGT) %R% END)
  
  MesAno <- c(paste(Meses[1], Anos[1], sep = "/"), paste(Meses[2], Anos[2], sep = "/"))
  
  # Garantindo período válido
  if (!(as.integer(Meses[1]) %in% c(0:12) | as.integer(Meses[1]) %in% c(0:12))){
    stop("Esse mês não existe")
  }
  if (!(as.integer(Meses[2]) %in% c(0:12) | as.integer(Meses[2]) %in% c(0:12))){
    stop("Esse mês não existe")
  }
  if ((Anos[2] > "2017") | ((Anos[2] == "2017") & (Meses[2] > "01"))){
    stop("Os dados vão até 01/2017.")
  }
  if (Anos[1] < "2012"){
    stop("Os dados começam em 01/2012.")
  }  
  if ((Anos[2] < Anos[1]) | ((Anos[2] == Anos[1]) & (Meses[2] < Meses[1]))){
    stop("A data inicial não pode ser depois da data final.")
  }
  
  
  # Define novo banco:
  
  # filtrado pelo tipo de crime
  minhatbl <- filter(dados, `Tipo de Crime` %in% crime) %>%
    arrange(Ano, Mes)
  
  # filtrado pelo período
  inicio <- min(which(minhatbl$Ano == Anos[1] & minhatbl$Mes == Meses[1]))
  fim <- max(which(minhatbl$Ano == Anos[2] & minhatbl$Mes == Meses[2]))
  
  minhatbl <- minhatbl[inicio:fim, ]
  
  # Nome do gráfico
  crime <- vapply(crime, maiuscula, character(1), USE.NAMES = FALSE)
  
  if(length(crime) == 1){
    titulo <- paste("Nº de", crime[1], "por mês") 
  } else if(length(crime) == 2){
    titulo <- paste("Nº de", crime[1], "and", crime[2], "por mês") 
  } else if (length(crime) > 2){
    titulo <- paste("Nº de", crime[1]) 
    for (i in 2:(length(crime)-1)){
      if (i < (length(crime)-1)){
        titulo <- paste(titulo, crime[i], sep = ", ") #Põe vírgula até o antepenúltimo
      } else if (i == (length(crime)-1)){
        titulo <- paste0(titulo, ", ", crime[i], " and ", crime[i+1], " por mês") #Põe o penúltimo "and" ultimo
      }
    }
  }
  
  graf <- ggplot(minhatbl, aes(x = Mes, y = Quantidade, group = `Tipo de Crime`, col = `Tipo de Crime`)) +
    geom_point() + facet_grid(.~Ano) +
    geom_line() + ggtitle(titulo, paste("De", MesAno[1],  "a", MesAno[2])) +
    scale_x_discrete(name = "Mês", labels = c("1", "", "3", "", "5", "", "7", "", "9", "", "11", ""))
  
  return(graf)
}

#Exemplo:
crime <- c("BATTERY","NARCOTICS")
quando <- c("2012-12-14", "2017-01-15") #com ou sem dia #separador é "/" ou "-", etc.
comport(crime, quando)
