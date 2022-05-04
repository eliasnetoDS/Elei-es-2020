'ROTINA: ANÁLISE DAS ELEIÇÕES 2020
CRIADOR: ELIAS NETO
DATA: 27-12-2020'


#IMPORTAÇÃO DOS DADOS

library(readr)
library(xlsx)



consulta_cand_2020_BRASIL <- read_delim("~/Mamirauá/Rotinas R/Análise Eleições/consulta_cand_2020_BRASIL.csv", 
                                        ";", escape_double = FALSE, trim_ws = TRUE)
View(consulta_cand_2020_BRASIL)

dados <- consulta_cand_2020_BRASIL #alterando o nome do dataset para "dados"

# EXCLUSÃO DE COLUNAS DESNECESSÁRIAS

names(dados)

head(dados)

validados <- dados [ ,c("SG_UF", "SG_PARTIDO", "NR_IDADE_DATA_POSSE", "DS_CARGO",
                       "DS_GENERO", "DS_GRAU_INSTRUCAO", "DS_ESTADO_CIVIL",
                       "DS_COR_RACA", "CD_CARGO", "NM_URNA_CANDIDATO",
                       "DS_OCUPACAO", "DS_SIT_TOT_TURNO")
                    ]

table(dados$DS_SIT_TOT_TURNO)

table(dados$DT_ELEICAO)

eleitos <- validados[validados$DS_SIT_TOT_TURNO == "ELEITO"|validados$DS_SIT_TOT_TURNO == "ELEITO POR QP"|validados$DS_SIT_TOT_TURNO == "ELEITO_POR_MEDIA",]


resumo <- data.frame(table(eleitos$SG_PARTIDO, eleitos$DS_CARGO))


resumo <- reshape(data = resumo, idvar = "Var1", timevar = "Var2", direction = "wide")

names(resumo) <- c("PARTIDO","PREFEITO","VEREADOR","VICE-PREFEITO")


write.table (resumo, file = "resumo.csv", sep= ",")






'ANÁLISE SOBRE OS NOMES'

library(stringr)

removeacentos <- function(x) {
  xx = iconv(x, to="ASCII//TRANSLIT") 
  xx = gsub("[~|^|~|\"|'|`]","",xx)
  return(xx)
}

#Candidatos
nome <- data.frame(table(dados$NM_URNA_CANDIDATO))
nome <- nome[order(nome$Freq, decreasing = T),]

nome$Var1 <- removeacentos(nome$Var1)

write.table (nome, file = "nome.csv", sep= ",")



#Eleitos

nome_eleitos <- data.frame(table(eleitos$NM_URNA_CANDIDATO))
nome_eleitos <- nome_eleitos[order(nome_eleitos$Freq, decreasing = T),]

nome_eleitos$Var1 <- removeacentos(nome_eleitos$Var1)

write.table (nome_eleitos, file = "nome eleitos.csv", sep= ",")


#bolsonaro
str_detect(nome$Var1, "bolsonaro")

x <- agrep(pattern="bolsonaro", validados$NM_URNA_CANDIDATO , ignore.case = TRUE, 
           value = TRUE, fixed = TRUE)  
x <- data.frame(x)

names(x) <- "NM_URNA_CANDIDATO"

bolsonaro <- merge(x, validados, by = c("NM_URNA_CANDIDATO"), all = F)



bolsonaro$NM_URNA_CANDIDATO <- removeacentos(bolsonaro$NM_URNA_CANDIDATO)
bolsonaro$DS_SIT_TOT_TURNO<- removeacentos(bolsonaro$DS_SIT_TOT_TURNO)

write.table (bolsonaro, file = "nome bolsonaro.csv", sep= ",")
