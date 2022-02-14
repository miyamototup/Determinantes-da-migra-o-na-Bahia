library(RSQLite)
setwd("/home/bmiyamoto/Documentos/Pesquisa/Censos_Demograficos/Dados trabalhados/Censosql")
con=dbConnect(SQLite(),dbname='censo')

dbListTables(con)

dbListFields(con,'censo10')

ba=dbGetQuery(con,'select `V0002`,`V1006`,`V6264`,`V0208`,`V0207`,
              `V0211`,`V0627`,`V6910`,`V0650`,`V6531`,`V0635`,`V6471`,
              `V0210`,`V0601` from censo10 where `V0001`=29')

#Agora vamos salvar os dados da bahia em um arquivo um formato .RData
save.image('/home/bmiyamoto/Documentos/Pesquisa/Censo-Abel/Bahia/bdbahia.RData')

################################################################################
#################################################################################
#Carregar dados Bahia
load('/home/bmiyamoto/Documentos/Pesquisa/Censo-Abel/Bahia/bdbahia.RData')

#V0002 =  municipio
#V1006 = numero de domicilios no municipio
#V6264 = Migração (É residente no município naquele ano?)
#V0208 = Água (FOrma de abastecimento de água)
#V0207 = Esgoto (Tipo de esgoto sanitáio)
#V0210 = Lixo (Destino do lixo)
#V0211 = Eletricidade (Existência de energia elétrica)
#V0627 = PArticipitação de pessoas com analfabetas no munípio
#V6910 = Percentual de pessoas em idade ativa desempregados no município
#V0650 = Participiação de pessoas ocupadas no município que contribuem com a previdência
#V6529 = Renda per capita do domicílio
#V0635 = número de pessoas no município com curso superior
#V6471 = Setor de atividade em que está ocupado 
#V0601 = Total da população

#Criar variáveis

#Participtação de domicilios urbanos sobre o total de domicílios dos
#municípios. (V1006= 1 é urbano e 2 é rural)

#Separando domicílios urbanos dos municípios
urb=ba[ba$V1006==1,c(1,2)]
urb$um=1
urb=aggregate(urb[,c(3)],by=list(mun=urb$V0002),sum,na.rm=T)
colnames(urb)[2]='urbano'

#Separando domíclios rurais dos municípios
rur=ba[ba$V1006==2,c(1,2)]
rur$um=1
rur=aggregate(rur[,c(3)],by=list(mun=rur$V0002),sum,na.rm=T)
colnames(rur)[2]='rural'

dom=merge(urb,rur,by=c('mun'),all.x=T,all.y=T)
dom$rural[is.na(dom$rural)]=0
dom$total=dom$urbano+dom$rural
dom$pdomurb=dom$urbano/dom$total
dom=dom[,c(1,5)]

ba=merge(ba,dom,by.x=c('V0002'),by.y=c('mun'))



####Criando variável migração - V6264 -  Município de residência em 31 de Julho de 2005
#8888888 ignorado
#9999999 

#Vamos comparar o municiípio atual do indivíduo com o município anterior
#Converte as variáveis codigo do municipio de origem e codigo do municipio atual em numéricas
ba$V6264=as.numeric(ba$V6264)
ba$V0002=as.numeric(ba$V0002)

#Se a variável de municipio de residencia a cinco anos atrás tiver valor 8888888
#ou 9999999 ela receve NA
ba$V6264[ba$V6264==8888888|ba$V6264==9999999]=NA

#Cria-se variável variável migra com valor igual a 0
ba$migra=0
#Migra recebe um se e somente se o codigo do municipio de origem for diferente do codigo do
#município atual e se o código do município de origem for diferente de NA
ba$migra[ba$V0002!=ba$V6264&!is.na(ba$V6264)]=1
#Se migrou migra recebe 1 se não recebe 0

#Participação percentual de domicilios por municipio em que o entrevista respondeu que migrou
migrante=ba[,c(1,16)]
#Total de domicílios do município
totdom=as.data.frame(table(migrante$V0002))
migrante=aggregate(migrante[,c(2)],by=list(mun=migrante$V0002),sum, na.rm=T)
migrante=merge(migrante,totdom,by.x=c('mun'),by.y=c('Var1'))
colnames(migrante)[2:3]=c('pmig','totaldom')
migrante$pmigdom=migrante$pmig/migrante$totaldom
migrante=migrante[,c(1,4)]
ba=merge(ba,migrante,by.x=c('V0002'),by.y=c('mun'))

######################################################
#Vamos utilizar a variável = V0208: FORMA DE ABASTECIMENTO DE ÁGUA
# Precisamos da participação dos domicílios atendidos pela
#rede geral de abastecimento: condição igual a 01 do censo 2010.
#"FORMA DE ABASTECIMENTO DE ÁGUA:
#01- Rede geral de distribuição
#02- Poço ou nascente na propriedade
#03- Poço ou nascente fora da propriedade
#04- Carro-pipa
#05- Água da chuva armazenada em cisterna
#06- Água da chuva armazenada de outra forma
#07- Rios, açudes, lagos e igarapés
#08- Outra 
#09- Poço ou nascente na aldeia
#10- Poço ou nascente fora da aldeia Branco"
######################################################

#Cria a tabela água que recebe as colunas 1 e 4 da tabela ba
agua=ba[,c(1,4)]

#Total de domicílios do município
totdom=as.data.frame(table(agua$V0002))

#A coluna ab1 da tabela agua recebe 1 caso o domicilio seja abastecido por rede gertal de água,
#ou seja caso a coluna V0208 seja igual a 1
agua$ab1[agua$V0208==1]=1

#SOma todos os valores 1 da coluna ab1 da tabela água, ou seja, todos domicilios por município que
#são abastecidos pela rede geral de água
agua=aggregate(agua[,c(3)],by=list(mun=agua$V0002),sum,na.rm=T)

#Funde-se a tabela água com a tabela totdom pelo código do município, que é mun na tabela água
# e Var1 na tabela totdom
agua=merge(agua,totdom,by.x=c('mun'),by.y=c('Var1'))
#Altera-se o nome das colounas 2 e 3 da tabela água
colnames(agua)[2:3]=c('ab1','totaldom')
#Cria-se a coluna pdomauga na tabela agua. Divisão de ab1 por totaldom
agua$pdomagua=agua$ab1/agua$totaldom
#Seleciona-se apenas as colunas 1 e 4 da tabela água, ou seja, codigo do municipio e participação de 
#AB1 no total de domicilios do municpio
agua=agua[,c(1,4)]
#Incorpora os resultados da tabela água na tabela ba, fundindo-se pelo código do município
ba=merge(ba,agua,by.x=c('V0002'),by.y=c('mun'))

############################################################
#V0207 "TIPO DE ESGOTAMENTO SANITÁRIO:
#1- Rede geral de esgoto ou pluvial
#2- Fossa séptica
#3- Fossa rudimentar
#4- Vala
#5- Rio, lago ou mar
#6- Outro 
#Branco" 
###########################################################
esgoto=ba[,c(1,5)]
#Total de domicílios do município
totdom=as.data.frame(table(esgoto$V0002))
esgoto$e123[esgoto$V0207==1|esgoto$V0207==2|esgoto$V0207==3]=1
esgoto=aggregate(esgoto[,c(3)],by=list(mun=esgoto$V0002),sum, na.rm=T)
esgoto=merge(esgoto,totdom,by.x=c('mun'),by.y=c('Var1'))
colnames(esgoto)[2:3]=c('e123','totaldom')
esgoto$pdomesgoto=esgoto$e123/esgoto$totaldom
esgoto=esgoto[,c(1,4)]
ba=merge(ba,esgoto,by.x=c('V0002'),by.y=c('mun'))


############################################################
#V0210 "DESTINO DO LIXO:
#1- Coletado diretamente por serviço de limpeza
#2- Colocado em caçamba de serviço de limpeza
#3- Queimado (na propriedade)
#4- Enterrado (na propriedade)
#5- Jogado em terreno baldio ou logradouro
#6- Jogado em rio, lago ou mar
#7- Tem outro destino
#Branco"
###########################################################
lixo=ba[,c(1,13)]
#Total de domicílios do município
totdom=as.data.frame(table(lixo$V0002))
lixo$l12[lixo$V0210==1|lixo$V0210==2]=1
lixo=aggregate(lixo[,c(3)],by=list(mun=lixo$V0002),sum,na.rm=T)
lixo=merge(lixo,totdom,by.x=c('mun'),by.y=c('Var1'))
colnames(lixo)[2:3]=c('l12','totaldom')
lixo$pdomlixo=lixo$l12/lixo$totaldom
lixo=lixo[,c(1,4)]
ba=merge(ba,lixo,by.x=c('V0002'),by.y=('mun'))


#########################################################
#V0211 "EXISTÊNCIA DE ENERGIA ELÉTRICA:
#1- Sim, de companhia distribuidora
#2- Sim, de outras fontes
#3- Não existe energia elétrica
#Branco" 
########################################################
energia=ba[,c(1,6)]
#Total de domicílios do município
totdom=as.data.frame(table(energia$V0002))
energia$en12[energia$V0211==1|energia$V0211==2]=1
energia=aggregate(energia[,c(3)],by=list(mun=energia$V0002),sum,na.rm=T)
energia=merge(energia,totdom,by.x=c('mun'),by.y=c('Var1'))
colnames(energia)[2:3]=c('en12','totaldom')
energia$pdomenergia=energia$en12/energia$totaldom
energia=energia[,c(1,4)]
ba=merge(ba,energia,by.x=c('V0002'),by.y=('mun'))


#V0627	SABE LER E ESCREVER:
#1- Sim
#2- Não
#Branco

#Cria-se variável variável migra com valor igual a 0
ba$pdomanalf=0
ba$pdomanalf[ba$V0627==2]=1

analfabetos=ba[,c(1,22)]
#Total de domicílios do município
totdom=as.data.frame(table(analfabetos$V0002))
analfabetos=aggregate(analfabetos[,c(2)],by=list(mun=analfabetos$V0002),sum,na.rm=T)
analfabetos=merge(analfabetos,totdom,by.x=c('mun'),by.y=c('Var1'))
colnames(analfabetos)[2:3]=c('analf','totaldom')
analfabetos$panalfdom=analfabetos$analf/analfabetos$totaldom
analfabetos=analfabetos[,c(1,4)]
ba=merge(ba,analfabetos,by.x=c('V0002'),by.y=('mun'))




#V6910	CONDIÇÃO DE OCUPAÇÃO NA SEMANA DE REFERÊNCIA
#1- Ocupadas
#2- Desocupadas
#Branco

desocupados=ba[,c(1,8)]
#Total de domicílios do município
totdom=as.data.frame(table(desocupados$V0002))
desocupados$des[desocupados$V6910==2]=1
desocupados=aggregate(desocupados[,c(3)],by=list(mun=desocupados$V0002),sum,na.rm=T)
desocupados=merge(desocupados,totdom,by.x=c('mun'),by.y=c('Var1'))
colnames(desocupados)[2:3]=c('des','totaldom')
desocupados$pdomdesocupados=desocupados$des/desocupados$totaldom
desocupados=desocupados[,c(1,4)]
ba=merge(ba,desocupados,by.x=c('V0002'),by.y=('mun'))



#Criar a tabela previdência. Ela recebe as colunas 1 e 8 da tabela ba.
#Coluna 1 = código do município; coluna 2 = variável V6910. Ou seja:
#condição de ocupação na semana de referência da pesquisa.
previdencia=ba[,c(1,9)]

#Aqui é feito o cálculo do total de domicílios por municípios no estado da Bahia.
totdom=as.data.frame(table(previdencia$V0002))

#Aqui, a coluna 2 da tabela previdência será igual a 1, caso a pessoa que
#resppndeu o questionário no domicílio estaja assegurado por algum tipo 
#de previdência na semana de referência da pesquisa.
previdencia$prev[previdencia$V0650==1| previdencia$V0650==2]=1

#Somam-se todos os valores 1 da coluna 1 e 2 da tabela previdência. Ou seja,
#todos os domicílios por municípios em que a pessoa que respondeu ao 
#questinário seja assegurado por:#1- Sim, no trabalho principal
#2- Sim, em outro trabalho #na semana de referência da pesquisa.
previdencia=aggregate(previdencia[,c(3)],by=list(mun=previdencia$V0002),sum,na.rm=T)

#Funde-se a tabela desocupados com a tabela totdom pelo código do município
#que é "mum" na tabela desocupados e "Var1" na tabela totdom.
previdencia=merge(previdencia,totdom,by.x=c("mun"), by.y=c("Var1"))

#Altera-se os nomes das colunas 2 e 3 ta tabela desocupados.
colnames(previdencia)[2:3]=c("prev", "totaldom") 

#Criando a coluna pdomprevidencia na tabela previdencia. 
#Divisãoo "des" por totdom.
previdencia$pdomprev=previdencia$prev/previdencia$totaldom

#seleciona-se apenas as colunas 1 e 4 da tabela desocupados. Ou seja, codido 
#do munucípio e participação "des" no total de domicílios no município
previdencia=previdencia[,c(1,4)]

#Incorpora-se os resultados da tabela previdencia na tabela ba fundindo-se pelo
#código do munuicípio.
ba=merge(ba,previdencia,by.x=c("V0002"), by.y=c("mun"))



#######################################################################
#V0635 "ESPÉCIE DO CURSO MAIS ELEVADO CONCLUÍDO:
#1- Superior de graduação
#2- Mestrado
#3- Doutorado
#Branco" 
######################################################################
#Criar a tabela previdência. Ela recebe as colunas 1 e 8 da tabela ba.
#Coluna 1 = código do município; coluna 2 = variável V6910. Ou seja:
#condição de ocupação na semana de referência da pesquisa.
superior=ba[,c(1,11)]

#Aqui é feito o cálculo do total de domicílios por municípios no estado da Bahia.
totdom=as.data.frame(table(superior$V0002))

#Aqui, a coluna 2 da tabela previdência será igual a 1, caso a pessoa que
#resppndeu o questionário no domicílio estaja assegurado por algum tipo 
#de previdência na semana de referência da pesquisa.
superior$sup[superior$V0635==1|superior$V0635==2|superior$V0635==3]=1

#Somam-se todos os valores 1 da coluna 1 e 2 da tabela previdência. Ou seja,
#todos os domicílios por municípios em que a pessoa que respondeu ao 
#questinário seja assegurado por:#1- Sim, no trabalho principal
#2- Sim, em outro trabalho #na semana de referência da pesquisa.
superior=aggregate(superior[,c(3)],by=list(mun=superior$V0002),sum,na.rm=T)

#Funde-se a tabela desocupados com a tabela totdom pelo código do município
#que é "mum" na tabela desocupados e "Var1" na tabela totdom.
superior=merge(superior,totdom,by.x=c("mun"), by.y=c("Var1"))

#Altera-se os nomes das colunas 2 e 3 ta tabela desocupados.
colnames(superior)[2:3]=c("sup", "totaldom") 

#Criando a coluna pdomprevidencia na tabela previdencia. 
#Divisãoo "des" por totdom.
superior$pdomsup=superior$sup/superior$totaldom

#seleciona-se apenas as colunas 1 e 4 da tabela desocupados. Ou seja, codido 
#do munucípio e participação "des" no total de domicílios no município
superior=superior[,c(1,4)]

#Incorpora-se os resultados da tabela previdencia na tabela ba fundindo-se pelo
#código do munuicípio.
ba=merge(ba,superior,by.x=c("V0002"), by.y=c("mun"))


#V6529	RENDIMENTO DOMICILIAR (DOMICÍLIO PARTICULAR) EM JULHO DE 2010  
#Vamos construir a renda média percapita do município
rendaper=ba[,c(1,10)]
rendaper$V6531=as.numeric(rendaper$V6531)
rendaper=aggregate(rendaper[,c(2)],by=list(mun=rendaper$V0002),mean,na.rm=T)
colnames(rendaper)[2]=c('rendapermun')

#Dividir por indivíduo e trazer os valores para 2016 ou 2015 corrigindo pela inflação
#Tratamento do IPCA
ipca=read.csv("/home/bmiyamoto/Documentos/Pesquisa/Censo-Abel/IPCA/IPCA.csv",header=T)
ipca=ipca[c(26:36),c(1,2)]
colnames(ipca)[2]="IPCA"

ipca$V1=((ipca$IPCA/100)+1)

#Valores reais de 2005. Divido o valor monetário por esse número
ipca$V2[1]=1
ipca$V2[2]=ipca$V1[2]
for (i in 3:length(ipca$V1)){
  ipca$V2[i]=(ipca$V2[i-1])*(ipca$V1[i])
}


#Valores reais de 2016. Multiplico o valor monetário po esse número
ipca[12,]=NA
ipca$V3[12]=1
ipca$V3[11]=ipca$V1[11]
for(i in (length(ipca$V1)-2):1){
  ipca$V3[i]=(ipca$V3[i+1])*(ipca$V1[i])
}

ipca=ipca[,c(1,5)]

rendaper$rendapermun=rendaper$rendapermun*1.488993
ba=merge(ba,rendaper,by.x=c('V0002'),by.y=c('mun'))


###############################################################################
#V6471 "ATIVIDADE – código
#(pode ter valor branco)
#- A relação de códigos encontra-se  na pasta  ANEXOS AUXILIARES"
###############################################################################
#sentor de atividade do ocupado do domícilio
setor=ba[,c(1,12)]
setor$V6471=as.numeric(setor$V6471)
setor$V6471[setor$V6471==0|setor$V6471==99000]=NA

#1101 ao 3002=Agropecuária (1)
#5000 ao 39000=Indústria (2)
#41000 ao 43999=Construção (3)
#45010 ao 48999= Comércio (4)
#49010 ao 82009=Serviços (5)
#84011 ao 84999= Administração Pública (6)
#85011 ao 96090= Serviços de educação, saúde, cultura e outros serviços (7)
#97000 = serviços domésticos (8)
#99000=NA

setor$setor[setor$V6471>=1101&setor$V6471<=3002]=1
setor$setor[setor$V6471>=5000&setor$V6471<=39000]=2
setor$setor[setor$V6471>=41000&setor$V6471<=43999]=3
setor$setor[setor$V6471>=45010&setor$V6471<=48999]=4
setor$setor[setor$V6471>=49010&setor$V6471<=82009]=5
setor$setor[setor$V6471>=84011&setor$V6471<=84999]=6
setor$setor[setor$V6471>=85011&setor$V6471<=96090]=7
setor$setor[setor$V6471==97000]=8


setor$ICS[setor$setor==2|setor$setor==4|setor$setor==5]=1
totdom=as.data.frame(table(setor$V0002))
setor=aggregate(setor[,c(4)],by=list(mun=setor$V0002),sum,na.rm=T)
setor=merge(setor,totdom,by.x=c('mun'),by.y=c('Var1'))
colnames(setor)[2:3]=c('setor','totdom')
setor$psetICS=setor$setor/setor$totdom
setor=setor[,c(1,4)]
ba=merge(ba,setor,by.x=c('V0002'),by.y=c('mun'))

rm(list= ls()[!(ls() %in% c('ba'))])

#Agora vamos salvar os dados da bahia em um arquivo um formato .RData
save.image('/home/bmiyamoto/Documentos/Pesquisa/Censo-Abel/Bahia/bdbahiaII.RData')

################################################################################
#################################################################################
##################################################################################
#Marco 1
#Carregar dados Bahia com variáveis já criadas
load('/home/bmiyamoto/Documentos/Pesquisa/Censo-Abel/Bahia/bdbahiaII.RData')
ba=ba[,c(1,15,17:21,23:28)]
ba=unique(ba)
bacluster=ba[,c(2:13)]

#CLusterização hierarquica
library(fastcluster)
hcluster=hclust.vector(bacluster,method="ward")
hclusterCut <- cutree(hcluster, 5)
hclusterCut=as.data.frame(hclusterCut)

#estatísticas descritivas de cada um dos cinco clusters criados por clusterização hierarquica
summary(ba[ba$hclusterCut==1,])
summary(ba[ba$hclusterCut==2,])
summary(ba[ba$hclusterCut==3,])
summary(ba[ba$hclusterCut==4,])
summary(ba[ba$hclusterCut==5,])


#Clusterização por kmeans 
kmeansba=kmeans(bacluster,centers=5)
kmeansbacluster=as.data.frame(kmeansba$cluster)
colnames(kmeansbacluster)=c('kmeans')

#estatísticas descriticas de cada um dos clusters criados por kmeans
kmeansba$centers


#Incluir os clusters hierarquicos e por kmeans na tabela ba
ba=cbind(ba,hclusterCut,kmeansbacluster)



#Mapa para clusterização hierarquica
library(maptools)
bahia=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Censo-Abel/Shapefiles/municipios_2010/municipios_2010.shp')
bahia=bahia[bahia$uf=='BA',]
bahia@data$ordem=1:dim(bahia@data)[1]
bahia@data=merge(bahia@data,ba,by.x=c('codigo_ibg'),by.y=c('V0002'))
bahia=bahia[order(bahia@data$ordem),]
bahia@data$cor[bahia$hclusterCut==1]='darkgreen'
bahia@data$cor[bahia$hclusterCut==2]='darkorange'
bahia@data$cor[bahia$hclusterCut==3]='firebrick1'
bahia@data$cor[bahia$hclusterCut==4]='gold2'
bahia@data$cor[bahia$hclusterCut==5]='navy'
plot(bahia, border=F,lwd=.1, axes=F,las=1,col=bahia@data$cor)


#Mapa para clusterização kmeans
library(maptools)
bahia=readShapePoly('/home/bmiyamoto/Documentos/Pesquisa/Censo-Abel/Shapefiles/municipios_2010/municipios_2010.shp')
bahia=bahia[bahia$uf=='BA',]
bahia@data$ordem=1:dim(bahia@data)[1]
bahia@data=merge(bahia@data,ba,by.x=c('codigo_ibg'),by.y=c('V0002'))
bahia=bahia[order(bahia@data$ordem),]
bahia@data$cor2[bahia$kmeans==1]='darkgreen'
bahia@data$cor2[bahia$kmeans==2]='darkorange'
bahia@data$cor2[bahia$kmeans==3]='firebrick1'
bahia@data$cor2[bahia$kmeans==4]='gold2'
bahia@data$cor2[bahia$kmeans==5]='navy'
plot(bahia, border=F,lwd=.1, axes=F,las=1,col=bahia@data$cor)
