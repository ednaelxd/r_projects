#dados_lucas222<-as.matrix(read.table("clipboard",dec=,))
#pesos_lucas222<-as.matrix(read.table("clipboard"))
#classes_lucas222<-as.matrix(read.table("clipboard"))
#cabecalho<-read.table("clipboard")
#preferencias222<-as.matrix(read.table("clipboard"))
#indiferencas222<-as.matrix(read.table("clipboard"))
preferencias<-as.matrix(read.table("clipboard"))

options(max.print=10000)
class_nominal<-function(x,w,b,coef_preferencia,coef_indiferencia){
  #x=matriz de dados
  #w=matriz de pesos
  #b=matriz de classes
  l<-1
  c<-1
  u<-1
  
  
 linhas_classes<-length(b[,1])#salva numero de linhas da matriz das classes
 linhas_dados<-length(x[,1])#salva numero de linhas da matriz de dados
 colunas_dados<-length(x[1,])
 colunas_classes<-length(b[1,])
  
  if(colnames(x)!=colnames(w) || colnames(x)!=colnames(b) || colnames(w)!=colnames(b)){
    print("Número de colunas das entradas deve ser igual")
    return(0)
  }
 #if(colnames(x)!=colnames(coef_preferencia) || colnames(coef_preferencia)!=colnames(b)
 #   || colnames(w)!=colnames(coef_preferencia) ||
 #   colnames(x)!=colnames(coef_indiferencia) || colnames(coef_indiferencia)!=colnames(b)
 #   || colnames(w)!=colnames(coef_indiferencia)){
 #  print("Número de colunas das entradas deve ser igual")
 #  return(0)
 #}
  
######Fluxo positivo#######
  matriz_de_fluxos<-matrix(1,linhas_dados*linhas_classes,colunas_classes)  #criação da matriz de fluxos
######Fluxo Negativo#######
  matriz_de_fluxosN<-matrix(1,linhas_dados*linhas_classes,colunas_classes)
######Fluxo Liquido#######
 fluxo_liquido<-matrix(1,linhas_dados*linhas_classes,1)
 nnn<-matrix(1,linhas_dados*linhas_classes,1)
 nnn[,1]<-c(1:4)
 rownames(fluxo_liquido)<-nnn

#Calculos dos fluxos POSITIVOS
 
 for(i in 1:linhas_dados){  
   
   for(l in 1:linhas_classes){
     
      for(c in 1:colunas_classes){
   
      if((x[i,c]-b[l,c])>coef_preferencia[,c]){
        matriz_de_fluxos[u,c]<-1
        }
      if((x[i,c]-b[l,c])<=coef_indiferencia[,c]){
        matriz_de_fluxos[u,c]<-0
      }
      if((x[i,c]-b[l,c])<coef_preferencia[,c] && (x[i,c]-b[l,c])>coef_indiferencia[,c]){
matriz_de_fluxos[u,c]<-(x[i,c]-b[l,c]-coef_indiferencia[,c])/(coef_preferencia[,c]-coef_indiferencia[,c])
          }
      }
     u=u+1
  }
}
u=1  
#Calculo dos fluxos NEGATIVOS 
 
 for(i in 1:linhas_dados){  
   
   for(l in 1:linhas_classes){
     
     for(c in 1:colunas_classes){
       
       if((b[l,c]-x[i,c])>coef_preferencia[,c]){
         matriz_de_fluxosN[u,c]<-1
       }
       if((b[l,c]-x[i,c])<=coef_indiferencia[,c]){
         matriz_de_fluxosN[u,c]<-0
       }
       if((b[l,c]-x[i,c])<coef_preferencia[,c] && (b[l,c]-x[i,c])>coef_indiferencia[,c]){
         matriz_de_fluxosN[u,c]<-(x[i,c]-b[l,c]-coef_indiferencia[,c])/(coef_preferencia[,c]-coef_indiferencia[,c])
       }
     }
     u=u+1
   }
 }
#print(matriz_de_fluxos)
#print(matriz_de_fluxosN)
##Fluxo Líquido
  y=1
  for(i in 1:(linhas_dados*linhas_classes)){
      fluxo_liquido[i,]<-sum(matriz_de_fluxos[i,]*w[y,])-sum(matriz_de_fluxosN[i,]*w[y,])
      y=y+1
      if(y>length(w[,1])){y=1}
  }

#utilizar operador de módulo
a<-matrix(1,linhas_dados,linhas_classes)
maiores_fluxos<-matrix(1,linhas_dados,1)
cont<-1
  for(i in 1:linhas_dados){
      maiores_fluxos[i,]<-max(fluxo_liquido[cont:(i*linhas_classes)])
      a[i,]<-fluxo_liquido[cont:(i*linhas_classes)]
      cont=cont+linhas_classes
  }

    
  #print(fluxo_liquido)
  cat("Coeficientes usados no problema:","Coeficiente de Preferencia\n",sep="\n\n")
  print(coef_preferencia)
  cat("Coeficiente de Indiferenca\n\n")
  print(coef_indiferencia)
  #colnames(maiores_fluxos)<-"Fluxos Finais"
  #return(maiores_fluxos)
  xxx<-cbind(maiores_fluxos,a)
  colnames(xxx)<-c("Maiores dentre:","C1","C2","C3","C4")
  return(xxx)
}
class_nominal(dados_lucas,pesos_lucas,classes_lucas,pref,indiferencas)

