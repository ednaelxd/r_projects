Organiza_por_tamanho<-function(x,y){

#x é a matriz coluna que contém o tamanho das empresas, onde:
#1= micro empresa
#2= empresa pequena
#3= empresa média
#4= empresa grande

#y é a matriz que contem uma informação relativa ao mesmo par [i,j] de x
#as matrizes abaixo receberão os itens correspondentes de y para cada tipo de empresa

grandes_médias<-matrix(nrow=24)	
pequenas_micro<-matrix(nrow=24)

#contadores
k=1 
l=1


A <-"Parâmetros com tamanhos distintos"

if (length(x[,1])==length(y[,1])){

	for (i in 1:length(x[,1])){
	
	if (x[i,1]==1 || x[i,1]==2){
	      pequenas_micro[k,1]<-y[i,1];
		k<-k+1
		}
	if (x[i,1]==3 || x[i,1]==4){
		grandes_médias[l,1]<-y[i,1];
		l<-l+1
		}
	
	}
	MatrizDividida<-cbind(pequenas_micro,grandes_médias)
	return(MatrizDividida)

}else (return(A))

}
#CONDIÇÃO COMPLICADA#
if(x[i,1]==NA){s=1}
resolvida, substituindo os valores de NA por 0 em dados$X8EM

##########TESTES###############
Organiza_por_tamanho(TamanhoEmpresas,cbind(empre_forne.M2[,1]))

Organiza_por_tamanho(cbind(dados$X8),cbind(empre_forne.M2[,1]))
Organiza_por_tamanho(estrogonofe,estrogonofe)
Organiza_por_tamanho(estrogonofe,cbind(empre_forne.M2[,1]))
