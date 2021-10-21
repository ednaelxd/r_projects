Avaliador<-function(x,y,z){
#x é a matriz
#y é o número de linhas
#z é o número de colunas

media<-matrix #matriz com media de cada linha
matrizR<-matrix #matriz soma de cada linha
matrizI<-matrix #matriz com interpretação de cada linha
matrizF<-matrix #matriz com dados finais

for(i in 1:y){

matrizR[i,1]<-sum(x[i,])

}
for(i in 1:y){
media[i,1]= matrizR[i,1]/(length(colnames(x)))

	if(media[i,1]>=3.6 && media[1,i]<=4.4){
	  matrizI[i,1]<-"Relação ótima"
	}
	else{
	if(media[i,1]>=2.7 && media[1,i]<=3.5){
	  matrizI[i,1]<-"Relação boa"
	}
	else{
	if(media[i,1]>=1.8 && media[1,i]<=2.6){
	  matrizI[i,1]<-"Relação regular"
	}
	else{
	if(media[i,1]>=0.9 && media[1,i]<=1.7){
	  matrizI[i,1]<-"Relação ruim"
	}
	else{
	if(media[i,1]>=0 && media[1,i]<=0.8){
	  matrizI[i,1]<-"Relação péssima"
	 }
	}
     }
    }
   }

  matrizF<-cbind(matrizR,media,matrizI)
}
return(matrizF)
}
