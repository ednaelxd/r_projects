#Criação de uma função que irá analisar os dados categoricos inseridos 
e inferir certa relação entre eles
#parâmetros de entrada: x= número de vetores numéricos a serem analisado,
no nosso caso, cada vetor desses contém respostas de perguntas;

#lógica, calcular a média das respostas e retornar avaliação para cada empresa:
média 5 = relação ótima
média 4 = relação boa
média 3 = relação regular
média 2 = relação ruim
média 1 = relação péssima

Avaliador<-function(x){

media=0

soma=0

z=length(colnames(x))

for(i in 1:z){

soma<-soma+sum(x[,i])

}

media=(soma/(z*length(x[,1])))
if(media<5){

print("teste ok")

}else{

print("problema")

}
}




