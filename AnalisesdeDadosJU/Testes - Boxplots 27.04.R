##HIPÓTESE 5.1

wilcox.test( Severidade_Micro_Interno,Severidade_Pequenas_Interno,alt="less")
wilcox.test( Severidade_Micro_Interno,Severidade_Pequenas_Interno,alt="greater")

wilcox.test( Severidade_Pequenas_Interno, Severidade_Micro_Interno,alt="less")
wilcox.test( Severidade_Pequenas_Interno, Severidade_Micro_Interno,alt="greater")

##HIPÓTESE 5.2

wilcox.test(Severidade_Micro_Externo, Severidade_Pequenas_Externo,alt="less")
wilcox.test(Severidade_Micro_Externo, Severidade_Pequenas_Externo,alt="greater")

wilcox.test(Severidade_Pequenas_Externo,Severidade_Micro_Externo,alt="less")
wilcox.test(Severidade_Pequenas_Externo,Severidade_Micro_Externo,alt="greater")


##HIPÓTESE 6.1

wilcox.test( Detecção_Pequenas_Interno, Detecção_Micro_Interno ,alt="less")
wilcox.test( Detecção_Pequenas_Interno, Detecção_Micro_Interno,alt="greater")

##HIPÓTESE 6.2

wilcox.test( Detecção_Pequenas_Externo,Detecção_Micro_Externo,alt="less")
wilcox.test( Detecção_Pequenas_Externo,Detecção_Micro_Externo,alt="greater")


##HIPÓTESE 3

wilcox.test(  empre_consu_micro222,empre_consu_pequenas222, alt="greater")
wilcox.test(  empre_consu_micro222,empre_consu_pequenas222, alt="less")


#############################BOXPLOTS#####################################

##BOXPLOT 1 - Avaliação da Gestão de Riscos

boxplot(c(AnaliseRiscoMicro222),c(AnaliseRiscoPequenas222),ylab="Risk management assessment",
ylim=c(0,5),names=c("Microcompanies","Small companies"),xlab="Company size",las=1,
boxwex=0.7,frame.plot=T)
legend(1.75,0.75,legend=c("W = 35","p-value = 0.0252"),bty="n")


##BOXPLOT 2 - Avaliação do Relacionamento com os fornecedores

boxplot(c(empre_forne_micro222),c(empre_forne_pequena222),ylab="Relationship assessment with suppliers",
ylim=c(0,5),names=c("Micro companies","Small companies"),xlab="Company size",las=1,
boxwex=0.7,frame.plot=T)
legend(1.75,0.75,legend=c("W = 48.5","p-value = 0.1318"),bty="n")


##BOXPLOT 3 - Avaliação do Relacionamento com os clientes


boxplot(c(empre_consu_micro222),c(empre_consu_pequenas222),ylab="Relationship assessment with clients",
ylim=c(0,5),names=c("Micro companies","Small companies"),xlab="Company size",las=1,
boxwex=0.7,frame.plot=T)
legend(1.75,0.75,legend=c("W = 63","p-value = 0.4373"),bty="n")


##BOXPLOT 4 - Avaliação da Ocorrencia de Riscos INTERNOS e EXTERNOS

par(mfrow=c(1,2))
#INTERNO
boxplot(c(Ocorrencia_Micros_Interno),c(Ocorrencia_Pequenas_Interno),
ylab="Assessment of the occurrence of risks internal",
ylim=c(0,5),names=c("Micro companies","Small companies"),xlab="Company size",las=1,
boxwex=0.4,frame.plot=T)
legend(1.75,0.75,legend=c("W = 44","p-value = 0.08786"),bty="n")

#EXTERNO
boxplot(c(Ocorrencia_Micros_Externo),c(Ocorrencia_Pequenas_Externo),
ylab="Assessment of the occurrence of risks external",
ylim=c(0,5),names=c("Micro companies","Small companies"),xlab="Company size",las=1,
boxwex=0.4,frame.plot=T)
legend(1.75,0.75,legend=c("W = 66","p-value = 0.5131"),bty="n")


##BOXPLOT 5 - Avaliação da Severidade de Riscos INTERNOS e EXTERNOS

par(mfrow=c(1,2))
#INTERNO
boxplot(c(Severidade_Micro_Interno),c(Severidade_Pequenas_Interno),
ylab="Internal risks severity assessment",
ylim=c(0,5),names=c("Micro companies","Small companies"),xlab="Company size",las=1,
boxwex=0.4,frame.plot=T)
legend(1.75,0.75,legend=c("W = 74.5","p-value = 0.7191"),bty="n")

#EXTERNO
boxplot(c(Severidade_Micro_Externo),c(Severidade_Pequenas_Externo),
ylab="External risks severity assessment",
ylim=c(0,5),names=c("Micro companies","Small companies"),xlab="Company size",las=1,
boxwex=0.4,frame.plot=T)
legend(1.75,0.75,legend=c("W = 67","p-value = 0.539"),bty="n")


##BOXPLOT 6 - Avaliação da Dificuldade de Detecção dos Riscos INTERNOS e EXTERNOS

par(mfrow=c(1,2))
#INTERNO
boxplot(c(Detecção_Micro_Interno),c(Detecção_Pequenas_Interno),
ylab="Assessment of the difficulty of detecting internal risks",
ylim=c(0,5),names=c("Micro companies","Small companies"),xlab="Company size",las=1,
boxwex=0.4,frame.plot=T)
legend(1.75,0.75,legend=c("W = 66","p-value = 0.513"),bty="n")

#EXTERNO
boxplot(c(Detecção_Micro_Externo),c(Detecção_Pequenas_Externo),
ylab="Assessment of the difficulty of detecting external risks",
ylim=c(0,5),names=c("Micro companies","Small companies"),xlab="Company size",las=1,
boxwex=0.4,frame.plot=T)
legend(1.75,0.75,legend=c("W = 59","p-value = 0.3319"),bty="n")

####GRÁFICOS DE DISPERSÃO#######

Hipótese 7 - Gestão de Riscos x Ocorrencia de Rupturas Internas/Externas

par(mfrow=c(2,2))
#(1,1)Microempresas - Griscos x Ocorrencias INternas
plot(AnaliseRiscoMicro222,Ocorrencia_Micros_Interno,ylim=c(0,5),xlim=c(1,5),
ylab="Avaliação da ocorrencia de riscos internos",xlab="Avaliação da gestão de riscos",las=1)
legend(2.5,0.75,legend=c("? = -0.1131455"),bty="n")

#(1,2)Pequenas empresas - Griscos x Ocorrencias INternas
plot(AnaliseRiscoPequenas222,Ocorrencia_Pequenas_Interno,ylim=c(0,5),xlim=c(1,5),
ylab="",xlab="Avaliação da gestão de riscos",las=1)
legend(2.5,0.75,legend=c("? = -0.1405564"),bty="n")

#(2,1)Microempresas - Griscos x Ocorrencias EXternas
plot(AnaliseRiscoMicro222,Ocorrencia_Micros_Externo,ylim=c(0,5),xlim=c(1,5),
ylab="Avaliação da ocorrencia de riscos externos",xlab="Avaliação da gestão de riscos",las=1)
legend(2.5,0.75,legend=c("? = -0.3459258"),bty="n")

#(2,2)Pequenas empresas - Griscos x Ocorrencias EXternas
plot(AnaliseRiscoPequenas222,Ocorrencia_Pequenas_Externo,ylim=c(0,5),xlim=c(1,5),
ylab="",xlab="Avaliação da gestão de riscos",las=1)
legend(2.5,0.75,legend=c("? = 0.2666667"),bty="n")



Hipótese 8 - Gestão de Riscos x Relacionamento com os fornecedores

par(mfrow=c(1,2))
#(1,1)Microempresas - Griscos x Relacionamento com os fornecedores
plot(AnaliseRiscoMicro222,empre_forne_micro222,ylim=c(0,5),xlim=c(1,5),
ylab="Avaliação do relacionamento com os fornecedores",xlab="Avaliação da gestão de riscos",las=1)
legend(3,0.75,legend=c("? = 0.02238946"),bty="n")

#(1,2)Pequenas empresas - Griscos x Relacionamento com os fornecedores
plot(AnaliseRiscoPequenas222,empre_forne_pequena222,ylim=c(0,5),xlim=c(1,5),
ylab="",xlab="Avaliação da gestão de riscos",las=1)
legend(3,0.75,legend=c("? = 0.3853132"),bty="n")

Hipótese 9 - Relacionamento com os fornecedores x Nº de fornecedores

par(mfrow=c(1,2))
#(1,1)Microempresas - Relacionamento com os fornecedores x Nº de fornecedores
plot(empre_forne_micro222,N_forne_micro,ylim=c(0,5),xlim=c(1,5),
ylab="Quantidade de fornecedores",xlab="Avaliação do relacionamento com os fornecedores",las=1)
legend(3,0.75,legend=c("? = -0.4003079"),bty="n")

#(1,2)Pequenas empresas - Relacionamento com os fornecedores x Nº de fornecedores
plot(empre_forne_pequena222,N_forne_pequena,ylim=c(0,5),xlim=c(1,5),
ylab="",xlab="Avaliação do relacionamento com os fornecedores",las=1)
legend(3,0.75,legend=c("? = 0"),bty="n")

Hipótese 10 - Relacionamento com os fornecedores x Compartilhamento de infomações

par(mfrow=c(1,2))
#(1,1)Microempresas - Relacionamento com os fornecedores x comp info
plot(empre_forne_micro222,comp_info_micro,ylim=c(0,5),xlim=c(1,5),
ylab="Avaliação quanto ao compartilhamento de informações",xlab="Avaliação do relacionamento com os fornecedores",las=1)
legend(3,0.75,legend=c("? = -0.2537765"),bty="n")

#(1,2)Pequenas empresas - Relacionamento com os fornecedores x Nº de fornecedores
plot(empre_forne_pequena222,comp_info_pequena,ylim=c(0,5),xlim=c(1,5),
ylab="",xlab="Avaliação do relacionamento com os fornecedores",las=1)
legend(3,0.75,legend=c("? = -0.2692341"),bty="n")



Npf<-c("mais de 20","mais de 20","mais de 20","mais de 20","mais de 20",
"de 10 à 20","de 10 à 20","de 10 à 20","mais de 20","de 2 à 10","de 10 à 20")







