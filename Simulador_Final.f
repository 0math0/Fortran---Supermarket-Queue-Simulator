C     **************************************************************
C     * Simulador 2 - Simulaá∆o da fila de um mercado com n caixas *
C     **************************************************************

C     *********************************************************
C     * C†lculo utilizado para gerar o n£mero pseudoaleat¢rio *
C     *********************************************************

C      X(n) = 16807 * x(n-1) * mod(2^(32-1) - 1)

C     ******************************
C     * Descriá∆o de cada vari†vel *
C     ******************************

C      TEC: tempo de espera de chegada de clientes
C      TS: tempo de serviáo, Ç o tempo de atendimento de cada cliente
C      T: tempo real
C      Ti: tempo de in°cio do atendimento de cada cliente nos caixas
C      Tf: tempo de fim do atendimento de cada cliente nos caixas
C      Tfc: tempo de fim de cada cliente que chega
C      Tfila: tempo que cada cliente fica na fila
C      Tsis: tempo no sistema, Ç o tempo que cada cliente fica no sistema
C      Toci: tempo ocioso do caixa, Ç o tempo em que os caixas se encontram sem cliente
C      probf: probabilidade de um cliente esperar na fila
C      nfila: n£mero de pessoas que ficaram esperando na fila
C      nsis: n£mero de clientes que se encontram sistema
C      somaoc: acumulor do tempo ocioso do caixa
C      probl: probabilidade de caixa livre
C      sTEC: acumulador dos TEC
C      mTEC: mÇdia dos TEC
C      sTS: acumulador dos TS
C      mTS: mÇdia dos TS
C      sTfila: acumulador dos tempos de espera na fila
C      mTfila: tempo mÇdio de espera na fila
C      sTsis: acumulador dos tempos gastos no sistema de cada cliente
C      mTsis: tempo mÇdio gasto no sistema
C      rnum: n£mero aleat¢rio gerado
C      NUM: n£mero de n£meros aleat¢rios
C      ISEED: semente ou valor inicial
C      dmax: maior valor inteiro que o gerador gera
C      pmod: 1 sobre dmax
C      k: controla quantas vezes o programa vai gerar simulaá‰es
C      mmTec: guarda o acumulo das mÇdias de Tec
C      mmTec2: guarda o acumulo dos quadrados das mÇdias de Tec
C      mmTs: guarda o acumulo das mÇdias de Ts
C      mmTs2: guarda o acumulo dos quadrados das mÇdias de Ts
C      mmTfila: guarda o acumulo das mÇdias de Tfila
C      mmTfila2: guarda o acumulo dos quadrados das mÇdias de Tfila
C      mmTsis: guarda o acumulo das mÇdias de Tsis
C      mmTsis2: guarda o acumulo dos quadrados das mÇdias de Tsis
C      z: n£mero de tabelas que ser∆o geradas
C      zr: z no tipo real para realizar alguns c†lculos
C      mm: mÇdia das mÇdias
C      varm: variÉncia das mÇdias
C      tvar: intervalo de erro no intervalo de confianáa
C      Taux2: auxiliar que salva o menor tf de caixa do vetor para o cliente
C      o: auxiliar que controla o menor tf de caixa no vetor para o cliente
C      u: auxiliar que controlar o vetor de caixas
C      ncaixa: numero de caixa no sistema
C      Toci2: tempo ocioso anterior
C      Taux: recebe os tempos finais dos clientes para ver quantos est∆o no sistema

C     ****************************
C     * Declaraá∆o das vari†veis *
C     ****************************

       REAL *8 rnum(10000),dmax,pmod,TEC,TS,T,Ti(10000),Tf(10000),Tfila,
     *Tsis,Toci(10000),probf,nfila,probl,somaoc,sTEC, Toci2(10000),
     *mTEC,sTS,mTS,sTfila,mTfila,sTsis,mTsis,mmTec,mmTec2,mm,varm,tvar,
     *zr,mmTs,mmTs2,mmTfila,mmTfila2,mmTsis,mmTsis2,Taux2, Taux(10000)

       integer ISEED,NUM,nsis,k,z,o,u,ncaixa

C     **************************************************
C     * Abertura do arquivo que conter† as informaá‰es *
C     **************************************************

       open(1, file='Simulador_dados.dat', status='unknown')

C     ************
C     * Entradas *
C     ************

C     Entrada do n£mero de tabelas que ser∆o geradas
       write(*,83)
83     FORMAT('Numero de replicas: ')
       READ(*,84)z
84     FORMAT(I5)

C     Entrada da semente
       write(*,80)
80     FORMAT('Semente: ')
       READ(*,92)ISEED
92     FORMAT(I5) ! I5: VALOR MAXIMO ACEITO 99999(qualquer n£mero com atÇ 5 caracteres)
       write(*, 81)ISEED
81     FORMAT('A semente Ç: ',I5)

C     Entrada do n£mero de cliente por tabela
       write(*,82)
82     FORMAT('Numero de clientes a serem gerados ')
       READ(*,92)num
       
C     Zera vari†veis acumuladoras
       mmTec=0
       mmTec2=0
       mmTs=0
       mmTs2=0
       mmTfila=0
       mmTfila2=0
       mmTsis=0
       mmTsis2=0

C      Loop que gera as tabelas e estat°sticas diferentes
       DO 51 k=1,z
       write(1,62)k
62     FORMAT('Tabela ',I3)
       pmod= 2147783647.D0 ! = 2**(32-1)-1
       dmax= 1.D0 /pmod

       DO 1 i=1,10000
       Taux(i)=0.d0
       Ti(i)=0.d0
       Tf(i)=0.d0
       Toci(i)=0.d0
       Toci2(i)=0.d0
1      rnum(i)=0.0d0
       rnum(1)=ISEED * dmax

       TEC=0
       TS=0
       T=0
       Tfila=0
       Tsis=0
       nsis=1
       nfila=0
       somaoc=0
       sTEC=0
       sTS=0
       sTfila=0
       sTsis=0
       ncaixa=1

C     *******************************
C     * Sa°da das colunas na tabela *
C     *******************************

       write(1,76)
76     format('TEC',6x,'TS',6x,'T',7x,'Ti',6x,'Tf',6x,'Tfila',3x,'Tsis'
     *,4x,'Toci',4x,'Nsis',4x,'Caixa')

       rnum(1)=cong(ISEED)

C     ******************************
C     * Gera os n£meros aleat¢rios *
C     ******************************

       DO 10 i=2,num+1
       
C     Gera um n£mero pseudoaleat¢rio para cada espaáo em rnum
       rnum(i)= cong(ISEED)
       
C     Gera um tempo de espera de chegada a partir de um n£mero pseudoaleat¢rio gerado
       TEC=-1.0D0*log(rnum(i))
       rnum(i)= cong(ISEED)
       
C     Gera um tempo de serviáo a partir de um n£mero pseudoaleat¢rio gerado
       TS=-2.0D0*log(rnum(i))

C     ************
C     * C†lculos *
C     ************
        
C     C†lculo do tempo real
       T=T+TEC
C     ******************************
C     * C†lculo do tempo de In°cio *
C     ******************************
C      Caso 1) Se tempo de fim for menor que tempo real, tempo de in°cio Ç o tempo real
       DO u=1, ncaixa
       if(Tf(u).le.T) then
       Ti(u)=T
       o=u
       go to 101
       end if
       end do
C      Caso 2) Se tempo de fim for maior que tempo real, tempo de in°cio Ç o menor tempo de fim
       DO u=1, ncaixa
       if(u.eq.1) Taux2=Tf(u)
       if(Taux2.gt.Tf(u)) then
       Taux2=Tf(u)
       o=u
       end if
       end do
       Ti(o)=Taux2
C      Caso 3) Se for o primeiro caso, tempo de in°cio Ç o tempo de espera de chegada
       if(i.eq.2) Ti(1)=TEC
101    if(i.eq.2) o=1

C     C†lculo do tempo ocioso do caixa
       DO u=1, ncaixa
C      Caso 1) Se o tempo real for menor que o tempo final anterior, o tempo ocioso Ç zero
       if(T.lt.Tf(u)) Toci(u)=0
C      Tempo ocioso anterior impede o tempo ocioso de acumular a mais do que deve
       Toci2(u)=Toci(u)
C      Caso 2) Se o tempo real for maior ou igual que o tempo final anterior, o tempo ocioso Ç a diferenáa do tempo real com o tempo final anterior
       if(T.ge.Tf(u) .AND. Toci2(u).ne.0) Toci(u)=T-Tf(u)
C      Caso 3) Se o tempo ocioso anterior n∆o for zero
       if(T.ge.Tf(u) .AND. Toci2(u).eq.0) Toci(u)=T-Toci2(u)
C      Atualiza o Toci2 caso seja atribuido novo valor no Caso 2)
       Toci2(u)=Toci(u)
       end do

C     C†lculo do tempo final
       Tf(o)=Ti(o)+TS

C     C†lculo do tempo de espera na fila
C      Caso 1) Se o tempo inicial for menor ou igual que o tempo real, tempo na fila Ç zero
       if(Ti(o).le.T) Tfila=0
C      Caso 2) Se o tempo inicial for maior, o tempo na fila Ç a diferenáa entre o tempo real e o tempo inicial
       if(Ti(o).gt.T) Tfila=Ti(o)-T
       
C     C†lculo do n£mero de pessoas no sistema
       Taux(i)=Tf(o)
       nsis=i
       DO u=1,i
       if(Taux(u).le.T) nsis=nsis-1
       end do
        
C     C†lculo do tempo no sistema
102    Tsis=Ts+Tfila

C     ****************************
C     * C†lculo dos acumuladores *
C     ****************************

C     C†lculo do acumulo dos TEC
       sTEC=sTEC+TEC
        
C     C†lculo do acumulo dos TS
       sTS=sTS+TS
        
C     C†lculo do acumulo do tempo de espera na fila
       sTfila=sTfila+Tfila

C     C†lculo do acumulo dos tempos ociosos
       DO u=1, ncaixa
       somaoc=Toci(u)+somaoc
       end do

C     C†lculo do acumulo de pessoas que ficaram na fila
       if(nsis.gt.1) nfila=nfila+1

C     C†lculo do acumulo do tempo gasto de cada cliente no sistema
       sTsis=sTsis+Tsis

C     ****************************************
C     * Sa°da dos valores gerados no arquivo *
C     ****************************************

       if(num.le.200) write(*,94)TEC,TS,T,Ti(o),Tf(o),Tfila,Tsis,Toci(o)
     *,nsis,o
10     write(1,94)TEC,TS,T,Ti(o),Tf(o),Tfila,Tsis,Toci(o),nsis,o
94     FORMAT(F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,
     *F6.3,2x,I5,2x,I5)

C     ****************
C     * Estat°sticas *
C     ****************

C     Item 1:
C     C†lculo da probabilidade de caixa livre
       probl=somaoc/(NUM-1)
C     Sa°da da probabilidade do caixa estar livre
       write(1,78)probl
78     FORMAT('\nProbabilidade de caixa livre: ', F8.6)

C     Item 2:
C     C†lculo da mÇdia dos TEC
       mTEC=sTEC/(NUM-1)
C     Sa°da da mÇdia dos TEC
       write(1,79)mTEC
79     FORMAT('\nMedia dos TEC: ',F8.6)

C     Item 3:
C     C†lculo da mÇdia dos TS
       mTS=sTS/(NUM-1)
C     Sa°da da mÇdia dos TS
       write(1,71)mTS
71     FORMAT('\nMedia dos TS: ',F8.6)

C     Item 4:
C     C†lculo do tempo mÇdio de espera na fila
       mTfila=sTfila/(NUM-1)
C     Sa°da do tempo mÇdio de espera na fila
       write(1,72)mTfila
72     FORMAT('\nTempo medio de espera na fila: ',F8.6)

C     Item 5:
C     C†lculo da probabilidade de um cliente esperar na fila
       probf=nfila/(NUM)
C     Sa°da da probabilidade de um cliente esperar na fila
       write(1,77)probf
77     FORMAT('\nProbabilidade de um cliente esperar na fila: ',F8.6)

C     Item 6:
C     C†lculo do tempo mÇdio gasto no sistema
       mTsis=sTsis/(NUM-1)
C     Sa°da do tempo mÇdio que um cliente permanece no sistema
       write(1,73)mTfila
73     FORMAT('\nTempo medio gasto no sistema: ',F8.6)

C     Acumulo das mÇdias do TEC
C      MÇdia das mÇdias TEC
       mmTec=mmTec+mTec
C      Quadrado das mÇdias das mÇdias TEC
       mmTec2=mmTec2+mTec**2
       
C     Acumulo das mÇdias do TS
C      MÇdia das mÇdias TS
       mmTs=mmTs+mTs
C      Quadrado das mÇdias das mÇdias TS
       mmTs2=mmTs2+mTs**2
       
C     Acumulo das mÇdias do Tfila
C      MÇdia das mÇdias Tfila
       mmTfila=mmTfila+mTfila
C      Quadrado das mÇdias das mÇdias Tfila
       mmTfila2=mmTfila2+mTfila**2
       
C     Acumulo das mÇdias do Tsis
C      MÇdia das mÇdias Tsis
       mmTsis=mmTsis+mTsis
C      Quadrado das mÇdias das mÇdias Tsis
       mmTsis2=mmTsis2+mTsis**2
       
C     Fim do loop que gera valores diferentes e pega semente nova
       write(1,64)
64     FORMAT('\n')
51     ISEED=ISEED+1

C     ***********************
C     * ÷ndice de Confianáa *
C     ***********************

C     Intervalo de confianáa da mÇdia do TEC
       write(1,61)
61     FORMAT('*********************************************************
     **************\nIntervalo de confianca(TEC): ')

C     C†lculo da mÇdia das mÇdias de TEC
       mm=mmTec/z
C     Sa°da da mÇdia das mÇdias do TEC
       write(1,52)mm
52     FORMAT('\nMedia das medias do TEC: ',F10.3)

C     C†lculo da variÉncia das mÇdias do TEC
       varm=(mmTec2-((mmTec**2)/z))/(z-1)
C     Sa°da da variÉncia das mÇdias do TEC
       write(1,53)varm
53     FORMAT('\nVariancia das medias do TEC: ',F10.3)

C     C†lculo do desvio padr∆o das mÇdias do TEC
       write(1,54)sqrt(varm)
54     FORMAT('\nDesvio padrao das medias do TEC: ',F10.3)

C     C†lculo do intervalo de confianáa
       zr=z
       tvar=1.96d0*(sqrt(varm)/sqrt(zr)) !1.96 valor do tstudent
       write(1,55)mm-tvar,mm,mm+tvar
55     FORMAT('\nIntervalo de confianca(TEC): ',F10.3,' ~ ',F10.3,' ~ ',
     *F10.3)

C     Intervalo de confianáa da mÇdia do TS
       write(1,65)
65     FORMAT('*********************************************************
     **************\nIntervalo de confianca(TS): ')

C     C†lculo da mÇdia das mÇdias de TS
       mm=mmTs/z
C     Sa°da da mÇdia das mÇdias do TS
       write(1,56)mm
56     FORMAT('\nMedia das medias do TS: ',F10.3)

C     C†lculo da variÉncia das mÇdias do TS
       varm=(mmTs2-((mmTs**2)/z))/(z-1)
C     Sa°da da variÉncia das mÇdias do TS
       write(1,57)varm
57     FORMAT('\nVariancia das medias do TS: ',F10.3)

C     C†lculo do desvio padr∆o das mÇdias do TS
       write(1,58)sqrt(varm)
58     FORMAT('\nDesvio padrao das medias do TS: ',F10.3)

C     C†lculo do intervalo de confianáa
       zr=z
       tvar=1.96d0*(sqrt(varm)/sqrt(zr)) !1.96 valor do tstudent
       write(1,59)mm-tvar,mm,mm+tvar
59     FORMAT('\nIntervalo de confianca(TS): ',F10.3,' ~ ',F10.3,' ~ ',
     *F10.3)
     
C     Intervalo de confianáa da mÇdia do tempo de espera na fila
       write(1,66)
66     FORMAT('*********************************************************
     **************\nIntervalo de confianca(Tfila): ')

C     C†lculo da mÇdia das mÇdias de Tfila
       mm=mmTfila/z
C     Sa°da da mÇdia das mÇdias do Tfila
       write(1,31)mm
31     FORMAT('\nMedia das medias do Tfila: ',F10.3)

C     C†lculo da variÉncia das mÇdias do Tfila
       varm=(mmTfila2-((mmTfila**2)/z))/(z-1)
C     Sa°da da variÉncia das mÇdias do Tfila
       write(1,32)varm
32     FORMAT('\nVariancia das medias do Tfila: ',F10.3)

C     C†lculo do desvio padr∆o das mÇdias do Tfila
       write(1,33)sqrt(varm)
33     FORMAT('\nDesvio padrao das medias do Tfila: ',F10.3)

C     C†lculo do intervalo de confianáa
       zr=z
       tvar=1.96d0*(sqrt(varm)/sqrt(zr)) !1.96 valor do tstudent
       write(1,34)mm-tvar,mm,mm+tvar
34     FORMAT('\nIntervalo de confianca(Tfila): ',F10.3,' ~ ',F10.3,' ~
     *',F10.3)
     
C     Intervalo de confianáa da mÇdia de tempo no sistema
       write(1,67)
67     FORMAT('*********************************************************
     **************\nIntervalo de confianca(Tsis): ')

C     C†lculo da mÇdia das mÇdias de Tsis
       mm=mmTsis/z
C     Sa°da da mÇdia das mÇdias do Tsis
       write(1,35)mm
35     FORMAT('\nMedia das medias do Tsis: ',F10.3)

C     C†lculo da variÉncia das mÇdias do Tsis
       varm=(mmTsis2-((mmTsis**2)/z))/(z-1)
C     Sa°da da variÉncia das mÇdias do Tsis
       write(1,36)varm
36     FORMAT('\nVariancia das medias do Tsis: ',F10.3)

C     C†lculo do desvio padr∆o das mÇdias do Tsis
       write(1,37)sqrt(varm)
37     FORMAT('\nDesvio padrao das medias do Tsis: ',F10.3)

C     C†lculo do intervalo de confianáa
       zr=z
       tvar=1.96d0*(sqrt(varm)/sqrt(zr)) !1.96 valor do tstudent
       write(1,38)mm-tvar,mm,mm+tvar
38     FORMAT('\nIntervalo de confianca(Tsis): ',F10.3,' ~ ',F10.3,' ~ '
     *,F10.3)
       

C     **************************
C     * Fechamento do programa *
C     **************************
       read(*,*)
       CLOSE(1)
       end

C     ******************
C     * SUBROTINA CONG *
C     ******************

C     ê a subrotina que gera os n£meros pseudoaleat¢rios a partir de uma semente
       function cong(ISEED)
       REAL*8 rmod, pmod, dmax
       Integer  ISEED, imod
       rmod = dfloat(ISEED)    !dble(ISEED)
       pmod=2147483647.D0
       dmax = 1.0d0/pmod
       rmod= rmod * 16807.D0
       imod=rmod*dmax
       rmod=rmod-pmod*imod
       cong=rmod*dmax
       ISEED=rmod
       return
       end
        
