C     *****************************************************************************
C     * Simulador 1 - Simula��o da fila de um caixa de um mercadinho com um caixa *
C     *****************************************************************************

C     *********************************************************
C     * C�lculo utilizado para gerar o n�mero pseudoaleat�rio *
C     *********************************************************

C      X(n) = 16807 * x(n-1) * mod(2^(32-1) - 1)

C     ******************************
C     * Descri��o de cada vari�vel *
C     ******************************

C      TEC: tempo de espera de chegada de clientes
C      TS: tempo de servi�o, � o tempo de atendimento de cada cliente
C      T: tempo real
C      Ti: tempo de in�cio do atendimento de cada cliente
C      Tf: tempo de fim do atendimento de cada cliente
C      Tfila: tempo que cada cliente fica na fila
C      Tsis: tempo no sistema, � o tempo que cada cliente fica no sistema
C      Toci: tempo ocioso do caixa, � o tempo em que o caixa se encontra sem cliente
C      probf: probabilidade de um cliente esperar na fila
C      nfila: n�mero de pessoas que ficaram esperando na fila
C      nsis: n�mero de clientes que se encontram sistema
C      somaoc: acumulor do tempo ocioso do caixa
C      probl: probabilidade de caixa livre
C      sTEC: acumulador dos TEC
C      mTEC: m�dia dos TEC
C      sTS: acumulador dos TS
C      mTS: m�dia dos TS
C      sTfila: acumulador dos tempos de espera na fila
C      mTfila: tempo m�dio de espera na fila
C      sTsis: acumulador dos tempos gastos no sistema de cada cliente
C      mTsis: tempo m�dio gasto no sistema
C      j: controla os tempos de fim dos clientes no vetor Taux
C      Taux: tempo auxiliar � o vetor que guarda os tempos finais anteriores
C      rnum: n�mero aleat�rio gerado
C      NUM: n�mero de n�meros aleat�rios
C      ISEED: semente ou valor inicial
C      dmax: maior valor inteiro que o gerador gera
C      pmod: 1 sobre dmax
C      k: controla quantas vezes o programa vai gerar simula��es
C      mmTec: guarda o acumulo das m�dias de Tec
C      mmTec2: guarda o acumulo dos quadrados das m�dias de Tec
C      mmTs: guarda o acumulo das m�dias de Ts
C      mmTs2: guarda o acumulo dos quadrados das m�dias de Ts
C      mmTfila: guarda o acumulo das m�dias de Tfila
C      mmTfila2: guarda o acumulo dos quadrados das m�dias de Tfila
C      mmTsis: guarda o acumulo das m�dias de Tsis
C      mmTsis2: guarda o acumulo dos quadrados das m�dias de Tsis
C      z: n�mero de tabelas que ser�o geradas
C      zr: z no tipo real para realizar alguns c�lculo
C      mm: m�dia das m�dias
C      varm: vari�ncia das m�dias
C      tvar: intervalo de erro no intervalo de confian�a

C     ****************************
C     * Declara��o das vari�veis *
C     ****************************

       REAL *8 rnum(10000),dmax,pmod,TEC,TS,T,Ti,Tf,Tfila,Tsis,Toci,
     *Taux(10000),probf,nfila,probl,somaoc,sTEC,mTEC,sTS,mTS,sTfila,
     *mTfila,sTsis,mTsis,mmTec,mmTec2,mm,varm,tvar,zr,mmTs,mmTs2,mmTfila
     *,mmTfila2,mmTsis,mmTsis2

       integer ISEED,NUM,nsis,j,k,z
        
C     **************************************************
C     * Abertura do arquivo que conter� as informa��es *
C     **************************************************

       open(1, file='Simulador.dat', status='unknown')

C     ************
C     * Entradas *
C     ************

C     Entrada do n�mero de tabelas que ser�o geradas
       write(*,83)
83     FORMAT('Numero de tabelas: ')
       READ(*,84)z
84     FORMAT(I5)

C     Entrada da semente
       write(*,80)
80     FORMAT('Semente: ')
       READ(*,92)ISEED
92     FORMAT(I5) ! I5: VALOR MAXIMO ACEITO 99999(qualquer n�mero com at� 5 caracteres)
       write(*, 81)ISEED
81     FORMAT('A semente �: ',I5)

C     Entrada do n�mero de cliente por tabela
       write(*,82)
82     FORMAT('Numero de clientes a serem gerados ')
       READ(*,92)num
       
C     Zera vari�veis acumuladoras
       mmTec=0
       mmTec2=0
       mmTs=0
       mmTs2=0
       mmTfila=0
       mmTfila2=0
       mmTsis=0
       mmTsis2=0

C      Loop que gera as tabelas e estat�sticas diferentes
       DO 51 k=1,z
       write(1,62)k
62     FORMAT('Tabela ',I3)
       pmod= 2147783647.D0 ! = 2**(32-1)-1
       dmax= 1.D0 /pmod

       DO 1 i=1,10000
       Taux(i)=0.0d0
1      rnum(i)=0.0d0
       rnum(1)=ISEED * dmax

       TEC=0
       TS=0
       T=0
       Ti=0
       Tf=0
       Tfila=0
       Toci=0
       Tsis=0
       nsis=1
       j=0
       nfila=0
       somaoc=0
       sTEC=0
       sTS=0
       sTfila=0
       sTsis=0
       
C     *******************************
C     * Sa�da das colunas na tabela *
C     *******************************

       write(1,76)
76     format('TEC',6x,'TS',6x,'T',7x,'Ti',6x,'Tf',6x,'Tfila',3x,'Tsis'
     *,4x,'Toci',4x,'Nsis')

       rnum(1)=cong(ISEED)

C     ******************************
C     * Gera os n�meros aleat�rios *
C     ******************************

       DO 10 i=2,num+1
       
C     Gera um n�mero pseudoaleat�rio para cada espa�o em rnum
       rnum(i)= cong(ISEED)
       
C     Gera um tempo de espera de chegada a partir de um n�mero pseudoaleat�rio gerado
       TEC=-1.0D0*log(rnum(i))
       rnum(i)= cong(ISEED)
       
C     Gera um tempo de servi�o a partir de um n�mero pseudoaleat�rio gerado
       TS=-2.0D0*log(rnum(i))

C     ************
C     * C�lculos *
C     ************
        
C     C�lculo do tempo real
       T=T+TEC

C     Tempo auxiliar guarda o tempo final do cliente atual
       Taux(i-2)=Tf
        
C     C�lculo do tempo de In�cio
C      Caso 1) Se tempo de fim for menor que tempo real, tempo de in�cio � o tempo real
       if(Tf.le.T) Ti=T
C      Caso 2) Se tempo de fim for maior que tempo real, tempo de ini�cio � o tempo de fim
       if(Tf.gt.T) Ti=Tf
C      Caso 3) Se for o primeiro caso, tempo de in�cio � o tempo de espera de chegada
       if(i.eq.2) Ti=TEC
        
C     C�lculo do tempo de espera na fila
C      Caso 1) Se o tempo inicial for menor ou igual que o tempo real, tempo na fila � zero
       if(Ti.le.T) Tfila=0
C      Caso 2) Se o tempo inicial for maior, o tempo na fila � a diferen�a entre o tempo real e o tempo inicial
       if(Ti.gt.T) Tfila=Ti-T
        
C     C�lculo do n�mero de pessoas na fila
C      Caso 1) Se o tempo real passar o tempo de fim de algum cliente, o n�mero de pessoas do sistema diminui em 1
       if(T.gt.Taux(j)) nsis=nsis-1
C      Garante que o n�mero de pessoas no sistema n�o seja menor que 1
       if(nsis.lt.1) nsis=1
C      j indica o tempo de fim do pr�ximo cliente
       if(T.gt.Taux(j)) j=j+1
C      Caso 2) Se o tempo real for menor que o tempo de fim anterior, incrementa uma pessoa no sistema
       if(T.le.Taux(j)) nsis=nsis+1
C      Caso 3) Primeira ocorr�ncia, quando chega o primeiro cliente
       if(Tf.eq.0) nsis=1
        
C     C�lculo do tempo ocioso do caixa
C      Caso 1) Se o tempo real for menor que o tempo final anterior, o tempo ocioso � zero
       if(T.lt.Tf) Toci=0
C      Caso 2) Se o tempo real for maior ou igual que o tempo final anterior, o tempo ocioso � a diferen�a do tempo real com o tempo final anterior
       if(T.ge.Tf) Toci=T-Tf
        
C     C�lculo do tempo final
       Tf=Ti+TS
        
C     C�lculo do tempo no sistema
       Tsis=Ts+Tfila

C     ****************************
C     * C�lculo dos acumuladores *
C     ****************************

C     C�lculo do acumulo dos TEC
       sTEC=sTEC+TEC
        
C     C�lculo do acumulo dos TS
       sTS=sTS+TS
        
C     C�lculo do acumulo do tempo de espera na fila
       sTfila=sTfila+Tfila

C     C�lculo do acumulo dos tempos ociosos
       somaoc=Toci+somaoc

C     C�lculo do acumulo de pessoas que ficaram na fila
       if(nsis.gt.1) nfila=nfila+1

C     C�lculo do acumulo do tempo gasto de cada cliente no sistema
       sTsis=sTsis+Tsis

C     ****************************************
C     * Sa�da dos valores gerados no arquivo *
C     ****************************************

       if(num.le.200) write(*,94)TEC,TS,T,Ti,Tf,Tfila,Tsis,Toci,nsis
10     write(1,94)TEC,TS,T,Ti,Tf,Tfila,Tsis,Toci,nsis
94     FORMAT(F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,F6.3,2x,
     *F6.3,2x,I5)

C     ****************
C     * Estat�sticas *
C     ****************

C     Item 1:
C     C�lculo da probabilidade de caixa livre
       probl=somaoc/(NUM-1)
C     Sa�da da probabilidade do caixa estar livre
       write(1,78)probl
78     FORMAT('\nProbabilidade de caixa livre: ', F8.6)

C     Item 2:
C     C�lculo da m�dia dos TEC
       mTEC=sTEC/(NUM-1)
C     Sa�da da m�dia dos TEC
       write(1,79)mTEC
79     FORMAT('\nMedia dos TEC: ',F8.6)

C     Item 3:
C     C�lculo da m�dia dos TS
       mTS=sTS/(NUM-1)
C     Sa�da da m�dia dos TS
       write(1,71)mTS
71     FORMAT('\nMedia dos TS: ',F8.6)

C     Item 4:
C     C�lculo do tempo m�dio de espera na fila
       mTfila=sTfila/(NUM-1)
C     Sa�da do tempo m�dio de espera na fila
       write(1,72)mTfila
72     FORMAT('\nTempo medio de espera na fila: ',F8.6)

C     Item 5:
C     C�lculo da probabilidade de um cliente esperar na fila
       probf=nfila/(NUM-1)
C     Sa�da da probabilidade de um cliente esperar na fila
       write(1,77)probf
77     FORMAT('\nProbabilidade de um cliente esperar na fila: ',F8.6)

C     Item 6:
C     C�lculo do tempo m�dio gasto no sistema
       mTsis=sTsis/(NUM-1)
C     Sa�da do tempo m�dio que um cliente permanece no sistema
       write(1,73)mTfila
73     FORMAT('\nTempo medio gasto no sistema: ',F8.6)

C     Acumulo das m�dias do TEC
C      M�dia das m�dias TEC
       mmTec=mmTec+mTec
C      Quadrado das m�dias das m�dias TEC
       mmTec2=mmTec2+mTec**2
       
C     Acumulo das m�dias do TS
C      M�dia das m�dias TS
       mmTs=mmTs+mTs
C      Quadrado das m�dias das m�dias TS
       mmTs2=mmTs2+mTs**2
       
C     Acumulo das m�dias do Tfila
C      M�dia das m�dias Tfila
       mmTfila=mmTfila+mTfila
C      Quadrado das m�dias das m�dias Tfila
       mmTfila2=mmTfila2+mTfila**2
       
C     Acumulo das m�dias do Tsis
C      M�dia das m�dias Tsis
       mmTsis=mmTsis+mTsis
C      Quadrado das m�dias das m�dias Tsis
       mmTsis2=mmTsis2+mTsis**2
       
C     Fim do loop que gera valores diferentes e pega semente nova
       write(1,64)
64     FORMAT('\n')
51     ISEED=ISEED+1

C     ***********************
C     * �ndice de Confian�a *
C     ***********************

C     Intervalo de confian�a da m�dia do TEC
       write(1,61)
61     FORMAT('*********************************************************
     **************\nIntervalo de confianca(TEC): ')

C     C�lculo da m�dia das m�dias de TEC
       mm=mmTec/z
C     Sa�da da m�dia das m�dias do TEC
       write(1,52)mm
52     FORMAT('\nMedia das medias do TEC: ',F10.3)

C     C�lculo da vari�ncia das m�dias do TEC
       varm=(mmTec2-((mmTec**2)/z))/(z-1)
C     Sa�da da vari�ncia das m�dias do TEC
       write(1,53)varm
53     FORMAT('\nVariancia das medias do TEC: ',F10.3)

C     C�lculo do desvio padr�o das m�dias do TEC
       write(1,54)sqrt(varm)
54     FORMAT('\nDesvio padrao das medias do TEC: ',F10.3)

C     C�lculo do intervalo de confian�a
       zr=z
       tvar=1.96d0*(sqrt(varm)/sqrt(zr)) !1.96 valor do tstudent
       write(1,55)mm-tvar,mm,mm+tvar
55     FORMAT('\nIntervalo de confianca(TEC): ',F10.3,' ~ ',F10.3,' ~ ',
     *F10.3)

C     Intervalo de confian�a da m�dia do TS
       write(1,65)
65     FORMAT('*********************************************************
     **************\nIntervalo de confianca(TS): ')

C     C�lculo da m�dia das m�dias de TS
       mm=mmTs/z
C     Sa�da da m�dia das m�dias do TS
       write(1,56)mm
56     FORMAT('\nMedia das medias do TS: ',F10.3)

C     C�lculo da vari�ncia das m�dias do TS
       varm=(mmTs2-((mmTs**2)/z))/(z-1)
C     Sa�da da vari�ncia das m�dias do TS
       write(1,57)varm
57     FORMAT('\nVariancia das medias do TS: ',F10.3)

C     C�lculo do desvio padr�o das m�dias do TS
       write(1,58)sqrt(varm)
58     FORMAT('\nDesvio padrao das medias do TS: ',F10.3)

C     C�lculo do intervalo de confian�a
       zr=z
       tvar=1.96d0*(sqrt(varm)/sqrt(zr)) !1.96 valor do tstudent
       write(1,59)mm-tvar,mm,mm+tvar
59     FORMAT('\nIntervalo de confianca(TS): ',F10.3,' ~ ',F10.3,' ~ ',
     *F10.3)
     
C     Intervalo de confian�a da m�dia do tempo de espera na fila
       write(1,66)
66     FORMAT('*********************************************************
     **************\nIntervalo de confianca(Tfila): ')

C     C�lculo da m�dia das m�dias de Tfila
       mm=mmTfila/z
C     Sa�da da m�dia das m�dias do Tfila
       write(1,31)mm
31     FORMAT('\nMedia das medias do Tfila: ',F10.3)

C     C�lculo da vari�ncia das m�dias do Tfila
       varm=(mmTfila2-((mmTfila**2)/z))/(z-1)
C     Sa�da da vari�ncia das m�dias do Tfila
       write(1,32)varm
32     FORMAT('\nVariancia das medias do Tfila: ',F10.3)

C     C�lculo do desvio padr�o das m�dias do Tfila
       write(1,33)sqrt(varm)
33     FORMAT('\nDesvio padrao das medias do Tfila: ',F10.3)

C     C�lculo do intervalo de confian�a
       zr=z
       tvar=1.96d0*(sqrt(varm)/sqrt(zr)) !1.96 valor do tstudent
       write(1,34)mm-tvar,mm,mm+tvar
34     FORMAT('\nIntervalo de confianca(Tfila): ',F10.3,' ~ ',F10.3,' ~
     *',F10.3)
     
C     Intervalo de confian�a da m�dia de tempo no sistema
       write(1,67)
67     FORMAT('*********************************************************
     **************\nIntervalo de confianca(Tsis): ')

C     C�lculo da m�dia das m�dias de Tsis
       mm=mmTsis/z
C     Sa�da da m�dia das m�dias do Tsis
       write(1,35)mm
35     FORMAT('\nMedia das medias do Tsis: ',F10.3)

C     C�lculo da vari�ncia das m�dias do Tsis
       varm=(mmTsis2-((mmTsis**2)/z))/(z-1)
C     Sa�da da vari�ncia das m�dias do Tsis
       write(1,36)varm
36     FORMAT('\nVariancia das medias do Tsis: ',F10.3)

C     C�lculo do desvio padr�o das m�dias do Tsis
       write(1,37)sqrt(varm)
37     FORMAT('\nDesvio padrao das medias do Tsis: ',F10.3)

C     C�lculo do intervalo de confian�a
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

C     � a subrotina que gera os n�meros pseudoaleat�rios a partir de uma semente
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
        
