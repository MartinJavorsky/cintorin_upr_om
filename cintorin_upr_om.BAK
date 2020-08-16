*PARAMETERS cSubor
*PUBLIC cKodKu,cNazovKu
set exclusive on
set deleted on
set decimals to 2
SET POINT TO "."
SET DATE GERMAN
SET CENTURY ON
SET DEFAULT TO "D:\"

cSubor=GETFILE("vtx","Vyberte mapu")
DO CASE
CASE 'Untitled' $ cSubor
	*SET DEFAULT TO &desta
	box_yes=MESSAGEBOX("  Úprava mapy bola zrušená ! ",16,"  Import dát")
	RETURN 
CASE EMPTY(cSubor)
	*SET DEFAULT TO &desta
	box_yes=MESSAGEBOX("  Úprava mapy bola zrušená ! ",16,"  Import dát")
	RETURN 
ENDCASE

cParam=GETFILE("csv","Vyberte súbor s parametrami")
DO CASE
CASE 'Untitled' $ cParam
	*SET DEFAULT TO &desta
	box_yes=MESSAGEBOX("  Úprava mapy bola zrušená ! ",16,"  Import dát")
	RETURN 
CASE EMPTY(cParam)
	*SET DEFAULT TO &desta
	box_yes=MESSAGEBOX("  Úprava mapy bola zrušená ! ",16,"  Import dát")
	RETURN 
ENDCASE

cAdrZdroj=SUBSTR(cSubor,1,RAT("\",cSubor))
*SET PATH TO &cAdrZdroj
cAdrKam=cAdrZdroj
cSubor=SUBSTR(cSubor,RAT("\",cSubor)+1)
cSubor=SUBSTR(cSubor,1,RAT(".",cSubor)-1)

cMapaHm=cAdrZdroj+cSubor+".dbf"
cFarby_K=cAdrZdroj+"farby_K.dbf"
cFarby_P="farby_P.dbf"

cPm=cAdrZdroj+cSubor+".vtx"
cPmU=cAdrKam+cSubor+"_p.vtx"
*cMapaCsv=cAdrZdroj+cSubor

DIMENSION Riadok(1)

IF FILE(cMapaHm)
   USE &cMapaHm IN 1
   SELECT 1
   ZAP
   ELSE
     CREATE DBF &cMapaHm (Riadok C(60))
ENDIF

IF FILE(cFarby_K)
   SELECT 2
   USE &cFarby_K
   ZAP 
 ELSE 
   CREATE DBF &cFarby_K (id N(3),typ C(20),farba C(10),k C(5))
ENDIF    
APPEND FROM &cParam DELIMITED WITH CHARACTER ";"
C=1
SCAN 
  DO CASE 
     CASE typ="Nesp. hrob"
          replace k WITH "1"
     CASE typ="Spev. hrob"
          replace k WITH "2"
     CASE typ="Spev. urna"
          replace k WITH "112"
     CASE typ="Neid. hrob"
          replace k WITH "113"
     CASE typ="Proj. urna"
          replace k WITH "114"
     CASE typ="Proj. hrob"
          replace k WITH "6"
     CASE UPPER(typ)="OBVOD"
          replace k WITH "1498"
     CASE "CHODN"$UPPER(typ)
          replace k WITH "1499"
     CASE UPPER(typ)="BUDOVA"
          replace k WITH "1500"
     CASE "SEKTOR" $ UPPER(typ)
          replace k WITH "14"+PADL(ALLTRIM(STR(C)),2,"0")  
          C=C+1        
  ENDCASE
ENDSCAN 
LOCATE FOR "CHODN"$UPPER(typ)
IF NOT FOUND()
	APPEND BLANK 
	replace id WITH 108
	replace typ WITH "CHODNÍK"
	replace k WITH "1499"
	replace farba WITH "#778899"
ENDIF 
LOCATE FOR "BUDOVA"$UPPER(typ)
IF NOT FOUND()
	APPEND BLANK 
	replace id WITH 109
	replace typ WITH "BUDOVA"
	replace k WITH "1500"
	replace farba WITH "#696969"
ENDIF 
*IF FILE(cFarby_P)
SELECT 3
USE Farby_P
* ELSE
   
*ENDIF 

lVynechaj=.F.

SELECT 1
lZdroj=FOPEN(cPm)
*suspend
DO WHILE .not.FEOF(lZdroj)
   cRiadok=FGETS(lZdroj)
   Riadok(1)=cRiadok
   DO CASE
      CASE "&O 21" $ cRiadok OR "&O 11" $ cRiadok
           
           cVrstva=SUBSTR(cRiadok,1)
  
           
      CASE "&O" $ cRiadok
           cVrstva=SUBSTR(cRiadok,4)
           
           SELECT 1
           APPEND FROM ARRAY Riadok
     
      CASE "&A" $ cRiadok
           cAtribut=SUBSTR(cRiadok,4)
           
           SELECT 1
           APPEND FROM ARRAY Riadok           
      
      CASE "&L P " $ cRiadok AND ("K=8" $ cRiadok OR "K=12" $ cRiadok OR "K=7" $ cRiadok) AND "HROBY"$cVrstva
           cRiadok=FGETS(lZdroj)
      
      CASE "&L P " $ cRiadok AND "K=112" $ cRiadok AND "HROBY"$cVrstva && AND "CHODNÍK"$cVrstva
              SELECT Farby_K
              LOCATE FOR VAL(k) = 112
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)            
              Riadok(1)=STRTRAN(cRiadok,"K=112","K=112 P="+cP)
              SELECT 1
              APPEND FROM ARRAY Riadok           

      CASE "&L P " $ cRiadok AND "K=113" $ cRiadok AND "HROBY"$cVrstva && AND "CHODNÍK"$cVrstva
              SELECT Farby_K
              LOCATE FOR VAL(k) = 113
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)            
              Riadok(1)=STRTRAN(cRiadok,"K=113","K=113 P="+cP)
              SELECT 1
              APPEND FROM ARRAY Riadok         
      
      CASE "&L P " $ cRiadok AND "K=1" $ cRiadok AND "HROBY"$cVrstva
           *SUSPEND
           GOTO BOTTOM
           IF "hrob" $ riadok
              SELECT Farby_K
              LOCATE FOR VAL(k) = 1
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)
              Riadok(1)=STRTRAN(cRiadok,"K=1","K=1 P="+cP)
              SELECT 1
              APPEND FROM ARRAY Riadok
            ELSE
              cRiadok=FGETS(lZdroj)
           ENDIF
      *CASE "&L P " $ cRiadok AND NOT "K="$cRiadok AND "HROBY"$cVrstva AND NOT "S="$cRiadok
      *     Riadok(1)=ALLTRIM(cRiadok)+" K=1 P=15"
      *     APPEND FROM ARRAY Riadok
      CASE "&L P " $ cRiadok AND "K=2"$cRiadok AND "HROBY"$cVrstva
              SELECT Farby_K
              LOCATE FOR VAL(k) = 2
              cFarba = ALLTRIM(farby_k.farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)      
           Riadok(1)=STRTRAN(cRiadok,"K=2","K=2 P="+cP) && P="+cP) P=6
           SELECT 1
           APPEND FROM ARRAY Riadok           

      CASE "&L P " $ cRiadok AND "K=6" $ cRiadok AND "HROBY"$cVrstva
              SELECT Farby_K
              LOCATE FOR VAL(k) = 6
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)            
           Riadok(1)=STRTRAN(cRiadok,"K=6","K=6 P="+cP)  && P="+cP) P=68
           SELECT 1
           APPEND FROM ARRAY Riadok           

      CASE "&L P " $ cRiadok AND "K=1498" $ cRiadok &&AND "OBVOD"$cVrstva
              SELECT Farby_K
              LOCATE FOR VAL(k) = 1498
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)            
           Riadok(1)=STRTRAN(cRiadok,"K=1498","K=1498 P="+cP)
           SELECT 1
           APPEND FROM ARRAY Riadok           

      CASE "&L P " $ cRiadok AND "K=1499" $ cRiadok && AND "CHODNÍK"$cVrstva
              SELECT Farby_K
              LOCATE FOR VAL(k) = 1499
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)            
           Riadok(1)=STRTRAN(cRiadok,"K=1499","K=1499 P="+cP)
           SELECT 1
           APPEND FROM ARRAY Riadok           

      CASE "&L P " $ cRiadok AND "K=1500" $ cRiadok &&AND "BUDOVA"$cVrstva
              SELECT Farby_K
              LOCATE FOR VAL(k) = 1500
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)            
           Riadok(1)=STRTRAN(cRiadok,"K=1500","K=1500 P="+cP)
           SELECT 1
           APPEND FROM ARRAY Riadok           

      CASE "&L P " $ cRiadok AND VAL(SUBSTR(cRiadok,AT("K=",cRiadok)+2,4)) > 1400 AND  VAL(SUBSTR(cRiadok,AT("K=",cRiadok)+2,4)) < 1498 && "K=14" $ cRiadok &&AND &&"SEKTOR"$UPPER(cAtribut)
              cK = SUBSTR(cRiadok,AT("K=",cRiadok)+2,4)
           *IF VAL(cK) > 1400 AND VAL(cK) < 1498   
              SELECT Farby_K
              cFarba=""
              SCAN FOR "SEKTOR"$UPPER(farby_k.typ)
                 IF VAL(farby_k.k) = VAL(cK) &&"14"+PADL(ALLTRIM(STR(C)),2,"0")
                    cFarba = ALLTRIM(farby_k.farba)
                    EXIT 
                 ENDIF 
              ENDSCAN 
            
           IF NOT EMPTY(cFarba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)            
              Riadok(1)=STRTRAN(cRiadok,"K="+cK,"K="+cK+" P="+cP)
              SELECT 1
              APPEND FROM ARRAY Riadok  
            ELSE 
              IF "SEKTORY"$cVrstva    
                 cRiadok=FGETS(lZdroj)
                 cRiadok=FGETS(lZdroj)
                 cRiadok=FGETS(lZdroj)
                 cRiadok=FGETS(lZdroj)
                 cRiadok=FGETS(lZdroj)                 
              ENDIF       
           ENDIF 

      CASE "&T" $ cRiadok AND "HROBY"$cVrstva
           Riadok(1)=STRTRAN(cRiadok,"F=2 H=2","F=12 H=3")
           SELECT 1
           APPEND FROM ARRAY Riadok 

      CASE "VSTUP" $ UPPER(cRiadok)
           Riadok(1)="&O VCHOD 1"
           SELECT 1
           APPEND FROM ARRAY Riadok
           Riadok(1)=LEFT(cRiadok,AT(" ",cRiadok,3))+"'VSTUP' D=8 F=3 H=4"
           APPEND FROM ARRAY Riadok
      
      CASE "&T" $ cRiadok AND "&O 11" $ cVrstva

           IF NOT ("chodn"$cRiadok OR "kamen"$cRiadok OR "zámk"$cRiadok OR "bet"$cRiadok OR "cesta"$cRiadok OR "dl"$cRiadok OR "asf"$cRiadok)
              Riadok(1)="&O TEXT 1"
           	  SELECT 1
           	  APPEND FROM ARRAY Riadok
           	  cUhol=SUBSTR(cRiadok,AT("U=",cRiadok),8)
              Riadok(1)=SUBSTR(cRiadok,1,AT("D=",cRiadok)-1)+" D=8 F=13 H=5 "+cUhol
              APPEND FROM ARRAY Riadok  
		   ENDIF 
      CASE "&T" $ cRiadok AND "12" $ cVrstva
           Riadok(1)=SUBSTR(cRiadok,1,AT("'",cRiadok,2))+" D=2 F=12 H=7"
           SELECT 1
           APPEND FROM ARRAY Riadok                                                                               

      CASE "   L " $ cRiadok AND NOT "OBVOD"$cVrstva
           Riadok(1)=STRTRAN(cRiadok," L","NL")
           SELECT 1
           APPEND FROM ARRAY Riadok

      CASE NOT ("S=112" $ cRiadok OR "S=113" $ cRiadok OR "S=114" $ cRiadok) AND "S=" $ cRiadok &&"S=506" $ cRiadok OR "S=128" $ cRiadok OR "S=138" $ cRiadok OR "S=35" $ cRiadok OR "S=36" $ cRiadok OR "S=498" $ cRiadok OR "S=101" $ cRiadok OR "S=128" $ cRiadok OR "S=51" $ cRiadok OR "S=154" $ cRiadok
           
         DO CASE 
           CASE "S=171" $ cRiadok &&AND "21"$cVrstva
              Riadok(1)="&O VCHOD 1"
           		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5) + " M=3"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                    
           
           CASE "S=498" $ cRiadok OR "S=499" $ cRiadok OR "S=506" $ cRiadok OR "S=220" $ cRiadok 
              Riadok(1)="&O SVETLO 1"

          		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5) + " M=2.5"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                    
                      
           CASE "S=51" $ cRiadok OR "S=50" $ cRiadok 
              Riadok(1)="&O KRIZ 1"

          		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5) + " M=5"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                    
                           
           CASE "S=128" $ cRiadok OR "S=129" $ cRiadok 
              Riadok(1)="&O VODA 1"
            
          		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5) + " M=3"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                    

           CASE "S=35" $ cRiadok OR "S=36" $ cRiadok OR "S=39" $ cRiadok 
              Riadok(1)="&O ZELEN 1"
 
          		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5) + " M=2"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                     
                             
           CASE "S=211" $ cRiadok 
              Riadok(1)="&O LAVICKA 1"

          		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5) + " M=1"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                    
              
           CASE "S=138" $ cRiadok OR "S=139" $ cRiadok OR "S=115" $ cRiadok OR "S=154" $ cRiadok
              Riadok(1)="&O KANAL 1"

          		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(STRTRAN(cRiadok,"S=138","S=139"),5) + " M=2.5"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(STRTRAN(cRiadok,"S=138","S=139"),5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                    
            
           CASE "S=101" $ cRiadok 
              Riadok(1)="&O TABULA 1"
 
           		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5) + " M=3"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                    
             
           CASE "S=58" $ cRiadok 
              Riadok(1)="&O POMNIK 1"

          		SELECT 1
           		APPEND FROM ARRAY Riadok                                    

           		IF NOT " M=" $ cRiadok
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5) + " M=3"
            	ELSE
              		Riadok(1)="&L P"+SUBSTR(cRiadok,5)
           		ENDIF   
           
           		APPEND FROM ARRAY Riadok                    
           
           OTHERWISE 
               *cRiadok=FGETS(lZdroj)
              
         ENDCASE                                                      
                            
      CASE ("S=112" $ cRiadok OR "S=113" $ cRiadok OR "S=114" $ cRiadok) AND "HROBY"$cVrstva
        *suspend
        cYX=SUBSTR(cRiadok,6)
        nY=VAL(LEFT(cYX,AT(" ",cYX)-1))
        nX=VAL(SUBSTR(cYX,AT(" ",cYX)+1,AT(" ",cYX,2)-1-AT(" ",cYX)))
        cY1=STR(nY+0.36,9,2)
        cX1=STR(nX+0.36,10,2)
        DO CASE 
           CASE "S=112" $ cRiadok
              SELECT Farby_K
              LOCATE FOR VAL(k) = 112
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)                     
              Riadok(1)="&L P "+cY1+" "+cX1+" "+"K=1 P=109"  &&+cP
           
           CASE "S=113" $ cRiadok
			  SELECT Farby_K
              LOCATE FOR VAL(k) = 113           
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)                 
              Riadok(1)="&L P "+cY1+" "+cX1+" "+"K=1 P="+cP
          
           CASE "S=114" $ cRiadok
			  SELECT Farby_K
              LOCATE FOR VAL(k) = 114
              cFarba = ALLTRIM(farba)
              SELECT Farby_P
              LOCATE FOR ALLTRIM(farba) = cFarba
              cP = ALLTRIM(farby_p.p)           
              Riadok(1)="&L P "+cY1+" "+cX1+" "+"K=1 P=68"  &&+cP  
        ENDCASE
        
        SELECT 1
        APPEND FROM ARRAY Riadok
        cY2=STR(nY+0.36,9,2)
        cX2=STR(nX-0.36,10,2)
        Riadok(1)="  NL "+cY2+" "+cX2
        APPEND FROM ARRAY Riadok
        cY3=STR(nY-0.36,9,2)
        cX3=STR(nX-0.36,10,2)
        Riadok(1)="  NL "+cY3+" "+cX3
        APPEND FROM ARRAY Riadok
        cY4=STR(nY-0.36,9,2)
        cX4=STR(nX+0.36,10,2)
        Riadok(1)="  NL "+cY4+" "+cX4
        APPEND FROM ARRAY Riadok
        Riadok(1)="  NL "+cY1+" "+cX1
        APPEND FROM ARRAY Riadok

      OTHERWISE
        SELECT 1
        APPEND FROM ARRAY Riadok
   ENDCASE
   
ENDDO
FCLOSE (lZdroj)
IF FILE(cPmU)
   DELETE FILE &cPmU
ENDIF
COPY TO &cPmU TYPE SDF

CLOSE ALL
=MESSAGEBOX("Hotovo")
RETURN



