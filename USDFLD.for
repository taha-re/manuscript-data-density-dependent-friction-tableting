C ======================================================
C this header should never change
C ======================================================
      SUBROUTINE USDFLD(FIELD,STATEV,PNEWDT,DIRECT,T,CELENT,
     1 TIME,DTIME,CMNAME,ORNAME,NFIELD,NSTATV,NOEL,NPT,LAYER,
     2 KSPT,KSTEP,KINC,NDI,NSHR,COORD,JMAC,JMATYP,MATLAYO,LACCFLA)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME,ORNAME
      CHARACTER*3 FLGRAY(15)
      DIMENSION FIELD(NFIELD),STATEV(NSTATV),DIRECT(3,3),T(3,3),TIME(2)
      DIMENSION ARRAY(15),JARRAY(15),JMAC(*),JMATYP(*),COORD(*)
C ======================================================
C end of the header
C ======================================================
C Set the initial relative density
      RD0=0.3411
        
C Get the plastic volume strain
C 'PEQC' gives all equivalent plastic strains for a cap model
      CALL GETVRM('PEQC',ARRAY,JARRAY,FLGRAY,JRCD,JMAC,JMATYP,MATLAYO,
     1 LACCFLA)

C Total volumetric inelastic strain
      E_VOL = -ARRAY(4)

C Calculate the relative density and assign it to field variable
      FIELD(1) = RD0*EXP(E_VOL)
      
      IF(FIELD(1)>1.0)THEN
      STATEV(1)=1.0
      ELSE
      STATEV(1) = FIELD(1)
      ENDIF
      
C If error, write comment to .DAT file:
      IF(JRCD.NE.0)THEN
      WRITE(6,*) 'REQUEST ERROR IN USDFLD FOR ELEMENT NUMBER ',
     1 NOEL,'INTEGRATION POINT NUMBER ',NPT
      ENDIF
C
      RETURN
      END
