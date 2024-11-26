//*                                                                   
//*------------------------------------------------------------------*
//* Author: Dirceu Ivo (contato@bimonti.com.br)                      |
//*   Date: 26-Nov-2024                                              |
//*  Descr: Sample mainframe modernization project.                  |
//*   Inst: Compile and run.                                         |
//EXPORT   EXPORT SYMLIST=*                                          |
//*------------------------------------------------------------------*
//*                                                                   
//*------------------------------------------------------------------*
//* CLEARFIL | Clear files for restart.                    | RC:0000 |
//*------------------------------------------------------------------*
//CLEARFIL EXEC PGM=IDCAMS                                              
//SYSPRINT DD SYSOUT=*                                                  
//SYSIN    DD *,SYMBOLS=JCLONLY                                         
  DELETE '&SYSUID..SYSMDUMP'                                            
  DELETE '&SYSUID..PGMLIB'                                              
  SET MAXCC=0                                                           
/*                                                                      
//*                                                                   
//*------------------------------------------------------------------*
//* ASMCP    | Assemble and link-edit procedure.           | RC:0000 |
//*------------------------------------------------------------------*
//ASMCP    PROC MBR=*,CPARM=NODECK,LPARM=,                              
//         PROGLIB=&SYSUID..PGMLIB                                      
//C        EXEC PGM=ASMA90,                                             
//         PARM='OBJ,XREF(SHORT),USING(NOWARN),LC(55),&CPARM'           
//SYSLIB   DD DISP=SHR,DSN=SYS1.MACLIB                                  
//         DD DISP=SHR,DSN=SYS1.MODGEN                                  
//SYSPUNCH DD DUMMY                                                     
//SYSPRINT DD SYSOUT=*                                                  
//SYSUT1   DD SPACE=(CYL,(10,5))                                        
//SYSLIN   DD DISP=(,PASS),SPACE=(CYL,(10,5))                           
//L        EXEC PGM=IEWL,COND=(5,LT,C),                                 
//         PARM='MAP,CALL,LET,LIST,AMODE=31,RMODE=ANY,&LPARM'           
//SYSPRINT DD SYSOUT=*                                                  
//SYSUT1   DD SPACE=(CYL,(10,5))                                        
//SYSLIB   DD DISP=SHR,DSN=SYS1.CSSLIB                                  
//SYSLIN   DD DISP=(SHR,DELETE),DSN=*.C.SYSLIN                          
//SYSLMOD  DD DISP=SHR,DSN=&PROGLIB(&MBR)                               
//PEND     PEND                                                         
//*                                                                   
//*------------------------------------------------------------------*
//* ALLOCPGM | Allocate a new PGMLIB.                      | RC:0000 |
//*------------------------------------------------------------------*
//ALLOCPGM EXEC PGM=IDCAMS                 
//SYSPRINT DD SYSOUT=*                                  
//SYSIN    DD *,SYMBOLS=JCLONLY                         
 ALLOCATE -                                             
     DSNAME('&SYSUID..PGMLIB') -                        
     FILE(PGMLIB)              -                        
     VOL(VPDFSB)               -                        
     UNIT(SYSALLDA)            -                        
     RECFM(U)                  -                        
     BLKSIZE(32760)            -                        
     DSORG(PO)                 -                        
     DSNTYPE(PDS)              -                        
     DIR(100)                  -                        
     NEW CATALOG               -                        
     SPACE(1,1) CYL                                     
/*                                                      
//*                                                                   
//*------------------------------------------------------------------*
//* COMPSRCE | Compile and link the source program.        | RC:0000 |
//*------------------------------------------------------------------*
SAMPLE   CSECT                                                          
SAMPLE   AMODE 31                                                       
MAIN     DS    0F                         Start address                       
*                                                                       
* Basic program structure and initialization                            
         SAVE  (14,12)                    Save area                           
         BASR  6,0                        Base register                       
         USING *,6                        Addressability                      
         LA    2,SAVEAREA                 Address of save area                
         ST    2,8(,13)                   Forward chain                       
         ST    13,SAVEAREA+4              Backward chain caller               
         LR    13,2                       Register 13 for sub calls           
*                                                                       
* Main program instructions                                             
         CALL  BPX1EXC,                                                X
               (PATHNL,                                                x
               PATHN,                                                  x
               ARGCNT,                                                 x
               ARGLSLL,                                                x
               ARGLST,                                                 x
               ENVCNT,                                                 x
               ENVCNLL,                                                x
               ENVCNL,                                                 x
               EXITRA,                                                 x
               EXITPLA,                                                x
               RTNVAL,                                                 X
               RTNCOD,                                                 X
               RSNCOD),                                                X
               VL,MF=(E,PARMLST)                                        
EXITRTN  EQU   *                                                        
         MVC   RTNVALW+22(L'RTNVAL),RTNVAL                              
RTNVALW  WTO   'Return value:                     '                     
         B     ENDPGM                     End the program                     
*                                                                       
* Basic program termination routine                                     
ENDPGM   EQU   *                                                        
         L     13,SAVEAREA+4              Point at old savearea               
         LM    14,12,12(13)               Restore the registers               
         RETURN (14,12),RC=0              Return Code                         
*                                                                       
* Data definition section                                               
         LTORG                                                          
PATHN    DC    CL7'/bin/sh'               Pathname                            
PATHNL   DC    A(L'PATHN)                 Length of pathname                  
ARGCNT   DC    F'1'                       Number of auguments                 
ARG001   DC    CL20'sh python3 --version' Program argument                              
ARGLSTL  DC    A(L'ARG001)                Length of argument                  
ARGLST   DC    A(ARG001)                  Addr of argument list               
ARGLSLL  DC    A(ARGLSTL)                 Addr of arg len list                
ENVCNT   DC    F'0'                       Env var count                       
ENVCNL   DC    F'0'                       Env var arg list addr               
ENVCNLL  DC    F'0'                       Env var arg len addr                
EXITRA   DC    A(EXITRTN)                 Exit routine addr                   
EXITPLA  DC    F'0'                       Exit rout parm list addr            
RTNVAL   DS    F                          Return value                        
RTNCOD   DS    F                          Return code                         
RSNCOD   DS    F                          Reason code                         
PARMLST  DS    13A                        Parameter list                      
SAVEAREA DS    18F                        Save area definition                
         END   SAMPLE                     End the program                     
/*                                                                      
//*                                                                   
//*------------------------------------------------------------------*
//* EXECPGML | Execute the program.                        | RC:0000 |
//*------------------------------------------------------------------*
//EXECPGML EXEC PGM=SAMPASM                           
//STEPLIB  DD DISP=SHR,DSN=&SYSUID..PGMLIB            
//SYSPRINT DD SYSOUT=*                                
//SYSOUT   DD SYSOUT=*                                
//SYSMDUMP DD DISP=(NEW,CATLG,),DSN=&SYSUID..SYSMDUMP,
//         SPACE=(CYL,(10,10),RLSE),UNIT=SYSDA,       
//         DCB=RECFM=FB,LRECL=4160,BLKSIZE=4160       
