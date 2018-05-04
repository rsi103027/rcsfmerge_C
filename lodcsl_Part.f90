!***********************************************************************
!                                                                      *
      SUBROUTINE LODCSL_Part(CSF_Number,N_C) 
!                                                                      *
!   Loads the data from the  .csl  file. A number of checks are made   *
!   to ensure correctness and consistency.                             *
!                                                                      *
!                                                                      *
!   Written by  G. Gaigalas                     Vilnius, May 2016      *
!                                                                      *
!***********************************************************************
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE BLK_C,            only: NBLOCK,NCFBLK
      USE rang_Int_C
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      IMPLICIT NONE
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      INTEGER,  INTENT(OUT)   :: CSF_Number
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      INTEGER            :: I, IOS, NCF, N, N_C, NCORE
      CHARACTER(LEN=256) :: RECORD,RECORD_C_shell,RECORD_C_quant,RECORD_C_coupl
!-----------------------------------------------
!
      CSF_Number = 0
      DO 
         CSF_Number = CSF_Number + 1
         Found_CSF = 1
         IF(CSF_Number .EQ. 1) NCF = NCFBLK(NBLOCK) + 1 
         READ (20, '(A)', IOSTAT=IOS) RECORD 
         IF (IOS == 0) THEN 
            IF (RECORD(1:2) == ' *') RETURN
            RECORD_C_shell = RECORD
!
!   Read the J_sub and v quantum numbers
!
            READ (20, '(A)') RECORD_C_quant
!
!   Read the X, J, and (sign of) P quantum numbers
!
            READ (20, '(A)') RECORD_C_coupl
            do i=1,NCORE
               IF(RECORD_C_shell(1:N_C*9) .eq. C_core(i)(1:N_C*9)) THEN
                  Found_CSF=0
               ENDIF
            enddo
            if(Found_CSF.eq.1)then
                  WRITE(22,'(A)') TRIM(RECORD_C_shell)
                  WRITE(22,'(A)') TRIM(RECORD_C_quant)
                  WRITE(22,'(A)') TRIM(RECORD_C_coupl)
            endif
         ELSE
            RETURN
         ENDIF 
      END DO
      RETURN  
!
      END SUBROUTINE LODCSL_Part
