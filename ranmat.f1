!***********************************************************
!*   Construct a matrix A of rank nr whose elements are    *
!*   random deviates between -1 and +1                     *
!***********************************************************

	SUBROUTINE ranmat(nr, A, iseed)
	
!	   This routine uses SLARUV from the LAPACK library
		
!	   ISEED is INTEGER array, dimension (4) used by SLARUV
!          On entry, the seed of the random number generator; the array
!          elements must be between 0 and 4095, and ISEED(4) must be
!          odd.  On exit, the seed is updated.
	
	IMPLICIT NONE
	
	INTEGER nr, n
	INTEGER :: i, j, k
        INTEGER :: ISEED(4) 

        REAL, ALLOCATABLE, DIMENSION(:) :: X
		 
	REAL A(nr,nr)
	 
	 n = nr*nr
	 
	 ALLOCATE ( X(n) )	
 	 	 
         CALL SLARUV( ISEED, N, X )   ! On exit X contains N uniform deviates on 0-1 
	 
	 k = 0
	 
	 do i = 1, nr
	   do j = 1, nr
	     k = k + 1
	     A(i,j) = 2.0*X(k) - 1.0
	   enddo
	 enddo
	 	
	DEALLOCATE( X )
		 
	END SUBROUTINE ranmat

	 
	
