!***********************************************************
!*   Construct a matrix A of rank nr whose elements are    *
!*   random deviates between -1 and +1                     *
!***********************************************************

	SUBROUTINE matran(nr, A, eps, iseed)
	
!	   This routine uses SLARUV from the LAPACK library
		
!	   ISEED is INTEGER array, dimension (4) used by SLARUV
!          On entry, the seed of the random number generator; the 
!          elements of ISEED must be between 0 and 4095, and ISEED(4) must be
!          odd.  On exit, the seed is updated.

!          eps is a small perturbation e.g. 0.001
!          On exit A is modified by small random additions
	
	IMPLICIT NONE
	
	INTEGER nr, n
	INTEGER :: i, j, k
        INTEGER :: ISEED(4) 

        REAL, ALLOCATABLE, DIMENSION(:) :: X
		 
	REAL A(nr,nr), matr(nr,nr), eps
	 
	 n = nr*nr
	 
	 ALLOCATE ( X(n) )	
 	 	 
         CALL SLARUV( ISEED, N, X )   ! On exit X contains N uniform deviates on 0-1 
	 
	 k = 0
	 
	 do i = 1, nr                 ! random matrix on -1/+1
	   do j = 1, nr
	     k = k + 1
	     matr(i,j) = 2.0*X(k) - 1.0
	   enddo
	 enddo
	 	   
	 do i = 1, nr         ! modify matrix by scaled random values
	   do j = 1, nr
	     A(i,j) =A(i,j)*( 1.0 + eps*matr(i,j))
	   enddo
	 enddo

	 	
	DEALLOCATE( X )
		 
	END SUBROUTINE matran

	 
	
