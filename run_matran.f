	PROGRAM run
	
	IMPLICIT NONE
	INTEGER :: ISEED(4) = (/1056, 3887, 2655, 2345/)
	real mat(6,6)
	INTEGER nr
	
	nr = 6 
	
	!generate a matrix
	write(*,*) ' trial 1' 

	call ranmat(nr, mat, ISEED)
	
	call wmat(mat, nr)
	
	write(*,*) ' ' 

	call matran(nr, mat, 0.001, ISEED)
	
	call wmat(mat, nr)
	
	write(*,*) ' '
	
	write(*,*) ' trial 2' 

	call ranmat(nr, mat, ISEED)
	
	mat = 1000.0*mat
	
	call wmat(mat, nr)
	
	write(*,*) ' ' 

	call matran(nr, mat, 0.001, ISEED)
	
	call wmat(mat, nr)


		
	END PROGRAM run
