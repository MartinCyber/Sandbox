	SUBROUTINE wmat(mat, np)
	
C **   Write out a matrix

	INTEGER np, i, j

	REAL ::  mat(np, np)
	
	do i = 1,np
	   write(*,100) (mat(i,j), j= 1,np)
	 enddo
	 
  100   FORMAT(1x, 10( F12.4 , 1X))
  
  	END SUBROUTINE wmat
  
