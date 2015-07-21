Module StandardIO
	use para_mod
	use array_mod
	implicit none

	contains
!====================================================
!====================================================

subroutine PrintOutPutTerm
	implicit none
	
	integer :: i,j

	do i=1,noutputterm,1
		write(*,*) "term:",i
		write(*,*) "sign:",outputterm(i).sign1
		write(*,*) "delta pairs:" 
		do j=1,outputterm(i).ndeltas,1
			write(*,*) outputterm(i).deltapair(:,j)
		end do
		write(*,*) ":normal operator lines:" 
		do j=1,outputterm(i).ntermoprs,1
			write(*,*) outputterm(i).oprline(:,j)
		end do
		write(*,*) "=============================="
	end do
return
end subroutine PrintOutPutTerm

!====================================================
!====================================================

end Module StandardIO
