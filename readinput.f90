subroutine ReadInput
! this subroutine read the input
	
	use para_mod
	use array_mod

	implicit none
	
	integer :: i,j

	open(unit=10,file="inp",status="old")

	read(10,*) noperators

	call AllocateInpArray
	
	read(10,*) opr(1,1:noperators)  ! the operator subscript i,j,k,l....
	read(10,*) opr(2,1:noperators)  ! the c(reation),a(nnihilation)
	read(10,*) opr(3,1:noperators)  ! the o(ccupation space),u(occupation space),r(andom)

	read(10,*) flag
	if(flag=='h' .or. flag=='g' .or. flag=='s') then ! specific
		read(10,*) ngroups
		do i=1,ngroups,1
			read(10,*) groupcoeff(i)
			do j=1,noperators,1
				read(10,*) groupinfo(:,j,i)
			end do
		end do
	end if

	close(10)
	
	write(*,*) "====================="
	write(*,*) "input operators array"
	do i=1,noperators,1
		write(*,*) opr(:,i)
	end do
	write(*,*) "====================="

return

end subroutine ReadInput


	

