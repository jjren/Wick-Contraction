subroutine ReadInput
! this subroutine read the input
	
	use para_mod
	use array_mod

	implicit none

	open(unit=10,file="inp",status="old")

	read(10,*) noperators

	call AllocateInpArray
	
	read(10,*) opr(1,1:noperators)  ! the operator subscript i,j,k,l....
	read(10,*) opr(2,1:noperators)  ! the c(reation),a(nnihilation)
	read(10,*) opr(3,1:noperators)  ! the o(ccupation space),u(occupation space),r(andom)
	
	close(10)

return

end subroutine ReadInput


	

