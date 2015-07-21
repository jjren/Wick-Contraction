Module Array_mod
	
	use para_mod
	implicit none

	type term
		integer(kind=4) :: sign1
		integer(kind=4) :: ndeltas
		integer(kind=4) :: ntermoprs
		character(len=1) :: deltapair(3,maxnpairs)
		character(len=1) :: oprline(3,maxnoperators)
	end type term

	character(len=1),allocatable :: opr(:,:)
	character(len=2),allocatable :: groupinfo(:,:,:)
	type(term),allocatable :: outputterm(:),workinterm(:),workoutterm(:)

contains
!=========================================================
!=========================================================

subroutine AllocateInpArray
	implicit none

	allocate(opr(3,noperators))
	allocate(outputterm(maxnterms))
	allocate(workinterm(maxnterms))
	allocate(workoutterm(maxnterms))
	allocate(groupinfo(4,noperators,maxngroups))
	! 1 is the operator subscript i,j,k,l
	! 2 is the operator orbital o,u,r
	! 3 is the operator orbital H1,L1...
	! 4 is the spin u(p) and d(own)
return
end subroutine AllocateInpArray

!=========================================================
!=========================================================

subroutine DeallocateArray
	implicit none

	deallocate(opr)
	deallocate(outputterm)
	deallocate(workinterm)
	deallocate(workoutterm)
return
end subroutine DeallocateArray

!=========================================================
!=========================================================

end Module Array_mod
