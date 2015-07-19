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
return
end subroutine AllocateInpArray

!=========================================================
!=========================================================


end Module Array_mod
