Module Para_Mod
	implicit none

	integer(kind=4) :: noperators  ! mod(noperators,2)==0 nup==ndown
	integer,parameter :: maxnpairs=10,maxnoperators=20,maxnterms=10000,maxngroups=200
	
	integer :: noutputterm,nworkinterm,nworkoutterm
	integer :: ngroups
	character(len=1) :: flag

end Module Para_Mod

