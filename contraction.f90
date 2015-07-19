Module Contraction_Mod
	use para_mod
	use array_mod
	implicit none

contains

!==================================================================
!==================================================================

subroutine Contraction
	implicit none
	integer :: i,j
	integer :: maxjterms
	type(term),allocatable :: tmpoutterm(:)
	

	call FirstTerm
	nworkinterm=1
	workinterm(1)=outputterm(1)
	
	do i=1,noperators/2,1  ! contraction npair loop
		maxjterms=(noperators/2+1-i)**2/4
		allocate(tmpoutterm(maxjterms))
		! initiate the workoutterm
		noutputterm=0
		workoutterm(:).ndeltas=-1

		do j=1,nworkinterm,1
			call Contraction_SinglePair(workinterm(j),tmpoutterm,maxjterms,realjterms)
			call Search_Same(tmpoutterm,realjterms)
		end do
		if(noutputerm+nworkoutterm>maxnterms) then
			write(*,*) "exceed maxnterms failed!"
			stop
		end if
		outputterm(noutputterm+1:noutputterm+nworkoutterm)=workoutterm(1:nworkoutterm)
		noutputterm=noutputterm+nworkoutterm
		workinterm(1:nworkinterm)=workoutterm(1:nworkoutterm)
		nworkinterm=nworkoutterm
		deallocate(tmpoutterm)
	end do

return
end subroutine Contraction

!==================================================================
!==================================================================

subroutine Search_Same(tmpoutterm,realjterms)
! search the same between tmpoutterm  and outputerm and update the outputterm
! the tmpoutterm themselves will not be the same always

	implicit none
	
	integer :: realjterms
	type(term) :: tmpoutterm(realjterms)
	! local
	integer :: i,j
	integer :: naddterms
	logical :: iffind,ifpairsame

	naddterms=0

	do i=1,realjterms,1
		iffind=.false.
		do j=1,noutputterm,1
			if(outputterm(j).ndeltas/=tmpoutterm(i).ndeltas) then
				cycle
			else
				ifpairsame=.true.
				do k=1,ndeltas,1
					do l=1,ndeltas,1
						if((outputterm(j).deltapair(1,k)/=tmpoutterm(i).deltapair(1,l)) .or. &
							(outputterm(j).deltapair(2,k)/=tmpoutterm(i).deltapair(2,l))) then
							ifpairsame=.false.
							exit
						end if
					end do
					if(ifpairsame==.false.) then
						exit
					end if
				end do
				if(ifpairsame==.true.) then
					iffind=.true.
					exit
				end do
			end if
		end do
		if(iffind==.false.) then
			naddterms=naddterms+1
			outputterm(noutputterm+naddterms)=tmpoutterm(i)
		end if
	end do
	noutputterm=noutputterm+naddterms

return
end subroutine Search_Same

!==================================================================
!==================================================================

subroutine Contraction_SingletPair(tmpinterm,tmpoutterm,maxjterms,realjterms)
	implicit none
	
	integer :: maxjterms,realjterms
	type(term),intent(in) :: tmpinterm
	type(term),intent(out) :: tmpoutterm(maxjterms)
	! local
	integer :: i,j

	realjterm=0
	do i=1,tmpinterm.ntermoprs,1
	do j=i+1,tmpinterm.ntermoprs,1
		if((tmpinterm.oprline(2,i)=='c' .and. tmpinterm.oprline(3,i)=='u') .or. &
			(tmpinterm.oprline(2,i)=='a' .and. tmpinterm.oprline(3,i)=='o') .or. &
			(tmpinterm.oprline(2,i)==tmpinterm.oprline(2,j)) .or. &
			(tmpinterm.oprline(3,i)/=tmpinterm.oprline(3,j)))  then
			cycle
		else
			realjterms=realjterms+1
			tmpoutterm(realjterms).sign1=tmpinterm.sign1*(-1)**(mod(j-i-1,2))
			tmpoutterm(realjterms).ndeltas=tmpinterm.deltas+1
			tmpoutterm(realjterms).ntermoprs=tmpinterm.ntermoprs-2
			tmpoutterm(realjterms).deltapair=tmpinterm.deltapair
			tmpoutterm(realjterms).deltapair(1,tmpoutterm(realjterms).ndeltas)=i
			tmpoutterm(realjterms).deltapair(2,tmpoutterm(realjterms).ndeltas)=j
			if(tmpinterm.oprline(3,i)/='r') then
				tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)=tmpinterm.oprline(3,i)
			else if(tmpinterm.oprline(3,j)/='r') then
				tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)=tmpinterm.oprline(3,j)
			else
				tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)='r'
			end if
			tmpoutterm(realjterms).oprline(:,1:i-1)=tmpinterm.oprline(:,1:i-1)
			tmpoutterm(realjterms).oprline(:,i:j-2)=tmpinterm.oprline(:,i+1:j)
		end if
	end do
	end do
return
end Subroutine Contraction_SinglePair

!==================================================================
!==================================================================

subroutine FirstTerm
! add the first normal order term
	use para_mod
	use array_mod

	implicit none

	noutputterm=1
	outputterm(1).sign1=1
	outputterm(1).ndeltas=0
	outputterm(1).ntermoprs=noperators
	outputterm(1).oprline(:,1:noperators)=opr(1:3,:)

return
end subroutine FirstTerm

!==================================================================
!==================================================================

end Module Contraction_Mod
