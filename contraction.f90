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
	integer :: maxjterms,realjterms
	type(term),allocatable :: tmpoutterm(:)
	

	call FirstTerm
	nworkinterm=1
	workinterm(1)=outputterm(1)
	
	do i=1,noperators/2,1  ! contraction npair loop
		! max contraction terms
		maxjterms=(noperators/2+1-i)**2
		allocate(tmpoutterm(maxjterms))
		! initiate the workoutterm
		nworkoutterm=0
		workoutterm(:).ndeltas=-1

		do j=1,nworkinterm,1
			call Contraction_SinglePair(workinterm(j),tmpoutterm,maxjterms,realjterms)
			call Search_Same(tmpoutterm,realjterms)
		end do
		if(noutputterm+nworkoutterm>maxnterms) then
			write(*,*) "exceed maxnterms failed!"
			stop
		end if
		outputterm(noutputterm+1:noutputterm+nworkoutterm)=workoutterm(1:nworkoutterm)
		noutputterm=noutputterm+nworkoutterm
		nworkinterm=nworkoutterm
		workinterm(1:nworkinterm)=workoutterm(1:nworkoutterm)
		deallocate(tmpoutterm)
	end do

return
end subroutine Contraction

!==================================================================
!==================================================================

subroutine Search_Same(tmpoutterm,realjterms)
! search the same between tmpoutterm  and workoutterm and update the workoutterm
! the tmpoutterm themselves will not be the same always

	implicit none
	
	integer :: realjterms
	type(term) :: tmpoutterm(realjterms)
	! local
	integer :: i,j,k,l
	integer :: naddterms
	logical :: iffind,ifpairsame

	naddterms=0

	! different terms
	do i=1,realjterms,1
		iffind=.false.
		! in every term
		do j=1,nworkoutterm,1
			do k=1,tmpoutterm(i).ndeltas,1
				ifpairsame=.false.
				! in fact tmpoutterm(i).ndeltas/=workoutterm(j).ndeltas is useless
				! but in workoutterm all ndeltas is the same : at the same contraction level
				do l=1,workoutterm(j).ndeltas,1
					! the delta pair is not the same
					! the small operator index is always in the front
					if((workoutterm(j).deltapair(1,l)==tmpoutterm(i).deltapair(1,k)) .and. &
						(workoutterm(j).deltapair(2,l)==tmpoutterm(i).deltapair(2,k))) then
						ifpairsame=.true.
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
			end if
		end do
		if(iffind==.false.) then
			naddterms=naddterms+1
			workoutterm(nworkoutterm+naddterms)=tmpoutterm(i)
		end if
	end do
	! the terms from one source should not be the same
	nworkoutterm=nworkoutterm+naddterms

return
end subroutine Search_Same

!==================================================================
!==================================================================

subroutine Contraction_SinglePair(tmpinterm,tmpoutterm,maxjterms,realjterms)
	implicit none
	
	integer :: maxjterms,realjterms
	type(term),intent(in) :: tmpinterm
	type(term),intent(out) :: tmpoutterm(maxjterms)
	! local
	integer :: i,j

	realjterms=0
	do i=1,tmpinterm.ntermoprs,1
	do j=i+1,tmpinterm.ntermoprs,1
		if((tmpinterm.oprline(2,i)=='c' .and. tmpinterm.oprline(3,i)=='u') .or. &
			(tmpinterm.oprline(2,i)=='a' .and. tmpinterm.oprline(3,i)=='o') .or. &
			(tmpinterm.oprline(2,i)==tmpinterm.oprline(2,j)) .or. &
			(tmpinterm.oprline(3,i)/=tmpinterm.oprline(3,j) .and. tmpinterm.oprline(3,i)/='r' .and. tmpinterm.oprline(3,j)/='r'))  then
			! these terms contraction is zero
			cycle
		else
			realjterms=realjterms+1
			tmpoutterm(realjterms).sign1=tmpinterm.sign1*(-1)**(mod(j-i-1,2))
			tmpoutterm(realjterms).ndeltas=tmpinterm.ndeltas + 1
			tmpoutterm(realjterms).ntermoprs=tmpinterm.ntermoprs - 2
			tmpoutterm(realjterms).deltapair=tmpinterm.deltapair
			tmpoutterm(realjterms).deltapair(1,tmpoutterm(realjterms).ndeltas)=tmpinterm.oprline(1,i)
			tmpoutterm(realjterms).deltapair(2,tmpoutterm(realjterms).ndeltas)=tmpinterm.oprline(1,j)
			
			! one version
			if(tmpinterm.oprline(3,i)/='r') then
				tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)=tmpinterm.oprline(3,i)
			else if(tmpinterm.oprline(3,j)/='r') then
				tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)=tmpinterm.oprline(3,j)
			else  
				! (i,j) pair are both random
				if(tmpinterm.oprline(2,i)=='c') then
					tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)='o'
				else
					tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)='u'
				end if
			end if
			! another version
		!	if(tmpinterm.oprline(2,i)=='c') then
		!		tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)='o'
		!	else
		!		tmpoutterm(realjterms).deltapair(3,tmpoutterm(realjterms).ndeltas)='u'
		!	end if

			tmpoutterm(realjterms).oprline(:,1:i-1)=tmpinterm.oprline(:,1:i-1)
			tmpoutterm(realjterms).oprline(:,i:j-2)=tmpinterm.oprline(:,i+1:j-1)
			tmpoutterm(realjterms).oprline(:,j-1:tmpinterm.ntermoprs-2)=tmpinterm.oprline(:,j+1:tmpinterm.ntermoprs)
		end if
	end do
	end do
return
end Subroutine Contraction_SinglePair

!==================================================================
!==================================================================

subroutine FirstTerm
! add the first normal order term

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
