Module analysis
	use para_mod
	use array_mod
	implicit none

	character(len=2),allocatable :: integralterm(:,:,:)
	
	contains
!===================================================
!===================================================

subroutine InterSingletFission
	implicit none
	integer :: i,igroup,j,k
	integer :: link1(noperators/2),link2(noperators/2)
	logical :: fullfill
	
	write(*,*) "orbital specific integral"
	allocate(integralterm(4,noutputterm,ngroups))
	integralterm="no"

	do igroup=1,ngroups,1
		do i=1,noutputterm,1
			if(outputterm(i).ntermoprs==0) then
				fullfill=.true.
				do j=1,outputterm(i).ndeltas,1
					do k=1,noperators,1
						! find the groupinfo terms corresponse the specific delta
						if(outputterm(i).deltapair(1,j)==groupinfo(1,k,igroup)) then
							link1(j)=k
						else if(outputterm(i).deltapair(2,j)==groupinfo(1,k,igroup)) then
							link2(j)=k
						end if
					end do
					if((outputterm(i).deltapair(3,j)/=groupinfo(2,link1(j),igroup) .and. groupinfo(2,link1(j),igroup)/='r') .or. &
						(outputterm(i).deltapair(3,j)/=groupinfo(2,link2(j),igroup) .and. groupinfo(2,link2(j),igroup)/='r')) then
						fullfill=.false.
						exit
					end if
				end do
				if(fullfill==.true.) then
					do j=1,outputterm(i).ndeltas,1
						if((groupinfo(3,link1(j),igroup)/=groupinfo(3,link2(j),igroup) .and. groupinfo(3,link2(j),igroup)/='r' .and. groupinfo(3,link1(j),igroup)/='r') .or. &
							(groupinfo(4,link1(j),igroup)/=groupinfo(4,link2(j),igroup))) then
							fullfill=.false.
							exit
						end if
					end do
				end if
				if(fullfill==.true.) then
					write(*,*) "======================="
					write(*,*) "igroup:",igroup
					write(*,*) "outputterm:",i,"sign:",outputterm(i).sign1
					do j=1,outputterm(i).ndeltas,1
						if(groupinfo(3,link1(j),igroup)=='r' .or. groupinfo(3,link2(j),igroup)=='r') then
							if(groupinfo(3,link1(j),igroup)=='r' .and. groupinfo(3,link2(j),igroup)=='r') then
								write(*,*) groupinfo(1,link1(j),igroup),outputterm(i).deltapair(3,j)
								call pqrssort(groupinfo(1,link1(j),igroup),outputterm(i).deltapair(3,j),i,igroup)
								write(*,*) groupinfo(1,link2(j),igroup),outputterm(i).deltapair(3,j)
								call pqrssort(groupinfo(1,link2(j),igroup),outputterm(i).deltapair(3,j),i,igroup)
							else if(groupinfo(3,link1(j),igroup)/='r') then
								write(*,*) groupinfo(1,link2(j),igroup),groupinfo(3,link1(j),igroup)
								call pqrssort(groupinfo(1,link2(j),igroup),groupinfo(3,link1(j),igroup),i,igroup)
							else if(groupinfo(3,link2(j),igroup)/='r') then
								write(*,*) groupinfo(1,link1(j),igroup),groupinfo(3,link2(j),igroup)
								call pqrssort(groupinfo(1,link1(j),igroup),groupinfo(3,link2(j),igroup),i,igroup)
							end if
						end if
					end do
				end if
			end if
		end do
	end do

	if(flag=='h' .or. flag=='g') then
		call sumintegral
	end if
	deallocate(integralterm)

return
end subroutine InterSingletFission

!===================================================
!===================================================

subroutine pqrssort(oindex,orbital,termindex,igroup)
	implicit none
	character(len=2) :: oindex,orbital
	integer :: termindex,igroup
	
	if(oindex=='p') then
		integralterm(1,termindex,igroup)=orbital
	else if(oindex=='q') then
		integralterm(2,termindex,igroup)=orbital
	else if(oindex=='r') then
		integralterm(3,termindex,igroup)=orbital
	else if(oindex=='s') then
		integralterm(4,termindex,igroup)=orbital
	end if
return
end subroutine pqrssort

!===================================================
!===================================================

subroutine sumintegral
	implicit none
	character(len=2) :: sumintegralterm(4,ngroups*noutputterm)
	integer :: realterms
	real(kind=8) :: sumintegralcoeff(ngroups*noutputterm)
	logical :: ifexist
	integer :: igroup,i,j,k

	realterms=0
	sumintegralcoeff=0.0D0

	do igroup=1,ngroups,1
	do j=1,noutputterm,1
		if(integralterm(1,j,igroup)/='no') then
			ifexist=.false.
			do k=1,realterms,1
				if(flag=='h') then
					if((sumintegralterm(1,k)==integralterm(1,j,igroup) &
						.and. sumintegralterm(2,k)==integralterm(2,j,igroup)) .or. &
						(sumintegralterm(1,k)==integralterm(2,j,igroup) &
						.and. sumintegralterm(2,k)==integralterm(1,j,igroup))) then
						ifexist=.true.
						exit
					end if
				else if(flag=='g') then
					if (  &  
						! combine the same two electron integral terms
						(((sumintegralterm(1,k)==integralterm(1,j,igroup) &
						.and. sumintegralterm(4,k)==integralterm(4,j,igroup)) .or. & 
						(sumintegralterm(1,k)==integralterm(4,j,igroup) &
						.and. sumintegralterm(4,k)==integralterm(1,j,igroup))) .and. & 
						((sumintegralterm(3,k)==integralterm(3,j,igroup) &
						.and. sumintegralterm(2,k)==integralterm(2,j,igroup)) .or. &
						(sumintegralterm(3,k)==integralterm(2,j,igroup) &
						.and. sumintegralterm(2,k)==integralterm(3,j,igroup)))) &
						 				.or. &
						(((sumintegralterm(1,k)==integralterm(3,j,igroup) &
						.and. sumintegralterm(4,k)==integralterm(2,j,igroup)) .or. & 
						(sumintegralterm(4,k)==integralterm(3,j,igroup) &
						.and. sumintegralterm(1,k)==integralterm(2,j,igroup))) .and. & 
						((sumintegralterm(2,k)==integralterm(1,j,igroup) &
						.and. sumintegralterm(3,k)==integralterm(4,j,igroup)) .or. &
						(sumintegralterm(3,k)==integralterm(4,j,igroup) &
						.and. sumintegralterm(2,k)==integralterm(1,j,igroup)))) &
										) then
						ifexist=.true.
						exit
					end if
				end if
			end do
			if(ifexist==.true.) then
				sumintegralcoeff(k)=sumintegralcoeff(k)+groupcoeff(igroup)*DBLE(outputterm(j).sign1)
			else
				realterms=realterms+1
				sumintegralterm(:,realterms)=integralterm(:,j,igroup)
				sumintegralcoeff(realterms)=groupcoeff(igroup)*DBLE(outputterm(j).sign1)
			end if
		end if
	end do
	end do
	
	write(*,*) "============================================================"
	write(*,*) "in the two electron term do not forget about the 1/2 factor!"
	write(*,*) "============================================================"
	do i=1,realterms,1
		write(*,*) "==========================="
		write(*,*) "integral term",i
		write(*,*) sumintegralcoeff(i)
		if(flag=='g') then
			write(*,*) "(",sumintegralterm(1,i),sumintegralterm(4,i),"|",sumintegralterm(2,i),sumintegralterm(3,i),")"
		else if(flag=='h') then
			write(*,*) "(",sumintegralterm(1,i),"|",sumintegralterm(2,i),")"
		end if
	end do

return
end subroutine sumintegral
!===================================================
!===================================================
end module
