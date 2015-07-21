Module analysis
	use para_mod
	use array_mod
	implicit none

	contains
!===================================================
!===================================================

subroutine InterSingletFission
	implicit none
	integer :: i,igroup,j,k
	integer :: link1(noperators/2),link2(noperators/2)
	logical :: fullfill
	
	write(*,*) "orbital specific integral"

	do igroup=1,ngroups,1
		do i=1,noutputterm,1
			if(outputterm(i).ntermoprs==0) then
				fullfill=.true.
				do j=1,outputterm(i).ndeltas,1
					do k=1,noperators,1
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
					write(*,*) igroup
					do j=1,outputterm(i).ndeltas,1
						if(groupinfo(3,link1(j),igroup)=='r') then
							groupinfo(3,link1(j),igroup)=groupinfo(3,link2(j),igroup)
							write(*,*) j,groupinfo(3,link1(j),igroup)
						else if(groupinfo(3,link2(j),igroup)=='r') then
							groupinfo(3,link2(j),igroup)=groupinfo(3,link1(j),igroup)
							write(*,*) j,groupinfo(3,link2(j),igroup)
						end if
					end do
				end if
			end if
		end do
	end do

return
end subroutine InterSingletFission

!===================================================
!===================================================
end module
