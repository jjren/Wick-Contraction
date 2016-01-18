program creatinp
	implicit none
	
	integer :: ntermL,ntermR,itermL,itermR
	character(len=2),allocatable :: oprL(:,:,:),oprR(:,:,:)
	real(kind=8),allocatable :: coeffL(:),coeffR(:)

	open(unit=10,file="inp.inp",status="old")
	read(10,*) ntermL,ntermR
	read(10,*) itermL,itermR
	close(10)
	
	allocate(oprL(4,itermL,ntermL))
	allocate(oprR(4,itermR,ntermR))
	allocate(coeffL(ntermL))
	allocate(coeffR(ntermR))

	call readopr('L',oprL,itermL,ntermL,coeffL)
	call readopr('R',oprR,itermR,ntermR,coeffR)

	call writeopr('g')
	call writeopr('h')
		
	deallocate(oprL,oprR,coeffL,coeffR)

contains
!==============================================================
!==============================================================

subroutine writeopr(H)
	implicit none
	integer :: i,j,k,l
	character(len=1) :: H
	integer :: ntermH,itermH
	character(len=2),allocatable :: oprH(:,:,:)

	if(H=='g') then
		ntermH=4
		itermH=4
	else if (H=='h') then
		ntermH=2
		itermH=2
	else
		write(*,*) "wrong H=",H
		stop
	end if

	open(unit=11,file=H,status="replace")
	write(11,*) H
	write(11,*) ntermL*ntermR*ntermH
	allocate(oprH(4,itermH,ntermH))
	call writeoprH(H,oprH,itermH,ntermH)
	do i=1,ntermL,1
	do j=1,ntermR,1
	do k=1,ntermH,1
		write(11,*) coeffL(i)*coeffR(j)
		do l=1,itermL,1
			write(11,'(4A3)') oprL(:,l,i)
		end do
		do l=1,itermH,1
			write(11,'(4A3)') oprH(:,l,k)
		end do
		do l=1,itermR,1
			write(11,'(4A3)') oprR(:,l,j)
		end do
	end do
	end do
	end do
	close(11)
	deallocate(oprH)

return
end subroutine writeopr

!==============================================================
!==============================================================

end program 

subroutine writeoprH(H,oprH,itermH,ntermH)
	implicit none
	integer :: itermH,ntermH
	character(len=2) :: oprH(4,itermH,ntermH)
	character(len=1) :: H
	
	oprH='em' ! empty

	if(H=='g') then
		oprH(1,1,:)='p'
		oprH(1,2,:)='q'
		oprH(1,3,:)='r'
		oprH(1,4,:)='s'
		oprH(2:3,:,:)='r'
		oprH(4,1:4,1)='u'
		oprH(4,1:4,2)='d'
		oprH(4,1,3)='u'
		oprH(4,4,3)='u'
		oprH(4,2,3)='d'
		oprH(4,3,3)='d'
		oprH(4,1,4)='d'
		oprH(4,4,4)='d'
		oprH(4,2,4)='u'
		oprH(4,3,4)='u'
	else if(H=='h') then
		oprH(1,1,:)='p'
		oprH(1,2,:)='q'
		oprH(2:3,:,:)='r'
		oprH(4,1:2,1)='u'
		oprH(4,1:2,2)='d'
	end if
return
end subroutine writeoprH

subroutine readopr(symbol,opr,iterm,nterm,coeff)
	implicit none
	character(len=1) :: symbol,dummysymbol
	integer :: iterm,nterm
	character(len=2) :: opr(4,iterm,nterm)
	real(kind=8) :: coeff(nterm)
	integer :: i,j

	open(unit=10,file="inp.inp",status="old")
	do while(.true.)
		read(10,*) dummysymbol
		if(dummysymbol==symbol) then
			exit
		end if
	end do
	do i=1,nterm,1
		read(10,*) coeff(i)
		do j=1,iterm,1
			read(10,*) opr(:,j,i)
		end do
	end do
	close(10)
	return
end subroutine readopr


