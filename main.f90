Program Wick_Contraction
! this program do wick theorem contraction of an array of creation
! and annihilation operators between HF like (fermi vaccum) state
	use Contraction_Mod
	use StandardIO
	use array_mod
	use analysis

	implicit none

	call ReadInput

	call Contraction

	call PrintOutputTerm

	call InterSingletFission

	call DeallocateArray

	write(*,*) "Wick_Contaction end Successfully!" 

end program Wick_Contraction

