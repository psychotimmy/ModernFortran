! Program to compute pi to 'digitsrequired' digits using a spigot algorithm
! Based on an original Pascal program retrieved from
! https://www.cs.williams.edu/~heeringa/classes/cs135/s15/readings/spigot.pdf
!
! Modern Fortran version by Tim Holyoake, 11th May 2026
!
program pispigot
  implicit none
! Set the digitsrequired parameter
  integer,parameter :: digitsrequired = 1000
  integer,parameter :: arraylen = (10*digitsrequired) / 3
  integer :: i,j,k,q,x,t,nines,predigit
  integer :: array(arraylen)

! Array starts off as all 2s
  array=2
! First predigit is 0
  predigit=0
  nines=0

  do j=1,digitsrequired
    q=0
    do i=arraylen,1,-1
      x=10*array(i) + q*i
      t=2*i-1
      array(i)=mod(x,t)
      q=x/t
    end do
    array(1)=mod(q,10)
    q=q/10
    if (q==9) then
      nines=nines+1
    else if (q==10) then
      write(*,'(i1)',advance='no') predigit+1
!     format assumes pi has no sequence of more than 20 repeating 0s
!     will still produce the correct result even so, but would break formatting
      write(*,'(20i1)',advance='no')(0, k=1,nines)
      predigit=0
      nines=0
    else
      write(*,'(i1)',advance='no') predigit
      predigit=q
      if (nines /= 0) then
!       format assumes pi has no sequence of more than 20 repeating 9s
!     will still produce the correct result even so, but would break formatting
        write(*,'(20i1)',advance='no')(9, k=1,nines)
        nines=0
      end if
    end if
  end do

! print the last predigit and a carriage return
  write(*,'(i1)') predigit

end program pispigot
