module aleatorio
contains

real function f(x)
!definicion de la funcion a integrar para mas facilidad 
implicit none
real::x
f=4./(1.+x*x)
end function

real function integrate(N)
!integra la funcion f por el metodo de monte carlo con N iteraciones
implicit none
integer::N
integer::i
real::x,y
do i=1,N
  x=ran2(-i)
  y=f(x)
  integrate=integrate+y
enddo
integrate=integrate/N
end function


!------------------------------------------------------------
real function ran2(idum)
implicit none

 integer,parameter:: ia1=40014, ia2=40692
 integer,parameter:: im1=2147483563, im2=2147483399, imm1=im1-1
 integer,parameter::iq1=53668, iq2=52774, ir1=12211
 integer,parameter:: ir2=3791, ntab=32, ndiv= 1+(imm1/ntab)
 real, parameter:: am=1./im1, eps=1.2e-7, rnmx=1.-eps

 integer:: idum, idum2

 integer:: j, k, iv(ntab), iy
 
 idum2=123456789
 iv=ntab*0
 iy=0

 if (idum .le. 0) then
   idum=max(-idum,1)
   idum2=idum

   do j=ntab+8, 1, -1
     k= idum/iq1
     idum=ia1*(idum - k*iq1) - (ir1*k)
     if(idum .lt. 0) idum=idum+im1
     if(j .le. ntab) iv(j)= idum
   enddo

   iy=iv(1)
 endif
 
 k=idum/iq1
 idum=ia1*(idum - k*iq1) - ir1*k
 if (idum .lt. 0) idum= idum + im1

 k=idum2/iq2
 idum2=ia2*(idum2 - k*iq2) - ir2*k
 if (idum2 .lt. 0) idum2= idum2 + im2

 j=1 + iy/ndiv
 iy=iv(j)-idum2
 iv(j)=idum
 if(iy .lt. 1) iy=iy+imm1
 ran2=min(am*iy, rnmx)

end function ran2


end module
