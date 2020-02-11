program integral
use aleatorio
implicit none
integer::N,mi,mf,dm
real::x
integer::i,j
real::xi,y,E

mi=1000 !mi es el numero de iteraciones inicial para la integracion 
mf=100000 !mf es el numero de iteraciones final para la integracion 
dm=1000 !dm es el paso en el numero de iteraciones

open(unit=10,file='datos1.dat')
do N=mi,mf,dm !se crea un ciclo desde Mi hasta Mf en pasos de dM
E=integrate(N) !se calcula la integral con N iteraciones 
write(10,*) N,E
enddo
close(10)

print*,float((mf-mi)/dm) ! se imprime el numero de puntos a graficar 

end program 

