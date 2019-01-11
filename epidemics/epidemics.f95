! Author: Sasithorn Hannarong, shannarong2015@my.fit.edu
! Course: CSE 4250, Fall 2017
! Project: Proj1, Spread of Epidemics

program main
    implicit none

    INTEGER :: n
    INTEGER :: IOstatus
    REAL :: alpha, a, z, y
    REAL :: eps = 4.e-16, endSeries = -(1.e-6)
    REAL :: p = 1.0, e1, t, w, L1, L2
    REAL wValue

    !Assign e value
    REAL :: e = 1.0
    e = exp(e)

    !get n and alpha then calculate z to plug in W(z) function
    do
        read  (*,*, IOSTAT=IOstatus) n, alpha
        if (IOstatus < 0) then
            EXIT
        else
            z = -alpha*(e**(-alpha))
            wValue = LambertW(z,z)
            y = (1 + wValue)/alpha
            PRINT *, INT (abs(n*y))
        end if
    end do

contains

recursive function LambertW(w, z) result(a)
        implicit none
        REAL, intent(in) :: w, z
        REAL :: a, prev
        REAL :: e = 1.0, e2 = -1.0
        e = exp(e)
        e2 = (e2 * e)** (-1)

        if (w < 0.00001 .AND. w > e2) then
            a = 0
        else if (w == e2) then
            a = -1
        else
            a = LambertW(w-1, z)
            a = a**2 + ( ( z*e**-a) / (1+a))
        end if

    END

end program
