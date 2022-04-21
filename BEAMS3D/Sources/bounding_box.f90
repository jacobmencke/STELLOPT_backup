!-----------------------------------------------------------------------
!     Function:      out_beams3d_nag
!     Authors:       S. Lazerson (lazerson@pppl.gov), M. McMillan (matthew.mcmillan@my.wheaton.edu)
!     Date:          06/20/2012
!     Description:   Save output from field line following while running
!                    and updates progress bar.
!-----------------------------------------------------------------------
SUBROUTINE bounding_box(t, q, filename, mystart)
    !-----------------------------------------------------------------------
    !     Libraries
    !-----------------------------------------------------------------------
    USE stel_kinds, ONLY: rprec
    USE beams3d_runtime, ONLY: dt, lverb, pi2, lneut, t_end, lvessel, &
                               lhitonly, npoinc, lcollision, ldepo, &
                               weight, invpi2, ndt, ndt_max
    USE beams3d_lines, ONLY: myline, moment, &
                             nsteps, nparticles, myend, &
                             mytdex, next_t,&
                             xlast, ylast, zlast, dense_prof, &
                             ltherm
    USE beams3d_grid
    USE beams3d_physics_mod, ONLY: beams3d_physics
    USE wall_mod, ONLY: collide, get_wall_ik, get_wall_area
    USE mpi_params
    USE mpi_inc
    USE DynamicalArrays!From Stack Overflow
    !-----------------------------------------------------------------------
    !     Input Parameters
    !          t          Location along fieldline in t
    !          q            (q(1),q(2),q(3),q(4)) = (R,phi,Z,vll)
    !-----------------------------------------------------------------------
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(inout) :: t
    DOUBLE PRECISION, INTENT(inout) :: q(4)
    character(len=50), INTENT(in) :: filename
    INTEGER, INTENT(in) :: mystart
    !-----------------------------------------------------------------------
    !     Local Variables
    !     jint      Index along phi
    !-----------------------------------------------------------------------
    LOGICAL             :: lhit
    INTEGER             :: ier, d1, d2, d3, d4, d5, n                                                                          
    DOUBLE PRECISION         :: x0,y0,z0,x1,y1,z1,xw,yw,zw, vperp
    !DOUBLE PRECISION         :: R_box, Z_box, PHI_box, vll_box, neut_box, S_box, U_box, B_box
    DOUBLE PRECISION    :: q2(4),qdot(4)                                                                                    
    ! For splines                                                                                                           
    INTEGER :: i,j,k,l                                                                                                      
    REAL*8 :: xparam, yparam, zparam !, hx, hy, hz, hxi, hyi, hzi
    REAL*8 :: fval(1)                                                                                                       
    INTEGER, parameter :: ict(8)=(/1,0,0,0,0,0,0,0/)
    REAL*8, PARAMETER :: one = 1
    DOUBLE PRECISION, PARAMETER :: pi=3.14159265359                                                                                            
    !-----------------------------------------------------------------------
    !     Begin Function                                                                                                    
    !-----------------------------------------------------------------------
    x0 = MOD(q(2), 2*pi)
    IF (x0 < 0) x0 = x0 + 2*pi
    !CALL EZspline_isInDomain(S_spl,q(1),x0,q(3),ier)
    y0 = 0  ! If we're out of domain then don't worry about collisions
    !IF (ier==0) THEN
    !write(6,'(I4,F9.5,F8.5,F8.5,F8.5)') myline,q(1),q(2),q(3),x0
    IF ((q(1) >= 5.00) .and. (q(1) <= 6.29) .and. &
        (x0 >= 1.47) .and. (x0 <= 1.63) .and. &
        (q(3) >= -0.05) .and. (q(3) <= 0.59)) THEN
       open(10*mystart+1,file = trim(filename)//'.txt',status="old",action="read",form='unformatted')
       read(10*mystart+1) n
       close(10*mystart+1)
       open(10*mystart+1,file = trim(filename)//'.txt',status="replace",action="write",form='unformatted')
       write(10*mystart+1) n+1
       close(10*mystart+1)      
       !open(unit=1,file=filename,form='unformatted')
       !write(6,'(A)') '-----------Open File---------'
       open(10*mystart,file = trim(filename)//'.dat',status="old", position="append",action="write",form='unformatted')
       !open(10,file = 'Complete.dat',status="old", position="append",action="write",form='unformatted')
       x0 = MOD(q(2), phimax)
       IF (x0 < 0) x0 = x0 + phimax
       !close(1) 
       !CALL ADDTOLISTINT(line_box,myline)
       !CALL ADDTOLIST(R_box, q(1))
       !CALL ADDTOLIST(PHI_box,q(2))
       !CALL ADDTOLIST(Z_box,q(3))
       !CALL ADDTOLIST(vll_box,q(4))
       !CALL ADDTOLIST(moment_box,moment)
       !CALL ADDTOLISTBOL(neut_box,lneut)
       i = MIN(MAX(COUNT(raxis < q(1)),1),nr-1)
       j = MIN(MAX(COUNT(phiaxis < x0),1),nphi-1)
       k = MIN(MAX(COUNT(zaxis < q(3)),1),nz-1)
       xparam = (q(1) - raxis(i)) * hri(i)
       yparam = (x0 - phiaxis(j)) * hpi(j)
       zparam = (q(3) - zaxis(k)) * hzi(k)
       CALL R8HERM3FCN(ict,1,1,fval,i,j,k,xparam,yparam,zparam,&
                       hr(i),hri(i),hp(j),hpi(j),hz(k),hzi(k),&                                                                                                                                                                                                                                       
                       S4D(1,1,1,1),nr,nphi,nz)
       y0 = fval(1)
       !CALL ADDTOLIST(S_box,y0)
       CALL R8HERM3FCN(ict,1,1,fval,i,j,k,xparam,yparam,zparam,&
                       hr(i),hri(i),hp(j),hpi(j),hz(k),hzi(k),&
                       U4D(1,1,1,1),nr,nphi,nz)
       z0 = fval(1)
       !U_val
       CALL R8HERM3FCN(ict,1,1,fval,i,j,k,xparam,yparam,zparam,&
                       hr(i),hri(i),hp(j),hpi(j),hz(k),hzi(k),&
                       MODB4D(1,1,1,1),nr,nphi,nz)
       !CALL ADDTOLIST(B_box,fval(1))
       write(10*mystart) myline,q(1),q(2),q(3), q(4),moment,lneut,y0,z0,fval(1),t
       !write(6,'(I4,x,F9.5,x,F8.5,x,F8.5,x,E10.5,x,E19.5,x,L,x,F8.5,x,F8.5,x,F15.5,x,E19.7)') &
       !myline,q(1),q(2),q(3),q(4),moment,lneut,y0,z0,fval(1), t
       close(10*mystart)
    END IF
END SUBROUTINE bounding_box
