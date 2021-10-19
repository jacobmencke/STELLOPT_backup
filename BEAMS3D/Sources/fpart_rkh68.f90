!-----------------------------------------------------------------------
!     Function:      fpart_rkh68
!     Authors:       S. Lazerson (samuel.lazerson@ipp.mpg.de)
!     Date:          06/20/2012
!     Description:   Just a wrapper to fpart_nag.
!
!-----------------------------------------------------------------------
      SUBROUTINE fpart_rkh68(t,q,qdot,istat)
!-----------------------------------------------------------------------
!     Libraries NONE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Input Variables
!          t          time coordinates
!          q          q(k) = (R,phi,Z,v_r,Vphi,V_z)
!          qdot       dq/dt
!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER          :: istat
      DOUBLE PRECISION :: t, q(6), qdot(6)
!-----------------------------------------------------------------------
!     Begin Subroutine
!------------------------------------------------
      CALL fpart_nag(t,q,qdot)
      istat = 0
      RETURN
!-----------------------------------------------------------------------
!     End Subroutine
!-----------------------------------------------------------------------
      END SUBROUTINE fpart_rkh68
