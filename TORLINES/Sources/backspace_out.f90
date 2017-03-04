!-----------------------------------------------------------------------
!     Function:      backspace
!     Authors:       S. Lazerson (lazerson@pppl.gov)
!     Date:          12/9/2011
!     Description:   Outputs n backspaces to unit.
!-----------------------------------------------------------------------
      SUBROUTINE backspace_out(unit,n)
!-----------------------------------------------------------------------
!     Libraries (none)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Input Parameters
!          unit         Unit number to output to.
!          n            Number of backspaces.
!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT(in) :: unit
      INTEGER, INTENT(in) :: n
!-----------------------------------------------------------------------
!     Local Variables
!          i            Dummy Index
!-----------------------------------------------------------------------
      INTEGER :: i
!-----------------------------------------------------------------------
!     Begin Function
!-----------------------------------------------------------------------
      DO i = 1, n
         WRITE(unit,'(a)',ADVANCE='no') CHAR(8)
      END DO
!-----------------------------------------------------------------------
!     End Function
!-----------------------------------------------------------------------
      END SUBROUTINE backspace_out
