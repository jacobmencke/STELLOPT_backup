!Stolen from https://stackoverflow.com/questions/28048508/how-to-add-new-element-to-dynamical-array-in-fortran-90


module DynamicalArrays

  contains

      subroutine AddToList(list, element)

          IMPLICIT NONE

          integer :: i, isize
          double precision, intent(in) :: element
          double precision, dimension(:), allocatable, intent(inout) :: list
          double precision, dimension(:), allocatable :: clist


          if(allocated(list)) then
              isize = size(list)
              allocate(clist(isize+1))
              do i=1,isize          
              clist(i) = list(i)
              end do
              clist(isize+1) = element

              deallocate(list)
              call move_alloc(clist, list)

          else
              allocate(list(1))
              list(1) = element
          end if


      end subroutine AddToList
      subroutine AddToListint(list, element)

          IMPLICIT NONE

          integer :: i, isize
          integer, intent(in) :: element
          integer, dimension(:), allocatable, intent(inout) :: list
          integer, dimension(:), allocatable :: clist


          if(allocated(list)) then
              isize = size(list)
              allocate(clist(isize+1))
              do i=1,isize
              clist(i) = list(i)
              end do
              clist(isize+1) = element

              deallocate(list)
              call move_alloc(clist, list)

          else
              allocate(list(1))
              list(1) = element
          end if


      end subroutine AddToListint
      subroutine AddToListbol(list, element)

          IMPLICIT NONE

          integer :: i, isize
          logical, intent(in) :: element
          logical, dimension(:), allocatable, intent(inout) :: list
          logical, dimension(:), allocatable :: clist


          if(allocated(list)) then
              isize = size(list)
              allocate(clist(isize+1))
              do i=1,isize
              clist(i) = list(i)
              end do
              clist(isize+1) = element

              deallocate(list)
              call move_alloc(clist, list)

          else
              allocate(list(1))
              list(1) = element
          end if


      end subroutine AddToListbol


  end module DynamicalArrays
