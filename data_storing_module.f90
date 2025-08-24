module data_storing_module
  implicit none
  private
  public :: solver_results
type solver_results
    real, allocatable :: times(:)
    real, allocatable :: temperature(:)
    real, allocatable :: temperature_field(:,:,:)  ! (optional, not used now)
    real, allocatable :: source_sum(:)
    real, allocatable :: source_max(:)
    real, allocatable :: source_norm(:)
    real, allocatable :: I0_values(:)
    real, allocatable :: delta_values(:)

    ! summary values
    real :: max_temperature
    real :: final_source_sum
    real :: final_source_max
    real :: final_source_norm
    real :: I0
    real :: delta
end type solver_results
end module data_storing_module
