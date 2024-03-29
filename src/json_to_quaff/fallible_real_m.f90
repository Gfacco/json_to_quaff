module json_to_fallible_real_m
  use erloff, only: error_list_t, fatal_t, module_t, procedure_t
  use rojff, only: &
      fallible_json_value_t, &
      json_element_t, &
      json_number_t, &
      json_value_t


  implicit none
  private
  public :: fallible_real_t

  type :: fallible_real_t
    !! This type serves to allow a function to return either an real or a list of errors.
    !! For example, this type can ensure that a user input is safe.
    private
    double precision :: real__ = 0.0
    type(error_list_t) :: errors_
  contains
    procedure :: failed
    procedure :: real_
    procedure :: errors
  end type

  interface fallible_real_t
    module procedure from_json_element
    module procedure from_json_value
    module procedure from_fallible_real
    module procedure from_fallible_json_value
  end interface

  character(len=*), parameter :: MODULE_NAME = "fallible_real_m"
contains
  impure elemental function from_json_element(json) result(fallible_real)
    type(json_element_t), intent(in) :: json
    type(fallible_real_t) :: fallible_real

    fallible_real = fallible_real_t(fallible_real_t(json%json), module_t(MODULE_NAME), procedure_t("from_json_element"))
  end function

  function from_json_value(json) result(fallible_real)
    class(json_value_t), intent(in) :: json
    type(fallible_real_t) :: fallible_real

    select type (json)
    type is (json_number_t)
      fallible_real%real__ = json%number
    class default
      fallible_real%errors_ = error_list_t(fatal_t( &
          module_t(MODULE_NAME), &
          procedure_t("from_json_value"), &
          json%to_compact_string() // " was not an real."))
    end select
  end function

  function from_fallible_real(fallible_real, module_, procedure_) result(new_fallible_real)
    !! This allows a procedure to either continue on with the returned value, or append its name to the call stack of any errors.
    type(fallible_real_t), intent(in) :: fallible_real
    type(module_t), intent(in) :: module_
    type(procedure_t), intent(in) :: procedure_
    type(fallible_real_t) :: new_fallible_real

    if (fallible_real%failed()) then
      new_fallible_real%errors_ = error_list_t(fallible_real%errors_, module_, procedure_)
    else
      new_fallible_real%real__ = fallible_real%real__
    end if
  end function

  function from_fallible_json_value(json) result(fallible_real)
    type(fallible_json_value_t), intent(in) :: json
    type(fallible_real_t) :: fallible_real

    if (json%failed()) then
      fallible_real%errors_ = json%errors
    else
      fallible_real = fallible_real_t( &
          fallible_real_t(json%value_), &
          module_t(MODULE_NAME), &
          procedure_t("from_fallible_json_value"))
    end if
  end function

  elemental function failed(self)
    class(fallible_real_t), intent(in) :: self
    logical :: failed

      failed = self%errors_%has_any()
  end function

  elemental function real_(self)
    class(fallible_real_t), intent(in) :: self
    double precision :: real_

    real_ = self%real__
  end function

  impure elemental function errors(self)
    class(fallible_real_t), intent(in) :: self
    type(error_list_t) :: errors

    errors = self%errors_
  end function
end module
