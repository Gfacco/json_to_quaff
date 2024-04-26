module json_to_fallible_integer_m
  use erloff, only: error_list_t, fatal_t, module_t, procedure_t
  use rojff, only: &
      fallible_json_value_t, &
      json_element_t, &
      json_integer_t, &
      json_number_t, &
      json_value_t


  implicit none
  private
  public :: fallible_integer_t

  type :: fallible_integer_t
    !! This type serves to allow a function to return either an integer or a list of errors.
    !! For example, this type can ensure that a user input is safe.
    private
    integer :: integer__ = 0
    type(error_list_t) :: errors_
  contains
    procedure :: failed
    procedure :: integer_
    procedure :: errors
  end type

  interface fallible_integer_t
    module procedure from_json_element
    module procedure from_json_value
    module procedure from_fallible_integer
    module procedure from_fallible_json_value
  end interface

  character(len=*), parameter :: MODULE_NAME = "fallible_integer_m"
contains
  impure elemental function from_json_element(json) result(fallible_integer)
    type(json_element_t), intent(in) :: json
    type(fallible_integer_t) :: fallible_integer

    fallible_integer = fallible_integer_t(fallible_integer_t(json%json), module_t(MODULE_NAME), procedure_t("from_json_element"))
  end function

  function from_json_value(json) result(fallible_integer)
    class(json_value_t), intent(in) :: json
    type(fallible_integer_t) :: fallible_integer

    select type (json)
    type is (json_integer_t)
      fallible_integer%integer__ = int(json%number)
    class default
      fallible_integer%errors_ = error_list_t(fatal_t( &
          module_t(MODULE_NAME), &
          procedure_t("from_json_value"), &
          json%to_compact_string() // " was not an integer."))
    end select
  end function

  function from_fallible_integer(fallible_integer, module_, procedure_) result(new_fallible_integer)
    !! This allows a procedure to either continue on with the returned value, or append its name to the call stack of any errors.
    type(fallible_integer_t), intent(in) :: fallible_integer
    type(module_t), intent(in) :: module_
    type(procedure_t), intent(in) :: procedure_
    type(fallible_integer_t) :: new_fallible_integer

    if (fallible_integer%failed()) then
      new_fallible_integer%errors_ = error_list_t(fallible_integer%errors_, module_, procedure_)
    else
      new_fallible_integer%integer__ = fallible_integer%integer__
    end if
  end function

  function from_fallible_json_value(json) result(fallible_integer)
    type(fallible_json_value_t), intent(in) :: json
    type(fallible_integer_t) :: fallible_integer

    if (json%failed()) then
      fallible_integer%errors_ = json%errors
    else
      fallible_integer = fallible_integer_t( &
          fallible_integer_t(json%value_), &
          module_t(MODULE_NAME), &
          procedure_t("from_fallible_json_value"))
    end if
  end function

  elemental function failed(self)
    class(fallible_integer_t), intent(in) :: self
    logical :: failed

      failed = self%errors_%has_any()
  end function

  elemental function integer_(self)
    class(fallible_integer_t), intent(in) :: self
    integer :: integer_

    integer_ = self%integer__
  end function

  impure elemental function errors(self)
    class(fallible_integer_t), intent(in) :: self
    type(error_list_t) :: errors

    errors = self%errors_
  end function
end module
