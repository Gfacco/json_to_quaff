module json_to_fallible_bool_m
  use erloff, only: error_list_t, fatal_t, module_t, procedure_t
  use rojff, only: &
      fallible_json_value_t, &
      json_element_t, &
      json_bool_t, &
      json_value_t


  implicit none
  private
  public :: fallible_bool_t

  type :: fallible_bool_t
    !! This type serves to allow a function to return either an logical or a list of errors.
    !! For example, this type can ensure that a user input is safe.
    private
    logical :: bool_ =.false.
    type(error_list_t) :: errors_
  contains
    procedure :: failed
    procedure :: bool
    procedure :: errors
  end type

  interface fallible_bool_t
    module procedure from_json_element
    module procedure from_json_value
    module procedure from_fallible_bool
    module procedure from_fallible_json_value
  end interface

  character(len=*), parameter :: MODULE_NAME = "fallible_bool_m"
contains
  impure elemental function from_json_element(json) result(fallible_bool)
    type(json_element_t), intent(in) :: json
    type(fallible_bool_t) :: fallible_bool

    fallible_bool = fallible_bool_t(fallible_bool_t(json%json), module_t(MODULE_NAME), procedure_t("from_json_element"))
  end function

  function from_json_value(json) result(fallible_bool)
    class(json_value_t), intent(in) :: json
    type(fallible_bool_t) :: fallible_bool

    select type (json)
    type is (json_bool_t)
      fallible_bool%bool_ = json%bool
    class default
      fallible_bool%errors_ = error_list_t(fatal_t( &
          module_t(MODULE_NAME), &
          procedure_t("from_json_value"), &
          json%to_compact_string() // " was not an logical."))
    end select
  end function

  function from_fallible_bool(fallible_bool, module_, procedure_) result(new_fallible_bool)
    !! This allows a procedure to either continue on with the returned value, or append its name to the call stack of any errors.
    type(fallible_bool_t), intent(in) :: fallible_bool
    type(module_t), intent(in) :: module_
    type(procedure_t), intent(in) :: procedure_
    type(fallible_bool_t) :: new_fallible_bool

    if (fallible_bool%failed()) then
      new_fallible_bool%errors_ = error_list_t(fallible_bool%errors_, module_, procedure_)
    else
      new_fallible_bool%bool_ = fallible_bool%bool_
    end if
  end function

  function from_fallible_json_value(json) result(fallible_bool)
    type(fallible_json_value_t), intent(in) :: json
    type(fallible_bool_t) :: fallible_bool

    if (json%failed()) then
      fallible_bool%errors_ = json%errors
    else
      fallible_bool = fallible_bool_t( &
          fallible_bool_t(json%value_), &
          module_t(MODULE_NAME), &
          procedure_t("from_fallible_json_value"))
    end if
  end function

  elemental function failed(self)
    class(fallible_bool_t), intent(in) :: self
    logical :: failed

      failed = self%errors_%has_any()
  end function

  elemental function bool(self)
    class(fallible_bool_t), intent(in) :: self
    logical :: bool

    bool = self%bool_
  end function

  impure elemental function errors(self)
    class(fallible_bool_t), intent(in) :: self
    type(error_list_t) :: errors

    errors = self%errors_
  end function
end module
