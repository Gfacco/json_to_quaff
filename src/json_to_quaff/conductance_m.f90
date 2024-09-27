module json_to_quaff_conductance_m
  use quaff, only: &
    fallible_conductance_t, &
    fallible_conductance_unit_t, &
    parse_conductance, &
    parse_conductance_unit
  use rojff, only: &
    fallible_json_value_t, &
    json_value_t, &
    json_string_t
  use erloff, only: &
    error_list_t, &
    module_t, &
    procedure_t, &
    fatal_t

implicit none

interface fallible_conductance_t
    module procedure fallible_conductance_from_fallible_json_value
end interface
interface fallible_conductance_unit_t
  module procedure fallible_conductance_unit_from_fallible_json_value
end interface

character(len=*), parameter :: MODULE_NAME = "json_to_quaff_conductance_m"

contains

function fallible_conductance_from_fallible_json_value( &
  fallible_json_value) result(fallible_conductance)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_conductance_t) :: fallible_conductance

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_conductance_from_fallible_json_value"

  if (fallible_json_value%failed()) then
      fallible_conductance = fallible_conductance_t(fallible_json_value%errors)
  else
      select type (json_value => fallible_json_value%value_)
          type is (json_string_t)
              fallible_conductance = fallible_conductance_t( &
                  parse_conductance(json_value%string), &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME))
          class default
              fallible_conductance = fallible_conductance_t(error_list_t(fatal_t( &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME), &
                  json_value%to_expanded_string() // " is not json_string_t type")))
      end select
  end if
end function

function fallible_conductance_unit_from_fallible_json_value( &
  fallible_json_value) result(fallible_conductance_unit)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_conductance_unit_t) :: fallible_conductance_unit

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_conductance_unit_from_fallible_json_value"

  if (fallible_json_value%failed()) then
    fallible_conductance_unit = fallible_conductance_unit_t(fallible_json_value%errors)
  else
    select type (json_value => fallible_json_value%value_)
      type is (json_string_t)
        fallible_conductance_unit = fallible_conductance_unit_t( &
            parse_conductance_unit(json_value%string), &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME))
      class default
        fallible_conductance_unit = fallible_conductance_unit_t(error_list_t(fatal_t( &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME), &
              json_value%to_expanded_string() // " is not json_string_t type")))
    end select
  end if
end function

end module