module json_to_quaff_mass_m
  use quaff, only: &
    fallible_mass_t, &
    fallible_mass_unit_t, &
    parse_mass, &
    parse_mass_unit
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

interface fallible_mass_t
    module procedure fallible_mass_from_fallible_json_value
end interface
interface fallible_mass_unit_t
  module procedure fallible_mass_unit_from_fallible_json_value
end interface

character(len=*), parameter :: MODULE_NAME = "json_to_quaff_mass_m"

contains

function fallible_mass_from_fallible_json_value( &
  fallible_json_value) result(fallible_mass)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_mass_t) :: fallible_mass

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_mass_from_fallible_json_value"

  if (fallible_json_value%failed()) then
      fallible_mass = fallible_mass_t(fallible_json_value%errors)
  else
      select type (json_value => fallible_json_value%value_)
          type is (json_string_t)
              fallible_mass = fallible_mass_t( &
                  parse_mass(json_value%string), &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME))
          class default
              fallible_mass = fallible_mass_t(error_list_t(fatal_t( &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME), &
                  json_value%to_expanded_string() // " is not json_string_t type")))
      end select
  end if
end function

function fallible_mass_unit_from_fallible_json_value( &
  fallible_json_value) result(fallible_mass_unit)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_mass_unit_t) :: fallible_mass_unit

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_mass_unit_from_fallible_json_value"

  if (fallible_json_value%failed()) then
    fallible_mass_unit = fallible_mass_unit_t(fallible_json_value%errors)
  else
    select type (json_value => fallible_json_value%value_)
      type is (json_string_t)
        fallible_mass_unit = fallible_mass_unit_t( &
            parse_mass_unit(json_value%string), &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME))
      class default
        fallible_mass_unit = fallible_mass_unit_t(error_list_t(fatal_t( &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME), &
              json_value%to_expanded_string() // " is not json_string_t type")))
    end select
  end if
end function

end module