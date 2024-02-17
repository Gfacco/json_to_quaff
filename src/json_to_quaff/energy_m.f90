module json_to_quaff_energy_m
  use quaff, only: &
    fallible_energy_t, &
    fallible_energy_unit_t, &
    parse_energy, &
    parse_energy_unit
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

interface fallible_energy_t
    module procedure fallible_energy_from_fallible_json_value
end interface
interface fallible_energy_unit_t
  module procedure fallible_energy_unit_from_fallible_json_value
end interface

character(len=*), parameter :: MODULE_NAME = "json_to_quaff_energy_m"

contains

function fallible_energy_from_fallible_json_value( &
  fallible_json_value) result(fallible_energy)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_energy_t) :: fallible_energy

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_energy_from_fallible_json_value"

  if (fallible_json_value%failed()) then
      fallible_energy = fallible_energy_t(fallible_json_value%errors)
  else
      select type (json_value => fallible_json_value%value_)
          type is (json_string_t)
              fallible_energy = fallible_energy_t( &
                  parse_energy(json_value%string), &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME))
          class default
              fallible_energy = fallible_energy_t(error_list_t(fatal_t( &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME), &
                  json_value%to_expanded_string() // " is not json_string_t type")))
      end select
  end if
end function

function fallible_energy_unit_from_fallible_json_value( &
  fallible_json_value) result(fallible_energy_unit)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_energy_unit_t) :: fallible_energy_unit

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_energy_unit_from_fallible_json_value"

  if (fallible_json_value%failed()) then
    fallible_energy_unit = fallible_energy_unit_t(fallible_json_value%errors)
  else
    select type (json_value => fallible_json_value%value_)
      type is (json_string_t)
        fallible_energy_unit = fallible_energy_unit_t( &
            parse_energy_unit(json_value%string), &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME))
      class default
        fallible_energy_unit = fallible_energy_unit_t(error_list_t(fatal_t( &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME), &
              json_value%to_expanded_string() // " is not json_string_t type")))
    end select
  end if
end function

end module