module json_to_quaff_speed_m
  use quaff, only: &
    fallible_speed_t, &
    fallible_speed_unit_t, &
    parse_speed, &
    parse_speed_unit
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

interface fallible_speed_t
    module procedure fallible_speed_from_fallible_json_value
end interface
interface fallible_speed_unit_t
  module procedure fallible_speed_unit_from_fallible_json_value
end interface

character(len=*), parameter :: MODULE_NAME = "json_to_quaff_speed_m"

contains

function fallible_speed_from_fallible_json_value( &
  fallible_json_value) result(fallible_speed)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_speed_t) :: fallible_speed

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_speed_from_fallible_json_value"

  if (fallible_json_value%failed()) then
      fallible_speed = fallible_speed_t(fallible_json_value%errors)
  else
      select type (json_value => fallible_json_value%json)
          type is (json_string_t)
              fallible_speed = fallible_speed_t( &
                  parse_speed(json_value%string), &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME))
          class default
              fallible_speed = fallible_speed_t(error_list_t(fatal_t( &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME), &
                  json_value%to_expanded_string() // " is not json_string_t type")))
      end select
  end if
end function

function fallible_speed_unit_from_fallible_json_value( &
  fallible_json_value) result(fallible_speed_unit)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_speed_unit_t) :: fallible_speed_unit

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_speed_unit_from_fallible_json_value"

  if (fallible_json_value%failed()) then
    fallible_speed_unit = fallible_speed_unit_t(fallible_json_value%errors)
  else
    select type (json_value => fallible_json_value%json)
      type is (json_string_t)
        fallible_speed_unit = fallible_speed_unit_t( &
            parse_speed_unit(json_value%string), &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME))
      class default
        fallible_speed_unit = fallible_speed_unit_t(error_list_t(fatal_t( &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME), &
              json_value%to_expanded_string() // " is not json_string_t type")))
    end select
  end if
end function

end module