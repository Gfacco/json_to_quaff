module json_to_quaff_length_m
  use quaff, only: &
    fallible_length_t, &
    fallible_length_unit_t, &
    parse_length, &
    parse_length_unit
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

interface fallible_length_t
    module procedure fallible_length_from_fallible_json_value
end interface
interface fallible_length_unit_t
  module procedure fallible_length_unit_from_fallible_json_value
end interface

character(len=*), parameter :: MODULE_NAME = "json_to_quaff_length_m"

contains

function fallible_length_from_fallible_json_value( &
  fallible_json_value) result(fallible_length)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_length_t) :: fallible_length

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_length_from_fallible_json_value"

  if (fallible_json_value%failed()) then
      fallible_length = fallible_length_t(fallible_json_value%errors)
  else
      select type (json_value => fallible_json_value%json)
          type is (json_string_t)
              fallible_length = fallible_length_t( &
                  parse_length(json_value%string), &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME))
          class default
              fallible_length = fallible_length_t(error_list_t(fatal_t( &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME), &
                  json_value%to_expanded_string() // " is not json_string_t type")))
      end select
  end if
end function

function fallible_length_unit_from_fallible_json_value( &
  fallible_json_value) result(fallible_length_unit)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_length_unit_t) :: fallible_length_unit

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_length_unit_from_fallible_json_value"

  if (fallible_json_value%failed()) then
    fallible_length_unit = fallible_length_unit_t(fallible_json_value%errors)
  else
    select type (json_value => fallible_json_value%json)
      type is (json_string_t)
        fallible_length_unit = fallible_length_unit_t( &
            parse_length_unit(json_value%string), &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME))
      class default
        fallible_length_unit = fallible_length_unit_t(error_list_t(fatal_t( &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME), &
              json_value%to_expanded_string() // " is not json_string_t type")))
    end select
  end if
end function

end module