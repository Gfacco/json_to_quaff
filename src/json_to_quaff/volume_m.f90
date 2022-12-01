module json_to_quaff_volume_m
  use quaff, only: &
    fallible_volume_t, &
    fallible_volume_unit_t, &
    parse_volume, &
    parse_volume_unit
  use rojff, only: &
    fallible_json_value_t, &
    json_value_t, &
    json_element_t, &
    json_string_t
  use erloff, only: &
    error_list_t, &
    module_t, &
    procedure_t, &
    fatal_t

implicit none

interface fallible_volume_t
    module procedure fallible_volume_from_fallible_json_value
    module procedure from_json_element
    module procedure from_json_value
end interface
interface fallible_volume_unit_t
  module procedure fallible_volume_unit_from_fallible_json_value
end interface

character(len=*), parameter :: MODULE_NAME = "json_to_quaff_volume_m"

contains

function fallible_volume_from_fallible_json_value( &
  fallible_json_value) result(fallible_volume)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_volume_t) :: fallible_volume

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_volume_from_fallible_json_value"

  if (fallible_json_value%failed()) then
      fallible_volume = fallible_volume_t(fallible_json_value%errors)
  else
      select type (json_value => fallible_json_value%value_)
          type is (json_string_t)
              fallible_volume = fallible_volume_t( &
                  parse_volume(json_value%string), &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME))
          class default
              fallible_volume = fallible_volume_t(error_list_t(fatal_t( &
                  module_t(MODULE_NAME), &
                  procedure_t(PROCEDURE_NAME), &
                  json_value%to_expanded_string() // " is not json_string_t type")))
      end select
  end if
end function

function fallible_volume_unit_from_fallible_json_value( &
  fallible_json_value) result(fallible_volume_unit)
  type(fallible_json_value_t), intent(in) :: fallible_json_value
  type(fallible_volume_unit_t) :: fallible_volume_unit

  character(len=*), parameter :: PROCEDURE_NAME = "fallible_volume_unit_from_fallible_json_value"

  if (fallible_json_value%failed()) then
    fallible_volume_unit = fallible_volume_unit_t(fallible_json_value%errors)
  else
    select type (json_value => fallible_json_value%value_)
      type is (json_string_t)
        fallible_volume_unit = fallible_volume_unit_t( &
            parse_volume_unit(json_value%string), &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME))
      class default
        fallible_volume_unit = fallible_volume_unit_t(error_list_t(fatal_t( &
              module_t(MODULE_NAME), &
              procedure_t(PROCEDURE_NAME), &
              json_value%to_expanded_string() // " is not json_string_t type")))
    end select
  end if
end function

impure elemental function from_json_element(json) result(fallible_volume)
  type(json_element_t), intent(in) :: json
  type(fallible_volume_t) :: fallible_volume

  fallible_volume = fallible_volume_t(json%json)
end function

function from_json_value(json_value) result(fallible_volume)
  class(json_value_t), intent(in) :: json_value
  type(fallible_volume_t) :: fallible_volume

  character(len=*), parameter :: PROCEDURE_NAME = "from_json_value"


  select type (json_value)
  type is (json_string_t)
      fallible_volume = fallible_volume_t( &
          parse_volume(json_value%string), &
          module_t(MODULE_NAME), &
          procedure_t(PROCEDURE_NAME))
  class default
      fallible_volume = fallible_volume_t(error_list_t(fatal_t( &
          module_t(MODULE_NAME), &
          procedure_t(PROCEDURE_NAME), &
          json_value%to_expanded_string() // " is not json_string_t type")))
end select
end function

end module