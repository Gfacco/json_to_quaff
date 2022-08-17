module json_to_quaff
  use json_to_quaff_mass_m, only : fallible_mass_t
  use json_to_quaff_temperature_m, only : fallible_temperature_t
  use json_to_quaff_time_m, only : fallible_time_t
  use json_to_quaff_volume_m, only : fallible_volume_t
  use json_to_quaff_length_m, only : fallible_length_t
  use json_to_fallible_integer_m, only : fallible_integer_t
  use json_to_fallible_real_m, only : fallible_real_t
  use json_to_fallible_bool_m, only : fallible_bool_t
end module