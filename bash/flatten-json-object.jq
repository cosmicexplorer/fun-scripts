def flatten_level:
  def flatten_iterable:
    to_entries[] | (.key | tostring) as $key | .value |
    (scalars | {key: $key, value: .}) //
    (flatten_iterable | {key: ($key + "/" + (.key | tostring)), value: .value});
  scalars // ([flatten_iterable] | from_entries);

flatten_level
