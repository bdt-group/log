# log

A wrapper around Erlang system logger to simply configuration

# Usage

Configuration example:
```erl
[
  ...
  {log, [{dir, "/path/to/log"},
         {level, warning},
         {rotate_size, 1024},
         {rotate_count, 5}]}
  ]
]
```

**NOTE**: all parameters are optional