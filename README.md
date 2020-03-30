# log

A wrapper around Erlang system logger to simply configuration

# Usage

Configuration example:
```erl
[
  ...
  {log, [{dir, "/path/to/log/dir"},
         {level, warning},
         {rotate_size, 1024},
         {rotate_count, 5}]}
  ...
]
```
This will produce the configuration like:
```erl
...
 #{config =>
       #{burst_limit_enable => true,burst_limit_max_count => 500,
         burst_limit_window_time => 1000,compress_on_rotate => false,
         drop_mode_qlen => 1000,
         file => "/path/to/log/dir/error.log",file_check => 0,
         filesync_repeat_interval => no_repeat,flush_qlen => 5000,
         max_no_bytes => 1024000,max_no_files => 1,
         modes => [delayed_write,raw,append],
         overload_kill_enable => false,
         overload_kill_mem_size => 3000000,
         overload_kill_qlen => 20000,
         overload_kill_restart_after => 5000,sync_mode_qlen => 1000,
         type => file},
   filter_default => log,filters => [],
   formatter =>
       {logger_formatter,#{legacy_header => false,max_size => 102400,
                           single_line => false,
                           template =>
                               [time," [",level,"] ",pid,
                                {mfa,["@",mfa,{line,[":",line],[]}],[]},
                                " ",
                                {logger_formatter,[[logger_formatter,title],":","\n"],[]},
                                msg,"\n"],
                           time_designator => 32}},
   id => error_log,level => error,module => logger_std_h},
 #{config =>
       #{burst_limit_enable => true,burst_limit_max_count => 500,
         burst_limit_window_time => 1000,compress_on_rotate => false,
         drop_mode_qlen => 1000,
         file => "/path/to/log/dir/all.log",file_check => 0,
         filesync_repeat_interval => no_repeat,flush_qlen => 5000,
         max_no_bytes => 1024000,max_no_files => 1,
         modes => [delayed_write,raw,append],
         overload_kill_enable => false,
         overload_kill_mem_size => 3000000,
         overload_kill_qlen => 20000,
         overload_kill_restart_after => 5000,sync_mode_qlen => 1000,
         type => file},
   filter_default => log,filters => [],
   formatter =>
       {logger_formatter,#{legacy_header => false,max_size => 102400,
                           single_line => false,
                           template =>
                               [time," [",level,"] ",pid,
                                {mfa,["@",mfa,{line,[":",line],[]}],[]},
                                " ",
                                {logger_formatter,[[logger_formatter,title],":","\n"],[]},
                                msg,"\n"],
                           time_designator => 32}},
   id => ejabberd_log,level => all,module => logger_std_h},
 #{config =>
       #{burst_limit_enable => true,burst_limit_max_count => 500,
         burst_limit_window_time => 1000,drop_mode_qlen => 200,
         filesync_repeat_interval => no_repeat,flush_qlen => 1000,
         overload_kill_enable => false,
         overload_kill_mem_size => 3000000,
         overload_kill_qlen => 20000,
         overload_kill_restart_after => 5000,sync_mode_qlen => 10,
         type => standard_io},
   filter_default => stop,
   filters =>
       [{remote_gl,{fun logger_filters:remote_gl/2,stop}},
        {domain,{fun logger_filters:domain/2,
                 {log,super,[otp,sasl]}}},
        {no_domain,{fun logger_filters:domain/2,
                    {log,undefined,[]}}}],
   formatter =>
       {logger_formatter,#{legacy_header => false,max_size => 102400,
                           single_line => false,
                           template =>
                               [time," [",level,"] ",pid," ",
                                {logger_formatter,[[logger_formatter,title],":","\n"],[]},
                                msg,"\n"],
                           time_designator => 32}},
   id => default,level => all,module => logger_std_h}]
...
```

Feel the difference ;)