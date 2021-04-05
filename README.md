# Description

[![Build Status](https://travis-ci.com/bdt-group/log.svg?branch=main)](https://travis-ci.com/bdt-group/log)

A wrapper around Erlang system logger to simply configuration.

# Configuration

* `dir`: path to the directory where log files will be created (`all.log` and `error.log`).
  The default is current directory.
* `level`: log level. See Erlang's `logger:level()` type description. The default is `notice`.
* `rotate_size`: a size of a log file to trigger rotation. The default is `1024*1024*10` (10 mb).
* `rotate_count`: a maximum number of rotated files. The default is `5`.
* `single_line`: whether to flatten log message into single line or not. The default is `false`.
* `max_line_size`: truncate log message at this size. The default is `1024*100` (100 kb).
* `filesync_repeat_interval`, `sync_mode_qlen`, `drop_mode_qlen`, `flush_qlen`: Same as
  in `logger_std_h(erl)` module. The defaults are correspondingly: `no_repeat`, `1000`, `1000`
  and `5000`.
* `console`: Whether to log into console or not. The default is `false`.
* `formatter`: formatter type: either `plain` or `json`. The default is `plain`. JSON formatter
  formats log message into [JSON Lines](https://jsonlines.org). The log can be viewed using
  [jq](https://stedolan.github.io/jq) for example. Note that the option doesn't affect
  console formatter (i.e. the `default` logger handler) -- plain messages are always logged
  into console. In other words, JSON formatter is only used for log files.
* `exclude_meta`: only makes sense when `formatter` is set to `json`. In this case keys listed in
  this option are excluded from resulting `meta` field of the logged JSON message. The default is
  `[domain, report_cb, gl, error_logger, logger_formatter]`.

## Configuration example

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

# JSON format example

When logged as `?LOG_DEBUG("Hello, world", [], #{msisdn => 7923423444})` the corresponding JSON log
message will look as follows:
```json
{
  "timestamp": "2020-10-15T10:26:15.704095+03:00",
  "severity": "debug",
  "message": "Hello, world",
  "line": 1929,
  "file": "hello.erl",
  "meta": {
    "msisdn": "7923423444"
  },
  "erlang": {
    "pid": "<0.44.0>",
    "mfa": "hello:world/0"
  }
}
```
