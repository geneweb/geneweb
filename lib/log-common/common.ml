let verbosity = ref 7
let debug = ref false

type level =
  [ `LOG_EMERG
  | `LOG_ALERT
  | `LOG_CRIT
  | `LOG_ERR
  | `LOG_WARNING
  | `LOG_NOTICE
  | `LOG_INFO
  | `LOG_DEBUG ]
