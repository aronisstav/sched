-module(sched_process).

-export([spawn/0]).

-type status() :: 'exiting' | 'waiting' | 'running'.

-record(process_info, {
          status = 'running' :: status(),
          message_queue = [] :: [signal('message')],
          self               :: pid()
         }).
