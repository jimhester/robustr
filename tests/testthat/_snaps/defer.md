# defer()'s global env facilities work

    Code
      defer(print("howdy"), envir = globalenv())
    Message <simpleMessage>
      Setting deferred event(s) on global environment.
        * Will be run automatically when session ends
        * Execute (and clear) with `withr::deferred_run()`.
        * Clear (without executing) with `withr::deferred_clear()`.

