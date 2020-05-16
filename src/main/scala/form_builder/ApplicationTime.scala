package form_builder

import java.time.LocalDateTime

trait ApplicationTime {
  protected def now(): LocalDateTime
}

object ApplicationTimeImpl {
  protected def now(): LocalDateTime = {
    LocalDateTime.now()
  }
}
