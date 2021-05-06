package minio

import java.util.{Timer, TimerTask, Date}
import scala.concurrent.duration._

trait Timing { this: Signature =>

  object timer extends Timer(true)

  /**
   * This effect will wait for the given duration and then complete.
   */
  def delay( amount: Duration): UIO[Unit] = effectAsync (
    reply => 
      timer.schedule(
        new TimerTask {
          def run: Unit = reply(unit)
        },
        amount.toMillis
      )
  )
}
