package minio

trait Platform {
  def fatal(t: Throwable): Boolean
  def shutdown(t: Throwable): Nothing
  def executeAsync(k: => Unit): Unit
  def executeBlocking[U](k: => U): U
}

object Platform {
  lazy val default =
    new Platform {
      import java.util.concurrent._
      import ForkJoinPool._

      private val pool = new ForkJoinPool

      def fatal(t: Throwable): Boolean = 
        ! scala.util.control.NonFatal(t)

      def shutdown(t: Throwable): Nothing = {
        pool.shutdownNow()
        throw t
      }

      def executeAsync(k: => Unit): Unit = 
        pool.execute( new Runnable { def run() = k } )

      def executeBlocking[U](k: => U): U = {
        var u: Option[U] = None
        val blocker =
          new ManagedBlocker {
            def isReleasable = u.isDefined
            def block(): Boolean = {
              u = Some(k)
              isReleasable
            }
          }
        managedBlock(blocker)
        u.get
      }
    }
}
