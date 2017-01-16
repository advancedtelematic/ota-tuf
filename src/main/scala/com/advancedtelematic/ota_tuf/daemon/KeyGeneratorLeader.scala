package com.advancedtelematic.ota_tuf.daemon

import akka.actor.Status.{Failure, Success}
import cats.syntax.show.toShowOps
import akka.actor.{Actor, ActorLogging, Props, Status, SupervisorStrategy}
import com.advancedtelematic.ota_tuf.daemon.KeyGeneratorLeader.Tick
import com.advancedtelematic.ota_tuf.data.DataType.{Key, KeyGenRequest}
import com.advancedtelematic.ota_tuf.data.{KeyGenRequestStatus, KeyType}
import com.advancedtelematic.ota_tuf.db.{KeyGenRequestSupport, KeyRepositorySupport}
import slick.driver.MySQLDriver.api._
import akka.pattern.pipe
import akka.routing.RoundRobinPool
import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair
import com.advancedtelematic.ota_tuf.data.DataType.KeyId._

import scala.async.Async._
import scala.concurrent.duration._

object KeyGeneratorLeader {
  case object Tick

  def props()(implicit db: Database): Props = Props(new KeyGeneratorLeader())
}

class KeyGeneratorLeader()(implicit db: Database) extends Actor with ActorLogging with KeyGenRequestSupport {

  import context.dispatcher

  override def preStart(): Unit = {
    self ! Tick
  }

  // override def postRestart(reason: Throwable): Unit = ()

  private val routerProps = RoundRobinPool(10)
    .withSupervisorStrategy(SupervisorStrategy.defaultStrategy)
    .props(KeyGeneratorWorker.props())

  private val router = context.system.actorOf(routerProps)

  def waiting(totalTasks: Int, remaining: Int): Receive =
    if(remaining == 0) {
      log.info("Finished generating {} keys", totalTasks)
      context.system.scheduler.scheduleOnce(3.seconds, self, Tick)
      receive
    } else {
      {
        case Status.Success(_) ⇒
          context.become(waiting(totalTasks, remaining - 1))
        case Status.Failure(ex) ⇒
          log.error(ex, "Could not generate key")
          context.become(waiting(totalTasks, remaining - 1))
      }
    }


  override def receive: Receive = {
    case Status.Failure(ex) ⇒
      throw ex

    case taskCount: Int ⇒
      log.info("Waiting for {} key generation tasks to complete", taskCount)
      context become waiting(taskCount, taskCount)

    case Tick =>
      log.info("Tick")

      // TODO: Be careful with starvation
      val f = keyGenRepo.findPending().map { m =>
        m.foreach { router ! _ }
        m.size
      }

      f.pipeTo(self)
  }
}

object KeyGeneratorWorker {
  def props()(implicit db: Database): Props = Props(new KeyGeneratorWorker())
}

class KeyGeneratorWorker()(implicit db: Database) extends Actor
  with ActorLogging
  with KeyGenRequestSupport
  with KeyRepositorySupport {

  import com.advancedtelematic.ota_tuf.crypt.RsaKeyPair._

  import context.dispatcher

  override def receive: Receive = {
    case kgr: KeyGenRequest =>
      log.info(s"Received key gen request for {}", kgr.id.show)

      val f = async {
        val keyPair = RsaKeyPair.generate(kgr.size)
        val key = Key(kgr.id, KeyType.RSA, keyPair.getPublic.show)
        val newKeyGnr = await(keyGenRepo.persistGenerated(kgr, key, keyRepo))

        log.info("Generated Key {}", key.id.show)
        Success(newKeyGnr)
      }

      f.recoverWith {
        case ex ⇒
          log.error("Key generation failed: {}", ex.getMessage)

          keyGenRepo
            .setStatus(kgr.id, KeyGenRequestStatus.ERROR)
            .map(_ => Failure(ex))
      }.pipeTo(sender)

    // TODO: store private key in vault
  }
}
